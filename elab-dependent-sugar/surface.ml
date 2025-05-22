(** {0 Surface language}

    The surface language closely mirrors what the programmer originally wrote,
    including syntactic sugar and higher level language features that make
    programming more convenient (in comparison to the {!Core.Syntax}).
*)


(** {1 Surface Syntax} *)

type pattern = string option

(** Terms in the surface language *)
type tm =
  | Let of pattern * params * tm option * tm * tm   (** Let expressions: [ let x : A := t; f x ] *)
  | Name of string                                  (** References to named things: [ x ] *)
  | Ann of tm * tm                                  (** Terms annotated with types: [ x : A ] *)
  | Univ                                            (** Universe of types: [ Type ] *)
  | Fun_type of params * tm                         (** Function types: [ fun (x : A) -> B x ] *)
  | Fun_arrow of tm * tm                            (** Function arrow types: [ A -> B ] *)
  | Fun_lit of params * tm option * tm              (** Function literals: [ fun x := f x ] or [ fun (x : A) := f x ] *)
  | Fun_app of tm * tm list                         (** Function applications: [ f x ] *)

and param = pattern * tm option
and params = param list


(** {1 Elaboration } *)

(** This is where we implement user-facing type checking, while also translating
    the surface language into the simpler, more explicit core language.

    While we {e could} translate syntactic sugar in the parser, by leaving
    this to elaboration time we make it easier to report higher quality error
    messages that are more relevant to what the programmer originally wrote.
*)

module Syntax = Core.Syntax
module Semantics = Core.Semantics


(** {2 Elaboration state} *)

(** The elaboration context records the bindings that are currently bound at
    the current scope in the program. The environments are unzipped to make it
    more efficient to call functions from {!Core.Semantics}. *)
type context = {
  size : Core.level;              (** Number of entries bound. *)
  names : Core.name Core.env;     (** Name environment *)
  tys : Semantics.vty Core.env;   (** Type environment *)
  tms : Semantics.vtm Core.env;   (** Term environment *)
}

(** The initial elaboration context, without any bindings *)
let initial_context = {
  size = 0;
  names = [];
  tys = [];
  tms = [];
}

(** Returns the next variable that will be bound in the context after calling
    {!bind_def} or {!bind_param} *)
let next_var (ctx : context) : Semantics.vtm =
  Semantics.Neu (Semantics.Var ctx.size)

(** Binds a definition in the context *)
let bind_def (ctx : context) (name : string option) (ty : Semantics.vty) (tm : Semantics.vtm) = {
  size = ctx.size + 1;
  names = name :: ctx.names;
  tys = ty :: ctx.tys;
  tms = tm :: ctx.tms;
}

(** Binds a parameter in the context *)
let bind_param (ctx : context) (name : string option) (ty : Semantics.vty) =
  bind_def ctx name ty (next_var ctx)

(** Lookup a name in the context *)
let lookup (ctx : context) (name : string) : (Core.index * Semantics.vty) option =
  (* Find the index of most recent binding in the context identified by
      [name], starting from the most recent binding. This gives us the
      corresponding de Bruijn index of the variable. *)
  ctx.names |> List.find_mapi @@ fun index name' ->
    match Some name = name' with
    | true -> Some (index, List.nth ctx.tys index)
    | false -> None

(** {3 Functions related to the core semantics} *)

(** These wrapper functions make it easier to call functions from the
    {!Core.Semantics} using state from the elaboration context. *)

let eval ctx : Syntax.tm -> Semantics.vtm =
  Semantics.eval ctx.tms
let quote ctx : Semantics.vtm -> Syntax.tm =
  Semantics.quote ctx.size
let normalise ctx : Syntax.tm -> Syntax.tm =
  Semantics.normalise ctx.size ctx.tms
let is_convertible ctx : Semantics.vtm -> Semantics.vtm -> bool =
  Semantics.is_convertible ctx.size
let pp ?(resugar = true) ctx =
  Syntax.pp ctx.names ~resugar


(** {2 Exceptions} *)

(** An error that will be raised if there was a problem in the surface syntax,
    usually as a result of type errors. This is normal, and should be rendered
    nicely to the programmer. *)
exception Error of string

(** Raises an {!Error} exception *)
let error message =
  raise (Error message)

let type_mismatch ctx ~expected ~found =
  Format.asprintf "@[<v 2>@[type mismatch@]@ @[expected: %a@]@ @[found:    %a@]@]"
    (pp ctx) expected
    (pp ctx) found

let not_bound name =
  Format.asprintf "`%s` is not bound in the current scope" name

let ambiguous_param name =
    Format.asprintf "ambiguous function parameter `%s`"
      (Option.value ~default:"_" name)


(** {2 Bidirectional type checking} *)

(** The algorithm is structured {i bidirectionally}, divided into mutually
    recursive {i checking} and {i inference} modes. By supplying type
    annotations as early as possible using the checking mode, we can improve
    the locality of type errors, and provide enough {i control} to the
    algorithm to keep type inference decidable even in the presence of ‘fancy’
    types, for example dependent types, higher rank types, and subtyping. *)

(** Elaborate a term in the surface language into a term in the core language
    in the presence of a type annotation. *)
let rec check (ctx : context) (tm : tm) (expected_ty : Semantics.vty) : Syntax.tm =
  match tm, expected_ty with
  (* Let expressions *)
  | Let (name, params, def_ty, def, body), expected_ty ->
      let def, def_ty = infer_def ctx params def_ty def in
      let ctx = bind_def ctx name def_ty (eval ctx def) in
      Syntax.Let (name, def, check ctx body expected_ty)

  (* Function literals *)
  | Fun_lit (params, body_ty, body), expected_ty ->
      check_fun_lit ctx params body_ty body expected_ty

  (* For anything else, try inferring the type of the term, then checking to
      see if the inferred type is the same as the expected type.

      Instead of using conversion checking, extensions to this type system
      could trigger unification or try to coerce the term to the expected
      type here. *)
  | tm, expected_ty ->
      let tm, ty' = infer ctx tm in
      if is_convertible ctx ty' expected_ty then tm else
        error (type_mismatch ctx
          ~expected:(quote ctx expected_ty)
          ~found:(quote ctx ty'))

(** Elaborate a term in the surface language into a term in the core language,
    inferring its type. *)
and infer (ctx : context) (tm : tm) : Syntax.tm * Semantics.vty =
  match tm with
  (* Let expressions *)
  | Let (name, params, def_ty, def, body) ->
      let def, def_ty = infer_def ctx params def_ty def in
      let ctx = bind_def ctx name def_ty (eval ctx def) in
      let body, body_ty = infer ctx body in
      Syntax.Let (name, def, body), body_ty

  (* Named terms *)
  | Name name ->
      begin match lookup ctx name with
      | Some (index, vty) -> (Syntax.Var index, vty)
      | None -> error (not_bound name)
      end

  (* Annotated terms *)
  | Ann (tm, ty) ->
      let ty = check ctx ty Semantics.Univ in
      let ty' = eval ctx ty in
      let tm = check ctx tm ty' in
      Syntax.Ann (tm, ty), ty'

  (* Universes *)
  | Univ ->
      (* We use [Type : Type] here for simplicity, which means this type
          theory is inconsistent. This is okay for a toy type system, but we’d
          want look into using universe levels in an actual implementation. *)
      Syntax.Univ, Semantics.Univ

  (* Function types *)
  | Fun_type (params, body_ty) ->
      let rec go ctx = function
        | [] -> check ctx body_ty Semantics.Univ
        (* Function types always require annotations *)
        | (name, None) :: _ -> error (ambiguous_param name)
        | (name, Some param_ty) :: params ->
            let param_ty = check ctx param_ty Semantics.Univ in
            let ctx = bind_param ctx name (eval ctx param_ty) in
            Syntax.Fun_type (name, param_ty, go ctx params)
      in
      go ctx params, Semantics.Univ

  (* Arrow types. These are implemented as syntactic sugar for non-dependent
      function types. *)
  | Fun_arrow (param_ty, body_ty) ->
      let param_ty = check ctx param_ty Semantics.Univ in
      let ctx = bind_param ctx None (eval ctx param_ty) in
      let body_ty = check ctx body_ty Semantics.Univ in
      Syntax.Fun_type (None, param_ty, body_ty), Semantics.Univ

  (* Function literals *)
  | Fun_lit (params, body_ty, body) ->
      infer_fun_lit ctx params body_ty body

  (* Function application *)
  | Fun_app (head, args) ->
      List.fold_left
        (fun (head, head_ty) arg ->
          match head_ty with
          | Semantics.Fun_type (_, param_ty, body_ty) ->
              let arg = check ctx arg (Lazy.force param_ty) in
              (Syntax.Fun_app (head, arg), body_ty (eval ctx arg))
          | _ -> error "not a function")
        (infer ctx head)
        args

(** Elaborate a function literal in checking mode. *)
and check_fun_lit (ctx : context) (params : params) (body_ty : tm option) (body : tm) (expected_ty : Semantics.vty) =
  match params, body_ty, expected_ty with
  | [], None, expected_ty -> check ctx body expected_ty
  | [], Some body_ty, expected_ty ->
      let body_ty = check ctx body_ty Semantics.Univ in
      let body_ty' = eval ctx body_ty in
      if is_convertible ctx body_ty' expected_ty then
        check ctx body body_ty'
      else error (type_mismatch ctx
        ~expected:(quote ctx expected_ty)
        ~found:body_ty)
  | (name, param_ty) :: params, body_ty, Semantics.Fun_type (_, expected_param_ty, expected_body_ty) ->
      let var = next_var ctx in
      let param_ty =
        match param_ty with
        | None -> Lazy.force expected_param_ty
        | Some param_ty ->
            let param_ty = check ctx param_ty Semantics.Univ in
            let param_ty' = eval ctx param_ty in
            let expected_param_ty = Lazy.force expected_param_ty in
            (* Check that the parameter annotation in the function literal
                matches the expected parameter type. *)
            if is_convertible ctx param_ty' expected_param_ty then param_ty' else
              error (type_mismatch ctx
                ~expected:(quote ctx expected_param_ty)
                ~found:param_ty)
      in
      let ctx = bind_def ctx name param_ty var in
      let body = check_fun_lit ctx params body_ty body (expected_body_ty var) in
      Syntax.Fun_lit (name, body)
  | _, _, _ ->
      error "too many parameters in function literal"

(** Elaborate a function literal in inference mode. *)
and infer_fun_lit (ctx : context) (params : params) (body_ty : tm option) (body : tm) =
  let rec go ctx params body_ty body =
    match params, body_ty with
    | [], None ->
        let body, body_ty = infer ctx body in
        body, quote ctx body_ty
    | [], Some body_ty ->
        let body_ty = check ctx body_ty Semantics.Univ in
        check ctx body (eval ctx body_ty), body_ty
    | (name, param_ty) :: params, body_ty ->
        let var = next_var ctx in
        let param_ty =
          match param_ty with
          (* We’re in inference mode, so function parameters need annotations *)
          | None -> error (ambiguous_param name)
          | Some param_ty -> check ctx param_ty Semantics.Univ
        in
        let ctx = bind_def ctx name (eval ctx param_ty) var in
        let body, body_ty = go ctx params body_ty body in
        Syntax.Fun_lit (name, body), Syntax.Fun_type (name, param_ty, body_ty)
  in
  let fun_tm, fun_ty = go ctx params body_ty body in
  Syntax.Ann (fun_tm, fun_ty), eval ctx fun_ty

(** Elaborate a (potentially) parameterised and annotated definition. *)
and infer_def ctx params def_ty def =
  (* Only annotate definitions when necessary *)
  match params, def_ty with
  | [], None -> infer ctx def
  | params, def_ty -> infer_fun_lit ctx params def_ty def
