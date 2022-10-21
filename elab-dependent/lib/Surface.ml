(** {0 Surface language} *)

(** The surface language closely mirrors what the programmer originaly wrote,
    including syntactic sugar and higher level language features that make
    programming more convenient (in comparison to the {!Core.Syntax}). *)


(** Returns the index of the given element in the list *)
let elem_index a =
  let rec go i = function
    | [] -> None
    | x :: xs -> if x = a then Some i else go (i + 1) xs in
  go 0


(** {1 Surface Syntax} *)

type pattern = string option

(** Terms in the surface language *)
type tm =
  | Let of pattern * params * tm option * tm * tm   (** Let expressions: [ let x : A := t; f x ] *)
  | Name of string                                  (** References to named things: [ x ] *)
  | Ann of tm * tm                                  (** Terms annotated with types: [ x : A ] *)
  | Univ                                            (** Universe of types: [ Type ] *)
  | FunType of params * tm                          (** Function types: [ fun (x : A) -> B x ] *)
  | FunArrow of tm * tm                             (** Function arrow types: [ A -> B ] *)
  | FunLit of params * tm option * tm               (** Function literals: [ fun x := f x ] or [ fun (x : A) := f x ] *)
  | FunApp of tm * tm list                          (** Function applications: [ f x ] *)

and param = pattern * tm option
and params = param list


(** {1 Elaboration } *)

(** This is where we implement user-facing type checking, in addition to
    translating the convenient surface language into a simpler, more explicit
    core language.

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
  tys : Semantics.ty Core.env;    (** Type environment *)
  tms : Semantics.tm Core.env;    (** Term environment *)
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
let next_var context =
  Semantics.Neu (Semantics.Var context.size)

(** Binds a definition in the context *)
let bind_def context name ty tm = {
  size = context.size + 1;
  names = name :: context.names;
  tys = ty :: context.tys;
  tms = tm :: context.tms;
}

(** Binds a parameter in the context *)
let bind_param context name ty =
  bind_def context name ty (next_var context)

(** {3 Functions related to the core semantics} *)

(** These wrapper functions make it easier to call functions from the
    {!Core.Semantics} using state from the elaboration context. *)

let eval context : Syntax.tm -> Semantics.tm =
  Semantics.eval context.tms
let quote context : Semantics.tm -> Syntax.tm =
  Semantics.quote context.size
let normalise context : Syntax.tm -> Syntax.tm =
  Semantics.normalise context.size context.tms
let is_convertible context : Semantics.tm * Semantics.tm -> bool =
  Semantics.is_convertible context.size
let pp ?(wrap = false) ?(resugar = true) context =
  Syntax.pp context.names ~wrap ~resugar


(** {2 Exceptions} *)

(** An error that will be raised if there was a problem in the surface syntax,
    usually as a result of type errors or a missing type annotations. This is
    normal, and should be rendered nicely to the programmer. *)
exception Error of string

(** Raises an {!Error} exception *)
let error message =
  raise (Error message)

let type_mismatch context ~expected ~found =
  Format.asprintf "@[<v 2>@[type mismatch@]@ @[expected: %a@]@ @[found:    %a@]@]"
    (pp context) expected
    (pp context) found

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
    algorithm to keep type inference deciable even in the presence of ‘fancy’
    types, for example dependent types, higher rank types, and subtyping. *)

(** Elaborate a term in the surface language into a term in the core language
    in the presence of a type annotation. *)
let rec check context tm (ty : Semantics.ty) : Syntax.tm =
  match tm, ty with
  (* Let expressions *)
  | Let (name, params, def_ty, def, body), ty ->
      let def, def_ty = infer_fun_lit context params def_ty def in
      let context = bind_def context name def_ty (eval context def) in
      Syntax.Let (name, def, check context body ty)

  (* Function literals *)
  | FunLit (params, body_ty, body), ty ->
      (* Iterate over the parameters, constructing a function literal in the
          core language. *)
      let rec go context params ty =
        match params, ty with
        | [], expected_body_ty ->
            begin match body_ty with
            | None -> check context body expected_body_ty
            | Some body_ty ->
                let body_ty = check context body_ty Semantics.Univ in
                let body_ty' = eval context body_ty in
                if is_convertible context (body_ty', expected_body_ty) then
                  check context body body_ty'
                else error (type_mismatch context
                  ~expected:(quote context expected_body_ty)
                  ~found:body_ty)
            end
        | (name, None) :: params, Semantics.FunType (_, param_ty, body_ty) ->
            let var = next_var context in
            let context = bind_def context name (Lazy.force param_ty) var in
            Syntax.FunLit (name, go context params (body_ty var))
        | (name, Some param_ty) :: params, Semantics.FunType (_, expected_param_ty, body_ty) ->
            let var = next_var context in
            let param_ty = check context param_ty Semantics.Univ in
            let param_ty' = eval context param_ty in
            let expected_param_ty = Lazy.force expected_param_ty in
            (* Check that the parameter annotation in the function literal
                matches the expected parameter type. *)
            if is_convertible context (param_ty', expected_param_ty) then
              let context = bind_def context name expected_param_ty var in
              Syntax.FunLit (name, go context params (body_ty var))
            else
              error (type_mismatch context
                ~expected:(quote context expected_param_ty)
                ~found:param_ty)
        | _, _ -> error "too many parameters in function literal"
      in
      go context params ty

  (* For anything else, try inferring the type of the term, then checking to
      see if the inferred type is the same as the expected type.

      Instead of using conversion checking, extensions to this type system
      could trigger unification or try to coerce the term to the expected
      type here. *)
  | tm, ty ->
      let tm, ty' = infer context tm in
      if is_convertible context (ty', ty) then tm else
        error (type_mismatch context
          ~expected:(quote context ty)
          ~found:(quote context ty'))

(** Elaborate a term in the surface language into a term in the core language,
    inferring its type. *)
and infer context : tm -> Syntax.tm * Semantics.ty = function
  (* Let expressions *)
  | Let (name, params, def_ty, def, body) ->
      let def, def_ty = infer_fun_lit context params def_ty def in
      let context = bind_def context name def_ty (eval context def) in
      let body, body_ty = infer context body in
      (Syntax.Let (name, def, body), body_ty)

  (* Named terms *)
  | Name name ->
      (* Find the index of most recent binding in the context identified by
          [name], starting from the most recent binding. This gives us the
          corresponding de Bruijn index of the variable. *)
      begin match elem_index (Some name) context.names with
      | Some (index) -> (Syntax.Var index, List.nth context.tys index)
      | None -> error (not_bound name)
      end

  (* Annotated terms *)
  | Ann (tm, ty) ->
      let ty = check context ty Semantics.Univ in
      let ty' = eval context ty in
      let tm = check context tm ty' in
      (Syntax.Ann (tm, ty), ty')

  (* Universes *)
  | Univ ->
      (* We use [Type : Type] here for simplicity, which means this type
          theory is inconsistent. This is okay for a toy type system, but we’d
          want look into using universe levels in an actual implementation. *)
      (Syntax.Univ, Semantics.Univ)

  (* Function types *)
  | FunType (params, body_ty) ->
      let rec go context = function
        | [] -> check context body_ty Semantics.Univ
        (* Function types always require annotations *)
        | (name, None) :: _ -> error (ambiguous_param name)
        | (name, Some param_ty) :: params ->
            let param_ty = check context param_ty Semantics.Univ in
            let context = bind_param context name (eval context param_ty) in
            Syntax.FunType (name, param_ty, go context params)
      in
      (go context params, Semantics.Univ)

  (* Arrow types. These are implemented as syntactic sugar for non-dependent
      function types. *)
  | FunArrow (param_ty, body_ty) ->
      let param_ty = check context param_ty Semantics.Univ in
      let context = bind_param context None (eval context param_ty) in
      let body_ty = check context body_ty Semantics.Univ in
      (Syntax.FunType (None, param_ty, body_ty), Semantics.Univ)

  (* Function literals *)
  | FunLit (params, body_ty, body) ->
      infer_fun_lit context params body_ty body

  (* Function application *)
  | FunApp (head, args) ->
      List.fold_left
        (fun (head, head_ty) arg ->
          match head_ty with
          | Semantics.FunType (_, param_ty, body_ty) ->
              let arg = check context arg (Lazy.force param_ty) in
              (Syntax.FunApp (head, arg), body_ty (eval context arg))
          | _ -> error "not a function")
        (infer context head)
        args

and infer_params context params infer_body =
  match params with
  | [] -> infer_body context
  (* We’re in inference mode, so function parameters need annotations *)
  | (name, None) :: _ -> error (ambiguous_param name)
  | (name, Some param_ty) :: params ->
      let var = next_var context in
      let param_ty = check context param_ty Semantics.Univ in
      let param_ty' = eval context param_ty in
      let context = bind_def context name param_ty' var in
      let body, body_ty = infer_params context params infer_body in
      (Syntax.FunLit (name, body), Syntax.FunType (name, param_ty, body_ty))

and infer_fun_lit context params body_ty body =
  match params, body_ty with
  | [], None -> infer context body
  | [], Some body_ty ->
      let body_ty = check context body_ty Semantics.Univ in
      let body_ty' = eval context body_ty in
      let body = check context body body_ty' in
      (Syntax.Ann (body, body_ty), body_ty')
  | params, None ->
      let fun_tm, fun_ty =
        infer_params context params (fun context ->
          let body, body_ty = infer context body in
          body, quote context body_ty)
      in
      (Syntax.Ann (fun_tm, fun_ty), eval context fun_ty)
  | params, Some body_ty ->
      let fun_tm, fun_ty =
        infer_params context params (fun context ->
          let body_ty = check context body_ty Semantics.Univ in
          let body_ty' = eval context body_ty in
          check context body body_ty', body_ty)
      in
      (Syntax.Ann (fun_tm, fun_ty), eval context fun_ty)
