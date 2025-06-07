(** {0 Surface language}

    The surface language closely mirrors what the programmer originally wrote,
    including syntactic sugar and higher level language features that make
    programming more convenient (in comparison to the {!Core.Syntax}).
*)


(** {1 Surface Syntax} *)

(** The start and end position in a source file *)
type loc =
  Lexing.position * Lexing.position

(** Located nodes *)
type 'a located = {
  loc : loc;
  data : 'a;
}

type pattern = string option located

(** Terms in the surface language *)
type tm =
  tm_data located

and tm_data =
  | Let of pattern * tm * tm * tm         (** Let expressions: [ let x : A := t; f x ] *)
  | Name of string                        (** References to named things: [ x ] *)
  | Ann of tm * tm                        (** Terms annotated with types: [ x : A ] *)
  | Univ                                  (** Universe of types: [ Type ] *)
  | Fun_type of (pattern * tm) list * tm  (** Function types: [ fun (x : A) -> B x ] *)
  | Fun_arrow of tm * tm                  (** Function arrow types: [ A -> B ] *)
  | Fun_lit of pattern list * tm          (** Function literals: [ fun x => f x ] *)
  | Fun_app of tm * tm list               (** Function applications: [ f x ] *)


(** Elaboration from the surface language into the core language

    This is where we implement user-facing type checking, while also translating
    the surface language into the simpler, more explicit core language.

    While we {e could} translate syntactic sugar in the parser, by leaving
    this to elaboration time we make it easier to report higher quality error
    messages that are more relevant to what the programmer originally wrote.
*)
module Elab = struct

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
  let lookup (ctx : context) (name : string) : (Core.index * Core.Semantics.vty) option =
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
  exception Error of loc * string

  (** Raises an {!Error} exception *)
    let error (type a) (loc : loc) (message : string) : a =
      raise (Error (loc, message))

  let type_mismatch (ctx : context) ~expected ~found : string =
    Format.asprintf "@[<v 2>@[type mismatch@]@ @[expected: %t@]@ @[found:    %t@]@]"
      (pp ctx expected)
      (pp ctx found)

  let not_bound (name : string) : string =
    Format.asprintf "`%s` is not bound in the current scope" name


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
    match tm.data, expected_ty with
    (* Let expressions *)
    | Let (name, def_ty, def, body), expected_ty ->
        let def_ty = check ctx def_ty Semantics.Univ in
        let def_ty' = eval ctx def_ty in
        let def = check ctx def def_ty' in
        let ctx = bind_def ctx name.data def_ty' (eval ctx def) in
        Syntax.Let (name.data, def, check ctx body expected_ty)

    (* Function literals *)
    | Fun_lit (names, body), expected_ty ->
        let rec go ctx names body_ty =
          match names with
          | [] -> check ctx body body_ty
          | name :: names ->
              begin match body_ty with
              | Semantics.Fun_type (_, param_ty, body_ty) ->
                  let var = next_var ctx in
                  let ctx = bind_def ctx name.data (Lazy.force param_ty) var in
                  Syntax.Fun_lit (name.data, go ctx names (body_ty var))
              | _ -> error tm.loc "too many parameters in function literal"
              end
        in
        go ctx names expected_ty

    (* For anything else, try inferring the type of the term, then checking to
        see if the inferred type is the same as the expected type.

        Instead of using conversion checking, extensions to this type system
        could trigger unification or try to coerce the term to the expected
        type here. *)
    | _, expected_ty ->
        let tm_loc = tm.loc in
        let tm, ty' = infer ctx tm in
        if is_convertible ctx ty' expected_ty then tm else
          error tm_loc (type_mismatch ctx
            ~expected:(quote ctx expected_ty)
            ~found:(quote ctx ty'))

  (** Elaborate a term in the surface language into a term in the core language,
      inferring its type. *)
  and infer (ctx : context) (tm : tm) : Syntax.tm * Semantics.vty =
    match tm.data with
    (* Let expressions *)
    | Let (name, def_ty, def, body) ->
        let def_ty = check ctx def_ty Semantics.Univ in
        let def_ty' = eval ctx def_ty in
        let def = check ctx def def_ty' in
        let ctx = bind_def ctx name.data def_ty' (eval ctx def) in
        let body, body_ty = infer ctx body in
        Syntax.Let (name.data, def, body), body_ty

    (* Named terms *)
    | Name name ->
        begin match lookup ctx name with
        | Some (index, vty) -> (Syntax.Var index, vty)
        | None -> error tm.loc (not_bound name)
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
        let rec go ctx params =
          match params with
          | [] -> check ctx body_ty Semantics.Univ
          | (name, param_ty) :: params ->
              let param_ty = check ctx param_ty Semantics.Univ in
              let ctx = bind_param ctx name.data (eval ctx param_ty) in
              Syntax.Fun_type (name.data, param_ty, go ctx params)
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
    | Fun_lit (_, _) ->
        error tm.loc "ambiguous function literal"

    (* Function application *)
    | Fun_app (head, args) ->
        List.fold_left
          (fun (head, head_ty) arg ->
            match head_ty with
            | Semantics.Fun_type (_, param_ty, body_ty) ->
                let arg = check ctx arg (Lazy.force param_ty) in
                Syntax.Fun_app (head, arg), body_ty (eval ctx arg)
            | _ -> error arg.loc "unexpected argument")
          (infer ctx head)
          args

end
