(** {0 Surface language}

    The surface language closely mirrors what the programmer originally wrote,
    including syntactic sugar and higher level language features that make
    programming more convenient (in comparison to the {!Core.Syntax}).
*)


(** {1 Surface Syntax} *)

(** The start and end position in a source file *)
type span =
  Lexing.position * Lexing.position

(** Spanned nodes *)
type 'a spanned = {
  span : span;
  data : 'a;
}

type pattern = string option spanned

(** Terms in the surface language *)
type tm =
  tm_data spanned

and tm_data =
  | Let of pattern * params * tm option * tm * tm   (** Let expressions: [ let x : A := t; f x ] *)
  | Name of string                                  (** References to named things: [ x ] *)
  | Ann of tm * tm                                  (** Terms annotated with types: [ x : A ] *)
  | Fun_type of params * tm                         (** Function types: [ fun (x : A) -> B x ] *)
  | Fun_arrow of tm * tm                            (** Function arrow types: [ A -> B ] *)
  | Fun_lit of params * tm option * tm              (** Function literals: [ fun x := f x ] or [ fun (x : A) := f x ] *)
  | Fun_app of tm * tm list                         (** Function applications: [ f x ] *)

and param = pattern * tm option
and params = param list


(** Elaboration from the surface language into the core language

    This is where we implement user-facing type checking, while also translating
    the surface language into the simpler, more explicit core language.

    While we {e could} translate syntactic sugar in the parser, by leaving
    this to elaboration time we make it easier to report higher quality error
    messages that are more relevant to what the programmer originally wrote.
*)
module Elab : sig

  val check : tm -> Core.Semantics.vty -> (Core.Syntax.tm, span * string) result
  val infer : tm -> (Core.Syntax.tm * Core.Semantics.vty, span * string) result

end = struct

  module Syntax = Core.Syntax
  module Semantics = Core.Semantics


  (** {2 Elaboration context} *)

  (** The elaboration context. This records the bindings that are currently
      bound at the current scope in the program. *)
  module Ctx :  sig

    type t

    (** The empty context *)
    val empty : t

    (** Returns the next variable that will be bound in the context after
        calling {!add_def} or {!add_param} *)
    val next_var : t -> Semantics.vtm lazy_t

    (** Binds a definition in the context *)
    val add_def : t -> string option -> Semantics.vtm -> Semantics.vtm lazy_t -> t

    (** Binds a parameter in the context *)
    val add_param : t -> string option -> Semantics.vtm -> t

    (** Lookup a name in the context *)
    val lookup : t -> string -> (Core.index * Semantics.vtm) option

    (** Functions related to the core semantics *)

    val eval : t -> Syntax.tm -> Semantics.vtm
    val quote : t -> Semantics.vtm -> Syntax.tm
    val is_convertible : t -> Semantics.vtm -> Semantics.vtm -> bool
    val pp_vtm : ?resugar:bool -> t -> Semantics.vtm -> Format.formatter -> unit

  end = struct

    (** The environments are unzipped to make it more efficient to call
        functions from {!Core.Semantics}. *)
    type t = {
      size : Core.level;                      (** Number of entries bound. *)
      names : Core.name Core.env;             (** Name environment *)
      ty_env : Semantics.vty Core.env;        (** Type environment *)
      tm_env : Semantics.vtm Lazy.t Core.env; (** Term environment *)
    }

    let empty = {
      size = 0;
      names = [];
      ty_env = [];
      tm_env = [];
    }

    let next_var (ctx : t) : Semantics.vtm Lazy.t =
      Lazy.from_val (Semantics.Neu (Semantics.Var ctx.size))

    let add_def (ctx : t) (name : string option) (vty : Semantics.vty) (vtm : Semantics.vtm Lazy.t) = {
      size = ctx.size + 1;
      names = name :: ctx.names;
      ty_env = vty :: ctx.ty_env;
      tm_env = vtm :: ctx.tm_env;
    }

    let add_param (ctx : t) (name : string option) (vty : Semantics.vty) =
      add_def ctx name vty (next_var ctx)

    let lookup (ctx : t) (name : string) : (Core.index * Core.Semantics.vty) option =
      (* Find the index of most recent binding in the context identified by
          [name], starting from the most recent binding. This gives us the
          corresponding de Bruijn index of the variable. *)
      ctx.names |> List.find_mapi @@ fun index name' ->
        match Some name = name' with
        | true -> Some (index, List.nth ctx.ty_env index)
        | false -> None

    let eval ctx = Semantics.eval ctx.tm_env
    let quote ctx = Semantics.quote ctx.size
    let is_convertible ctx = Semantics.is_convertible ctx.size
    let pp_vtm ?(resugar = true) ctx vtm = Syntax.pp ctx.names (quote ctx vtm) ~resugar

  end


  (** {2 Exceptions} *)

  (** An exception used internally when encountering errors. These are expected
      to be caught later by the {!run_elab} function and should never escape
      this module.

      Real-world implementations should use error recovery so that elaboration
      can proceed after errors have been encountered. See [elab-error-recovery]
      for an example of how to implement this. *)
  exception Error of span * string

  (** Raises an {!Error} exception *)
  let error (type a) (span : span) (message : string) : a =
    raise (Error (span, message))

  let check_convertible (ctx : Ctx.t) (span : span) ~(found : Semantics.vty) ~(expected : Semantics.vty) =
    if Ctx.is_convertible ctx found expected then () else
      error span
        (Format.asprintf "@[<v 2>@[mismatched types:@]@ @[expected: %t@]@ @[   found: %t@]@]"
          (Ctx.pp_vtm ctx expected)
          (Ctx.pp_vtm ctx found))


  (** {2 Bidirectional type checking} *)

  (** The algorithm is structured {i bidirectionally}, divided into mutually
      recursive {i checking} and {i inference} modes. By supplying type
      annotations as early as possible using the checking mode, we can improve
      the locality of type errors, and provide enough {i control} to the
      algorithm to keep type inference decidable even in the presence of ‘fancy’
      types, for example dependent types, higher rank types, and subtyping. *)

  (** Elaborate a term in the surface language into a term in the core language
      in the presence of a type annotation. *)
  let rec check (ctx : Ctx.t) (tm : tm) (vty : Semantics.vty) : Syntax.tm =
    match tm.data with
    (* Let expressions *)
    | Let (name, params, def_ty, def, body) ->
        let def, def_vty = infer_def ctx params def_ty def in
        let body = check (Ctx.add_def ctx name.data def_vty (lazy (Ctx.eval ctx def))) body vty in
        Syntax.Let (name.data, def, body)

    (* Function literals *)
    | Fun_lit (params, body_ty, body) ->
        check_fun_lit ctx params body_ty body vty

    (* For anything else, try inferring the type of the term, then checking to
        see if the inferred type is the same as the expected type.

        Instead of using conversion checking, extensions to this type system
        could trigger unification or try to coerce the term to the expected
        type here. *)
    | _ ->
        let tm_span = tm.span in
        let tm, found_vty = infer ctx tm in
        check_convertible ctx tm_span ~found:found_vty ~expected:vty;
        tm

  (** Elaborate a term in the surface language into a term in the core language,
      inferring its type. *)
  and infer (ctx : Ctx.t) (tm : tm) : Syntax.tm * Semantics.vty =
    match tm.data with
    (* Let expressions *)
    | Let (name, params, def_ty, def, body) ->
        let def, def_vty = infer_def ctx params def_ty def in
        let body, body_vty = infer (Ctx.add_def ctx name.data def_vty (lazy (Ctx.eval ctx def))) body in
        Syntax.Let (name.data, def, body), body_vty

    (* Named terms *)
    | Name name ->
        begin match Ctx.lookup ctx name with
        | Some (index, vty) -> (Syntax.Var index, vty)
        (* We use [Type : Type] for simplicity, which means this type theory
           is inconsistent. This is fine for a toy type system, but we should
           use universe levels in an actual implementation. *)
        | None when name = "Type" -> Syntax.Univ, Semantics.Univ
        | None -> error tm.span (Format.asprintf "unbound name `%s`" name)
        end

    (* Annotated terms *)
    | Ann (tm, ty) ->
        let ty = check ctx ty Semantics.Univ in
        let vty = Ctx.eval ctx ty in
        Syntax.Ann (check ctx tm vty, ty), vty

    (* Function types *)
    | Fun_type (params, body_ty) ->
        let rec go ctx = function
          | [] -> check ctx body_ty Semantics.Univ
          (* Function types always require annotations *)
          | (name, None) :: _ ->
              error name.span "ambiguous function parameter type"
          | (name, Some param_ty) :: params ->
              let param_ty = check ctx param_ty Semantics.Univ in
              let body_ty = go (Ctx.add_param ctx name.data (Ctx.eval ctx param_ty)) params in
              Syntax.Fun_type (name.data, param_ty, body_ty)
        in
        go ctx params, Semantics.Univ

    (* Arrow types. These are implemented as syntactic sugar for non-dependent
        function types. *)
    | Fun_arrow (param_ty, body_ty) ->
        let param_ty = check ctx param_ty Semantics.Univ in
        let body_ty = check (Ctx.add_param ctx None (Ctx.eval ctx param_ty)) body_ty Semantics.Univ in
        Syntax.Fun_type (None, param_ty, body_ty), Semantics.Univ

    (* Function literals *)
    | Fun_lit (params, body_ty, body) ->
        infer_fun_lit ctx params body_ty body

    (* Function application *)
    | Fun_app (head, args) ->
        let rec go ctx (head, head_vty) args =
          match args with
          | [] -> head, head_vty
          | arg :: args ->
              match head_vty with
              | Semantics.Fun_type (_, param_vty, body_vty) ->
                  let arg = check ctx arg (Lazy.force param_vty) in
                  go ctx (Syntax.Fun_app (head, arg), body_vty (lazy (Ctx.eval ctx arg))) args
              | _ -> error arg.span "unexpected argument"
        in
        go ctx (infer ctx head) args

  (** Elaborate a function literal in checking mode. *)
  and check_fun_lit (ctx : Ctx.t) (params : params) (body_ty : tm option) (body : tm) (vty : Semantics.vty) =
    match params, body_ty, vty with
    (* Elaborate the body of the function literal *)
    | [], None, vty -> check ctx body vty
    | [], Some ({ span = body_ty_span; _ } as body_ty), vty ->
        let body_ty = check ctx body_ty Semantics.Univ in
        let body_vty = Ctx.eval ctx body_ty in
        check_convertible ctx body_ty_span ~found:body_vty ~expected:vty;
        check ctx body body_vty

    (* Elaborate a new parameter *)
    | (name, param_ty) :: params, body_ty, Semantics.Fun_type (_, param_vty', body_vty') ->
        let var = Ctx.next_var ctx in
        let param_ty =
          match param_ty with
          | None -> Lazy.force param_vty'
          | Some param_ty ->
              let param_ty = check ctx param_ty Semantics.Univ in
              let param_vty = Ctx.eval ctx param_ty in
              check_convertible ctx name.span ~found:param_vty ~expected:(Lazy.force param_vty');
              param_vty
        in
        let ctx = Ctx.add_def ctx name.data param_ty var in
        let body = check_fun_lit ctx params body_ty body (body_vty' var) in
        Syntax.Fun_lit (name.data, body)

    | (name, _) :: _, _, _ ->
        error name.span "too many parameters in function literal"

  (** Elaborate a function literal in inference mode. *)
  and infer_fun_lit (ctx : Ctx.t) (params : params) (body_ty : tm option) (body : tm) =
    let rec go ctx params body_ty body =
      match params, body_ty with
      (* Elaborate the body of the function literal *)
      | [], None ->
          let body, body_ty = infer ctx body in
          body, Ctx.quote ctx body_ty
      | [], Some body_ty ->
          let body_ty = check ctx body_ty Semantics.Univ in
          check ctx body (Ctx.eval ctx body_ty), body_ty

      (* Elaborate a new parameter *)
      | (name, param_ty) :: params, body_ty ->
          let var = Ctx.next_var ctx in
          let param_ty =
            match param_ty with
            (* We’re in inference mode, so function parameters need annotations *)
            | None -> error name.span "ambiguous function parameter type"
            | Some param_ty -> check ctx param_ty Semantics.Univ
          in
          let ctx = Ctx.add_def ctx name.data (Ctx.eval ctx param_ty) var in
          let body, body_ty = go ctx params body_ty body in
          Syntax.Fun_lit (name.data, body), Syntax.Fun_type (name.data, param_ty, body_ty)
    in
    let fun_tm, fun_ty = go ctx params body_ty body in
    Syntax.Ann (fun_tm, fun_ty), Ctx.eval ctx fun_ty

  (** Elaborate a (potentially) parameterised and annotated definition. *)
  and infer_def ctx params def_ty def =
    (* Only annotate definitions when necessary *)
    match params, def_ty with
    | [], None -> infer ctx def
    | params, def_ty -> infer_fun_lit ctx params def_ty def


  (** {2 Running elaboration} *)

  let run_elab (type a) (prog : unit -> a) : (a, span * string) result =
    match prog () with
    | result -> Ok result
    | exception Error (span, message) -> Error (span, message)


  (** {2 Public API} *)

  let check (tm : tm) (vty : Semantics.vty) : (Core.Syntax.tm, span * string) result =
    run_elab (fun () -> check Ctx.empty tm vty)

  let infer (tm : tm) : (Core.Syntax.tm * Core.Semantics.vty, span * string) result =
    run_elab (fun () -> infer Ctx.empty tm)

end
