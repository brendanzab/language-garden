(** {0 Surface language}

    The surface language closely mirrors what the programmer originally wrote,
    including syntactic sugar and higher level language features that make
    programming more convenient (in comparison to the {!Core}).
*)

(** {1 Syntax} *)

(** The start and end position in a source file *)
type span =
  Lexing.position * Lexing.position

(** Spanned nodes *)
type 'a spanned = {
  span : span;
  data : 'a;
}

(** Types in the surface language *)
type ty =
  ty_data spanned

and ty_data =
  | Name of string
  | Fun_type of ty * ty

(** Names that bind definitions or parameters *)
type binder = string spanned

(** Terms in the surface language *)
type tm =
  tm_data spanned

and tm_data =
  | Name of string
  | Let of binder * param list * ty option * tm * tm
  | Ann of tm * ty
  | Fun_lit of param list * tm
  | Int_lit of int
  | App of tm * tm
  | If_then_else of tm * tm * tm
  | Infix of [`Eq | `Add | `Sub | `Mul] * tm * tm
  | Prefix of [`Neg] * tm

(** Parameters, with optional type annotations *)
and param =
  binder * ty option


(** Elaboration from the surface language into the core language

    This is where we implement user-facing type checking, while also translating
    the surface language into the simpler, more explicit core language.

    While we {e could} translate syntactic sugar in the parser, by leaving
    this to elaboration time we make it easier to report higher quality error
    messages that are more relevant to what the programmer originally wrote.
*)
module Elab : sig

  val check_ty : ty -> (Core.ty, span * string) result
  val check_tm : tm -> Core.ty -> (Core.tm, span * string) result
  val infer_tm : tm -> (Core.tm * Core.ty, span * string) result

end = struct

  (** {2 Elaboration context} *)

  (** A stack of bindings currently in scope *)
  type context = (string * Core.ty) Core.env

  (** Lookup a name in the context *)
  let lookup (ctx : context) (name : string) : (Core.index * Core.ty) option =
    ctx |> List.find_mapi @@ fun index (name', ty) ->
      match name = name' with
      | true -> Some (index, ty)
      | false -> None


  (** {2 Elaboration errors} *)

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

  let equate_ty (span : span) ~(found : Core.ty) ~(expected : Core.ty) =
    if found = expected then () else
      error span
        (Format.asprintf "@[<v 2>@[mismatched types:@]@ @[expected: %t@]@ @[found: %t@]@]"
          (Core.pp_ty expected)
          (Core.pp_ty found))


  (** {2 Bidirectional type checking} *)

  (** The algorithm is structured {i bidirectionally}, divided into mutually
      recursive {i checking} and {i inference} modes. By supplying type
      annotations as early as possible using the checking mode, we can improve
      the locality of type errors. We can also extend the type system with
      advanced features like dependent types, higher rank types, and subtyping
      while maintaining decidability by allowing the programmer to supply
      annotations where necessary. *)

  (** Elaborate a type, checking that it is well-formed. *)
  let rec check_ty (ty : ty) : Core.ty =
    match ty.data with
    | Name "Bool" -> Bool_type
    | Name "Int" -> Int_type
    | Name name ->
        error ty.span (Format.asprintf "unbound type `%s`" name)
    | Fun_type (ty1, ty2) ->
        Fun_type (check_ty ty1, check_ty ty2)

  (** Elaborate a surface term into a core term, given an expected type. *)
  let rec check_tm (ctx : context) (tm : tm) (ty : Core.ty) : Core.tm =
    match tm.data with
    | Let (def_name, params, def_body_ty, def_body, body) ->
        let def, def_ty = infer_fun_lit ctx params def_body_ty def_body in
        let body = check_tm ((def_name.data, def_ty) :: ctx) body ty in
        Let (def_name.data, def_ty, def, body)

    | Fun_lit (params, body) ->
        check_fun_lit ctx params body ty

    | If_then_else (head, tm1, tm2) ->
        let head = check_tm ctx head Bool_type in
        let tm1 = check_tm ctx tm1 ty in
        let tm2 = check_tm ctx tm2 ty in
        Bool_elim (head, tm1, tm2)

    (* Fall back to type inference *)
    | _ ->
        let tm', ty' = infer_tm ctx tm in
        equate_ty tm.span ~found:ty' ~expected:ty;
        tm'

  (** Elaborate a surface term into a core term, inferring its type. *)
  and infer_tm (ctx : context) (tm : tm) : Core.tm * Core.ty =
    match tm.data with
    | Name name ->
        begin match lookup ctx name with
        | Some (index, ty) -> Var index, ty
        | None when name = "true" -> Bool_lit true, Bool_type
        | None when name = "false" -> Bool_lit false, Bool_type
        | None -> error tm.span (Format.asprintf "unbound name `%s`" name)
        end

    | Let (def_name, params, def_body_ty, def_body, body) ->
        let def, def_ty = infer_fun_lit ctx params def_body_ty def_body in
        let body, body_ty = infer_tm ((def_name.data, def_ty) :: ctx) body in
        Let (def_name.data, def_ty, def, body), body_ty

    | Ann (tm, ty) ->
        let ty = check_ty ty in
        check_tm ctx tm ty, ty

    | Int_lit i ->
        Int_lit i, Int_type

    | Fun_lit (params, body) ->
        infer_fun_lit ctx params None body

    | App (head, arg) ->
        let head, head_ty = infer_tm ctx head in
        let param_ty, body_ty =
          match head_ty with
          | Fun_type (param_ty, body_ty) -> param_ty, body_ty
          | _ -> error arg.span "unexpected argument"
        in
        let arg = check_tm ctx arg param_ty in
        Fun_app (head, arg), body_ty

    | If_then_else (head, tm1, ({ span = tm2_span; _ } as tm2)) ->
        let head = check_tm ctx head Bool_type in
        let tm1, ty1 = infer_tm ctx tm1 in
        let tm2, ty2 = infer_tm ctx tm2 in
        equate_ty tm2_span ~found:ty2 ~expected:ty1;
        Bool_elim (head, tm1, tm2), ty1

    | Infix (`Eq, tm1, tm2) ->
        let tm1, ty1 = infer_tm ctx tm1 in
        let tm2, ty2 = infer_tm ctx tm2 in
        equate_ty tm.span ~found:ty2 ~expected:ty1;
        begin match ty1 with
        | Bool_type -> Prim_app (Bool_eq, [tm1; tm2]), Bool_type
        | Int_type -> Prim_app (Int_eq, [tm1; tm2]), Bool_type
        | ty -> error tm.span (Format.asprintf "@[unsupported type: %t@]" (Core.pp_ty ty))
        end

    | Infix ((`Add | `Sub | `Mul) as prim, tm1, tm2) ->
        let prim =
          match prim with
          | `Add -> Prim.Int_add
          | `Sub -> Prim.Int_sub
          | `Mul -> Prim.Int_mul
        in
        let tm1 = check_tm ctx tm1 Int_type in
        let tm2 = check_tm ctx tm2 Int_type in
        Prim_app (prim, [tm1; tm2]), Int_type

    | Prefix (`Neg, tm) ->
        let tm = check_tm ctx tm Int_type in
        Prim_app (Int_neg, [tm]), Int_type

  (** Elaborate a function literal into a core term, given an expected type. *)
  and check_fun_lit (ctx : context) (params : param list) (body : tm) (ty : Core.ty) : Core.tm =
    match params, ty with
    | [], ty ->
        check_tm ctx body ty
    | (name, None) :: params, Fun_type (param_ty, body_ty) ->
        let body = check_fun_lit ((name.data, param_ty) :: ctx) params body body_ty in
        Fun_lit (name.data, param_ty, body)
    | (name, Some param_ty) :: params, Fun_type (param_ty', body_ty) ->
        let param_ty_span = param_ty.span in
        let param_ty = check_ty param_ty in
        equate_ty param_ty_span ~found:param_ty ~expected:param_ty';
        let body = check_fun_lit ((name.data, param_ty) :: ctx) params body body_ty in
        Fun_lit (name.data, param_ty, body)
    | (name, _) :: _, _ ->
        error name.span "unexpected parameter"

  (** Elaborate a function literal into a core term, inferring its type. *)
  and infer_fun_lit (ctx : context) (params : param list) (body_ty : ty option) (body : tm) : Core.tm * Core.ty =
    match params, body_ty with
    | [], Some body_ty ->
        let body_ty = check_ty body_ty in
        check_tm ctx body body_ty, body_ty
    | [], None ->
        infer_tm ctx body
    | (name, None) :: _, _ ->
        error name.span "ambiguous parameter type"
    | (name, Some param_ty) :: params, body_ty ->
        let param_ty = check_ty param_ty in
        let body, body_ty = infer_fun_lit ((name.data, param_ty) :: ctx) params body_ty body in
        Fun_lit (name.data, param_ty, body), Fun_type (param_ty, body_ty)


  (** {2 Running elaboration} *)

  let run_elab (type a) (prog : unit -> a) : (a, span * string) result =
    match prog () with
    | result -> Ok result
    | exception Error (span, message) -> Error (span, message)


  (** {2 Public API} *)

  let check_ty (ty : ty) : (Core.ty, span * string) result =
    run_elab (fun () -> check_ty ty)

  let check_tm (tm : tm) (ty : Core.ty) : (Core.tm, span * string) result =
    run_elab (fun () -> check_tm [] tm ty)

  let infer_tm (tm : tm) : (Core.tm * Core.ty, span * string) result =
    run_elab (fun () -> infer_tm [] tm)

end
