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

(** Names that bind definitions or parameters *)
type binder = string spanned

(** Terms in the surface language *)
type tm =
  tm_data spanned

and tm_data =
  | Name of string
  | Prim of string
  | Let of binder * param list * tm option * tm * tm
  | Ann of tm * tm
  | Fun_lit of param list * tm
  | Int_lit of int
  | App of tm * tm
  | If_then_else of tm * tm * tm
  | Infix of [`Arrow | `Eq | `Add | `Sub | `Mul] * tm * tm
  | Prefix of [`Neg] * tm

(** Parameters, with optional type annotations *)
and param =
  binder * tm option


(** Elaboration from the surface language into the core language

    This is where we implement user-facing type checking, while also translating
    the surface language into the simpler, more explicit core language.

    While we {e could} translate syntactic sugar in the parser, by leaving
    this to elaboration time we make it easier to report higher quality error
    messages that are more relevant to what the programmer originally wrote.
*)
module Elab : sig

  type elab_tm =
    | Univ
    | Type of Core.ty
    | Expr of Core.expr * Core.ty

  val check_ty : tm -> (Core.ty, span * string) result
  val check_expr : tm -> Core.ty -> (Core.expr, span * string) result
  val infer_expr : tm -> (Core.expr * Core.ty, span * string) result
  val infer : tm -> (elab_tm, span * string) result

end = struct

  (** This type allows us to define a bidirectional type checking algorithm that
      works over multiple levels of our core language. Universes only exist as
      part of the elaboration process. *)
  type elab_tm =
    | Univ
    | Type of Core.ty
    | Expr of Core.expr * Core.ty


  (** {2 Elaboration context} *)

  type entry =
    | Univ_def of string
    | Type_def of string * Core.ty
    | Expr_def of string * Core.ty

  (** A stack of bindings currently in scope *)
  type context = entry Core.env

  (** Lookup a name in the context *)
  let lookup (ctx : context) (name : string) : elab_tm option =
    let rec go ctx i =
      match ctx with
      | (Univ_def name') :: ctx -> if name = name' then Some Univ else go ctx i
      | (Type_def (name', t)) :: ctx -> if name = name' then Some (Type t) else go ctx i
      | (Expr_def (name', t)) :: ctx -> if name = name' then Some (Expr (Core.Var i, t)) else go ctx (i + 1)
      | [] -> None
    in
    go ctx 0


  (** {2 Elaboration errors} *)

  (** An exception used internally when encountering errors. These are expected
      to be caught later by the {!run_elab} function and should never escape
      this module.

      Real-world implementations should use error recovery so that elaboration
      can proceed after errors have been encountered. See [elab-error-recovery]
      for an example of how to implement this. *)
  exception Error of span * string

  (** Raise an elaboration error with a formatted message *)
  let error (type a b) (span : span) : (b, Format.formatter, unit, a) format4 -> b =
    Format.kasprintf (fun message -> raise (Error (span, message)))

  let equate_ty (span : span) ~(found : Core.ty) ~(expected : Core.ty) =
    if found = expected then () else
      error span "@[<v 2>@[mismatched types:@]@ @[expected: %t@]@ @[   found: %t@]@]"
        (Core.pp_ty expected)
        (Core.pp_ty found)


  (** {2 Bidirectional type checking} *)

  (** The algorithm is structured {i bidirectionally}, divided into mutually
      recursive {i checking} and {i inference} modes. By supplying type
      annotations as early as possible using the checking mode, we can improve
      the locality of type errors. We can also extend the type system with
      advanced features like dependent types, higher rank types, and subtyping
      while maintaining decidability by allowing the programmer to supply
      annotations where necessary. *)

  (** Elaborate a surface term into a core type. *)
  let rec check_ty (ctx : context) (tm : tm) : Core.ty =
    match tm.data with
    | Let (def_name, params, def_body_t, def_body, body) ->
        begin match infer_fun_lit ctx params def_body_t def_body with
        | Univ -> check_ty (Univ_def def_name.data :: ctx) body
        | Type def -> check_ty (Type_def (def_name.data, def) :: ctx) body
        | Expr (_, def_t) -> check_ty (Expr_def (def_name.data, def_t) :: ctx) body
        end

    | _ ->
        begin match infer ctx tm with
        | Univ -> error tm.span "expected type, found universe"
        | Type t -> t
        | Expr (_, _) -> error tm.span "expected type, found expression"
        end

  (** Elaborate a surface term into a core expression, given an expected type. *)
  and check_expr (ctx : context) (tm : tm) (t : Core.ty) : Core.expr =
    match tm.data with
    | Let (def_name, params, def_body_t, def_body, body) ->
        begin match infer_fun_lit ctx params def_body_t def_body with
        | Univ -> check_expr (Univ_def def_name.data :: ctx) body t
        | Type def -> check_expr (Type_def (def_name.data, def) :: ctx) body t
        | Expr (def, def_t) ->
            let body = check_expr (Expr_def (def_name.data, def_t) :: ctx) body t in
            Core.Let (def_name.data, def_t, def, body)
        end

    | Fun_lit (params, body) ->
        check_fun_lit ctx params body t

    | If_then_else (head, tm1, tm2) ->
        let head = check_expr ctx head Core.Bool_type in
        let e1 = check_expr ctx tm1 t in
        let e2 = check_expr ctx tm2 t in
        Core.Bool_elim (head, e1, e2)

    | _ ->
        let e', t' = infer_expr ctx tm in
        equate_ty tm.span ~found:t' ~expected:t;
        e'

  (** Elaborate a surface term into a core term, inferring its type. *)
  and infer (ctx : context) (tm : tm) : elab_tm =
    match tm.data with
    | Name n ->
        begin match lookup ctx n with
        | Some tm -> tm
        | None when n = "Type" -> Univ
        | None when n = "Bool" -> Type Core.Bool_type
        | None when n = "true" -> Expr (Core.Bool_lit true, Core.Bool_type)
        | None when n = "false" -> Expr (Core.Bool_lit false, Core.Bool_type)
        | None when n = "Int" -> Type Core.Int_type
        | None -> error tm.span "unbound name `%s`" n
        end

    | Prim name ->
        begin match Prim.of_name name with
        | Some (Bool_eq as prim) -> Expr (Core.Prim prim, Core.(Fun_type (Bool_type, Fun_type (Bool_type, Bool_type))))
        | Some (Int_eq as prim) -> Expr (Core.Prim prim, Core.(Fun_type (Int_type, Fun_type (Int_type, Bool_type))))
        | Some (Int_add as prim) -> Expr (Core.Prim prim, Core.(Fun_type (Int_type, Fun_type (Int_type, Int_type))))
        | Some (Int_sub as prim) -> Expr (Core.Prim prim, Core.(Fun_type (Int_type, Fun_type (Int_type, Int_type))))
        | Some (Int_mul as prim) -> Expr (Core.Prim prim, Core.(Fun_type (Int_type, Fun_type (Int_type, Int_type))))
        | Some (Int_neg as prim) -> Expr (Core.Prim prim, Core.(Fun_type (Int_type, Int_type)))
        | None -> error tm.span "unknown primitive operation `#%s`" name
        end

    | Let (def_name, params, def_body_t, def_body, body) ->
        begin match infer_fun_lit ctx params def_body_t def_body with
        | Univ -> infer (Univ_def def_name.data :: ctx) body
        | Type def -> infer (Type_def (def_name.data, def) :: ctx) body
        | Expr (def, def_t) ->
            begin match infer (Expr_def (def_name.data, def_t) :: ctx) body with
            | (Univ | Type _) as elab_tm -> elab_tm
            | Expr (body, body_t) -> Expr (Core.Let (def_name.data, def_t, def, body), body_t)
            end
        end

    | Ann (tm, ty) ->
        begin match infer ctx ty with
        | Univ -> Type (check_ty ctx tm)
        | Type t -> Expr (check_expr ctx tm t, t)
        | Expr (_, _) -> error tm.span "expected type or universe, found expression"
        end

    | Int_lit i -> Expr (Core.Int_lit i, Core.Int_type)

    | Fun_lit (params, body) ->
        infer_fun_lit ctx params None body

    | App (head, arg) ->
        let head, head_t = infer_expr ctx head in
        let param_t, body_t =
          match head_t with
          | Core.Fun_type (param_t, body_t) -> param_t, body_t
          | _ -> error arg.span "unexpected argument"
        in
        let arg = check_expr ctx arg param_t in
        Expr (Core.Fun_app (head, arg), body_t)

    | If_then_else (head, expr1, ({ span = expr2_span; _ } as expr2)) ->
        let head = check_expr ctx head Bool_type in
        let e1, t1 = infer_expr ctx expr1 in
        let e2, t2 = infer_expr ctx expr2 in
        equate_ty expr2_span ~found:t2 ~expected:t1;
        Expr (Core.Bool_elim (head, e1, e2), t1)

    | Infix (`Arrow, param_t, body_t) ->
        let param_t = check_ty ctx param_t in
        let body_t = check_ty ctx body_t in
        Type (Core.Fun_type (param_t, body_t))

    | Infix (`Eq, tm1, tm2) ->
        let e1, t1 = infer_expr ctx tm1 in
        let e2, t2 = infer_expr ctx tm2 in
        equate_ty tm.span ~found:t2 ~expected:t1;
        begin match t1 with
        | Core.Bool_type -> Expr (Core.(fun_app (Prim Prim.Bool_eq) [e1; e2]), Core.Bool_type)
        | Core.Int_type -> Expr (Core.(fun_app (Prim Prim.Int_eq) [e1; e2]), Core.Bool_type)
        | t -> error tm.span "@[unsupported type: %t@]" (Core.pp_ty t)
        end

    | Infix ((`Add | `Sub | `Mul) as prim, tm1, tm2) ->
        let prim =
          match prim with
          | `Add -> Prim.Int_add
          | `Sub -> Prim.Int_sub
          | `Mul -> Prim.Int_mul
        in
        let e1 = check_expr ctx tm1 Core.Int_type in
        let e2 = check_expr ctx tm2 Core.Int_type in
        Expr (Core.(fun_app (Prim prim) [e1; e2]), Core.Int_type)

    | Prefix (`Neg, tm) ->
        let e = check_expr ctx tm Core.Int_type in
        Expr (Core.(fun_app (Prim Prim.Int_neg) [e]), Core.Int_type)

  and infer_expr (ctx : context) (tm : tm) : Core.expr * Core.ty =
    match infer ctx tm with
    | Univ -> error tm.span "expected expression, found universe"
    | Type _ -> error tm.span "expected expression, found type"
    | Expr (e, t) -> e, t

  (* Function elaboration *)

  (** Elaborate a function literal into a core term, given an expected type. *)
  and check_fun_lit (ctx : context) (params : param list) (body : tm) (ty : Core.ty) : Core.expr =
    match params, ty with
    | [], ty ->
        check_expr ctx body ty

    | (name, None) :: params, Core.Fun_type (param_t, body_t) ->
        let body = check_fun_lit (Expr_def (name.data, param_t) :: ctx) params body body_t in
          Core.Fun_lit (name.data, param_t, body)

    | (name, Some param_t) :: params, Core.Fun_type (param_t', body_t) ->
        let param_t_span = param_t.span in
        let param_t = check_ty ctx param_t in
        equate_ty param_t_span ~found:param_t ~expected:param_t';
        let body = check_fun_lit (Expr_def (name.data, param_t) :: ctx) params body body_t in
        Core.Fun_lit (name.data, param_t, body)

    | (name, _) :: _, _ ->
        error name.span "unexpected parameter"

  (** Elaborate a function literal into a core term, inferring its type. *)
  and infer_fun_lit (ctx : context) (params : param list) (body_t : tm option) (body : tm) : elab_tm =
    match params, body_t with
    | [], None ->
        infer ctx body

    | [], Some body_t ->
        begin match infer ctx body_t with
        | Univ -> Type (check_ty ctx body)
        | Type body_t -> Expr (check_expr ctx body body_t, body_t)
        | Expr (_, _) -> error body_t.span "expected type or universe, found expression"
        end

    | (name, None) :: _, _ ->
        error name.span "ambiguous parameter type"

    | (name, Some param_t) :: params, body_t ->
        let param_t = check_ty ctx param_t in
        let body, body_t =
          match infer_fun_lit (Expr_def (name.data, param_t) :: ctx) params body_t body with
          | Univ -> error body.span "expected expression, found universe"
          | Type _ -> error body.span "expected expression, found type"
          | Expr (e, t) -> e, t
        in
        Expr (Core.Fun_lit (name.data, param_t, body), Core.Fun_type (param_t, body_t))


  (** {2 Running elaboration} *)

  let run_elab (type a) (prog : unit -> a) : (a, span * string) result =
    match prog () with
    | result -> Ok result
    | exception Error (span, message) -> Error (span, message)


  (** {2 Public API} *)

  let check_ty (tm : tm) : (Core.ty, span * string) result =
    run_elab (fun () -> check_ty [] tm)

  let check_expr (tm : tm) (ty : Core.ty) : (Core.expr, span * string) result =
    run_elab (fun () -> check_expr [] tm ty)

  let infer_expr (tm : tm) : (Core.expr * Core.ty, span * string) result =
    run_elab (fun () -> infer_expr [] tm)

  let infer (tm : tm) : (elab_tm, span * string) result =
    run_elab (fun () -> infer [] tm)

end
