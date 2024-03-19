(** {0 Surface language}

    The surface language closely mirrors what the programmer originaly wrote,
    including syntactic sugar and higher level language features that make
    programming more convenient (in comparison to the {!Core}).
*)

(** {1 Syntax} *)

(** The start and end position in a source file *)
type loc =
  Lexing.position * Lexing.position

(** Located nodes *)
type 'a located = {
  loc : loc;
  data : 'a;
}

(** Names that bind definitions or parameters *)
type binder = string located

(** Terms in the surface language *)
type tm =
  tm_data located

and tm_data =
  | Name of string
  | Let of binder * param list * tm option * tm * tm
  | Ann of tm * tm
  | FunLit of param list * tm
  | BoolLit of bool
  | IntLit of int
  | App of tm * tm
  | IfThenElse of tm * tm * tm
  | Op2 of [`Arrow | `Eq | `Add | `Sub | `Mul] * tm * tm
  | Op1 of [`Neg] * tm

(** Parameters, with optional type annotations *)
and param =
  binder * tm option


(** {1 Elaboration} *)

module Elab = struct
  (** This is where we implement user-facing type checking, while also translating
      the surface language into the simpler, more explicit core language.

      While we {e could} translate syntactic sugar in the parser, by leaving
      this to elaboration time we make it easier to report higher quality error
      messages that are more relevant to what the programmer originally wrote.
  *)

  (** This type allows us to define a bidirectional type checking algorithm that
      works over multiple levels of our core language. Universes only exist as
      part of the elaboration process. *)
  type elab_tm =
    | Univ
    | Type of Core.ty
    | Expr of Core.expr * Core.ty


  (** {2 Local bindings} *)

  type entry =
    | UnivDef of string
    | TypeDef of string * Core.ty
    | ExprDef of string * Core.ty

  (** A stack of bindings currently in scope *)
  type context = entry Core.env

  (** Lookup a name in the context *)
  let lookup (ctx : context) (name : string) : elab_tm option =
    let rec go ctx i =
      match ctx with
      | (UnivDef name') :: ctx -> if name = name' then Some Univ else go ctx i
      | (TypeDef (name', t)) :: ctx -> if name = name' then Some (Type t) else go ctx i
      | (ExprDef (name', t)) :: ctx -> if name = name' then Some (Expr (Var i, t)) else go ctx (i + 1)
      | [] -> None
    in
    go ctx 0


  (** {2 Elaboration errors} *)

  (** An error that will be raised if there was a problem in the surface syntax,
      usually as a result of type errors. This is normal, and should be rendered
      nicely to the programmer. *)
  exception Error of loc * string

  (** Raises an {!Error} exception *)
  let error (loc : loc) (message : string) : 'a =
    raise (Error (loc, message))

  let equate_ty (loc : loc) (ty1 : Core.ty) (ty2 : Core.ty) =
    if ty1 = ty2 then () else
      error loc
        (Format.asprintf "@[<v 2>@[mismatched types:@]@ @[expected: %a@]@ @[found: %a@]@]"
          Core.pp_ty ty1
          Core.pp_ty ty2)


  (** {2 Bidirectional type checking} *)

  (** The algorithm is structured {i bidirectionally}, divided into mutually
      recursive {i checking} and {i inference} modes. By supplying type
      annotations as early as possible using the checking mode, we can improve
      the locality of type errors. We can also extend the type system with
      advanced features like dependent types, higher rank types, and subtyping
      while maintaining decidability by allowing the programmer to supply
      annotations where necessary. *)

  (** Elaborate a surface term into a core type. *)
  let rec check_type (ctx : context) (tm : tm) : Core.ty =
    match tm.data with
    | Let (def_name, params, def_body_t, def_body, body) -> begin
      match infer_fun_lit ctx params def_body_t def_body with
      | Univ -> check_type (UnivDef def_name.data :: ctx) body
      | Type def -> check_type (TypeDef (def_name.data, def) :: ctx) body
      | Expr (_, def_t) -> check_type (ExprDef (def_name.data, def_t) :: ctx) body
    end

    | _ -> begin
      match infer ctx tm with
      | Univ -> error tm.loc "expected type, found universe"
      | Type t -> t
      | Expr (_, _) -> error tm.loc "expected type, found expression"
    end

  (** Elaborate a surface term into a core expression, given an expected type. *)
  and check_expr (ctx : context) (tm : tm) (t : Core.ty) : Core.expr =
    match tm.data with
    | Let (def_name, params, def_body_t, def_body, body) -> begin
      match infer_fun_lit ctx params def_body_t def_body with
      | Univ -> check_expr (UnivDef def_name.data :: ctx) body t
      | Type def -> check_expr (TypeDef (def_name.data, def) :: ctx) body t
      | Expr (def, def_t) -> begin
        let body = check_expr (ExprDef (def_name.data, def_t) :: ctx) body t in
        Let (def_name.data, def_t, def, body)
      end
    end

    | FunLit (params, body) ->
      check_fun_lit ctx params body t

    | IfThenElse (head, tm0, tm1) ->
      let head = check_expr ctx head Core.BoolType in
      let e0 = check_expr ctx tm0 t in
      let e1 = check_expr ctx tm1 t in
      BoolElim (head, e0, e1)

    | _ ->
      let e', t' = infer_expr ctx tm in
      equate_ty tm.loc t t';
      e'

  (** Elaborate a surface term into a core term, inferring its type. *)
  and infer (ctx : context) (tm : tm) : elab_tm =
    match tm.data with
    | Name n -> begin
      match lookup ctx n with
      | Some tm -> tm
      | None when n = "Type" -> Univ
      | None when n = "Bool" -> Type BoolType
      | None when n = "Int" -> Type IntType
      | None -> error tm.loc (Format.asprintf "unbound name `%s`" n)
    end

    | Let (def_name, params, def_body_t, def_body, body) -> begin
      match infer_fun_lit ctx params def_body_t def_body with
      | Univ -> infer (UnivDef def_name.data :: ctx) body
      | Type def -> infer (TypeDef (def_name.data, def) :: ctx) body
      | Expr (def, def_t) -> begin
        match infer (ExprDef (def_name.data, def_t) :: ctx) body with
        | (Univ | Type _) as elab_tm -> elab_tm
        | Expr (body, body_t) -> Expr (Let (def_name.data, def_t, def, body), body_t)
      end
    end

    | Ann (tm, ty) -> begin
      match infer ctx ty with
      | Univ -> Type (check_type ctx tm)
      | Type t -> Expr (check_expr ctx tm t, t)
      | Expr (_, _) -> error tm.loc "expected type or universe, found expression"
    end

    | BoolLit b -> Expr (BoolLit b, BoolType)
    | IntLit i -> Expr (IntLit i, IntType)

    | FunLit (params, body) ->
      infer_fun_lit ctx params None body

    | App (head, arg) ->
      let head_loc = head.loc in
      let head, head_t = infer_expr ctx head in
      let param_t, body_t =
        match head_t with
        | FunType (param_t, body_t) -> param_t, body_t
        | head_t ->
            error head_loc
              (Format.asprintf "@[<v 2>@[mismatched types:@]@ @[expected: function@]@ @[found: %a@]@]"
                Core.pp_ty head_t)
      in
      let arg = check_expr ctx arg param_t in
      Expr (FunApp (head, arg), body_t)

    | IfThenElse (_, _, _) ->
      error tm.loc "ambiguous if expression"

    | Op2 (`Arrow, param_t, body_t) ->
      let param_t = check_type ctx param_t in
      let body_t = check_type ctx body_t in
      Type (FunType (param_t, body_t))

    | Op2 ((`Eq) as prim, tm0, tm1) ->
      let e0 = check_expr ctx tm0 Core.IntType in
      let e1 = check_expr ctx tm1 Core.IntType in
      Expr (PrimApp (prim, [e0; e1]), BoolType)

    | Op2 ((`Add | `Sub | `Mul) as prim, tm0, tm1) ->
      let e0 = check_expr ctx tm0 Core.IntType in
      let e1 = check_expr ctx tm1 Core.IntType in
      Expr (PrimApp (prim, [e0; e1]), IntType)

    | Op1 ((`Neg) as prim, tm) ->
      let e = check_expr ctx tm IntType in
      Expr (PrimApp (prim, [e]), IntType)

  and infer_expr (ctx : context) (tm : tm) : Core.expr * Core.ty =
    match infer ctx tm with
    | Univ -> error tm.loc "expected expression, found universe"
    | Type _ -> error tm.loc "expected expression, found type"
    | Expr (e, t) -> e, t

  (* Function elaboration *)

  (** Elaborate a function literal into a core term, given an expected type. *)
  and check_fun_lit (ctx : context) (params : param list) (body : tm) (ty : Core.ty) : Core.expr =
    match params, ty with
    | [], ty ->
      check_expr ctx body ty

    | (name, None) :: params, FunType (param_t, body_t) ->
      let body = check_fun_lit (ExprDef (name.data, param_t) :: ctx) params body body_t in
        FunLit (name.data, param_t, body)

    | (name, Some param_t) :: params, FunType (param_t', body_t) ->
      let param_t_loc = param_t.loc in
      let param_t = check_type ctx param_t in
      equate_ty param_t_loc param_t param_t';
      let body = check_fun_lit (ExprDef (name.data, param_t) :: ctx) params body body_t in
      FunLit (name.data, param_t, body)

    | (name, _) :: _, _ ->
      error name.loc "unexpected parameter"

  (** Elaborate a function literal into a core term, inferring its type. *)
  and infer_fun_lit (ctx : context) (params : param list) (body_t : tm option) (body : tm) : elab_tm =
    match params, body_t with
    | [], None ->
      infer ctx body

    | [], Some body_t -> begin
      match infer ctx body_t with
      | Univ -> Type (check_type ctx body)
      | Type body_t -> Expr (check_expr ctx body body_t, body_t)
      | Expr (_, _) -> error body_t.loc "expected type or universe, found expression"
    end

    | (name, None) :: _, _ ->
      error name.loc "ambiguous parameter type"

    | (name, Some param_t) :: params, body_t ->
      let param_t = check_type ctx param_t in
      let body, body_t =
        match infer_fun_lit (ExprDef (name.data, param_t) :: ctx) params body_t body with
        | Univ -> error body.loc "expected expression, found universe"
        | Type _ -> error body.loc "expected expression, found type"
        | Expr (e, t) -> e, t
      in
      Expr (FunLit (name.data, param_t, body), FunType (param_t, body_t))

end
