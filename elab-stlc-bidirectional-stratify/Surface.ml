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

  (* $MDX part-begin=elab-types *)
  (* An elaborated type *)
  type _ elab_ty =
    | Univ1 : [`Univ1] elab_ty
    | Univ0 : [`Univ0] elab_ty
    | Type : Core.ty -> Core.ty elab_ty

  (* An elaborated term *)
  type _ elab_tm =
    | Univ0 : [`Univ1] elab_tm
    | Type : Core.ty -> [`Univ0] elab_tm
    | Expr : Core.expr -> Core.ty elab_tm

  type ann_tm =
    | AnnTm : 'ann elab_tm * 'ann elab_ty -> ann_tm
  (* $MDX part-end *)


  (** {2 Local bindings} *)

  (** A stack of bindings currently in scope *)
  type context = (string * Core.ty) Core.env

  (** Lookup a name in the context *)
  let lookup (ctx : context) (name : string) : (Core.index * Core.ty) option =
    ctx |> List.find_mapi @@ fun index (name', ty) ->
      match name = name' with
      | true -> Some (index, ty)
      | false -> None


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

  (** Elaborate a surface term into a core term, given an expected type. *)
  let rec check : type ann. context -> tm -> ann elab_ty -> ann elab_tm =
    fun ctx tm ty ->
      match tm.data, ty with
      | _, Univ1 -> begin
        match infer ctx tm with
        | AnnTm (Expr _, Type _) -> error tm.loc "expected universe, found expression"
        | AnnTm (Type _, Univ0) -> error tm.loc "expected universe, found type"
        | AnnTm (Univ0, Univ1) -> Univ0
      end

      | _, Univ0 -> begin
        match infer ctx tm with
        | AnnTm (Expr _, Type _) -> error tm.loc "expected type, found expression"
        | AnnTm (Type t, Univ0) -> Type t
        | AnnTm (Univ0, Univ1) -> error tm.loc "expected type, found universe"
      end

      | Let (def_name, params, def_body_t, def_body, body), Type t ->
        let def, def_t = infer_fun_lit ctx params def_body_t def_body in
        let body = check_expr ((def_name.data, def_t) :: ctx) body t in
        Expr (Let (def_name.data, def_t, def, body))

      | FunLit (params, body), Type t ->
        Expr (check_fun_lit ctx params body t)

      | IfThenElse (head, tm0, tm1), Type t ->
        let head = check_expr ctx head Core.BoolType in
        let e0 = check_expr ctx tm0 t in
        let e1 = check_expr ctx tm1 t in
        Expr (BoolElim (head, e0, e1))

      | _, Type t ->
        let e', t' = infer_expr ctx tm in
        equate_ty tm.loc t t';
        Expr e'

  (** Elaborate a surface term into a core term, inferring its type. *)
  and infer (ctx : context) (tm : tm) : ann_tm =
    match tm.data with
    | Name n -> begin
      match lookup ctx n with
      | Some (i, ty) -> AnnTm (Expr (Var i), Type ty)
      | None when n = "Type" -> AnnTm (Univ0, Univ1)
      | None when n = "Bool" -> AnnTm (Type BoolType, Univ0)
      | None when n = "Int" -> AnnTm (Type IntType, Univ0)
      | None -> error tm.loc (Format.asprintf "unbound name `%s`" n)
    end

    | Let (def_name, params, def_body_t, def_body, body) ->
      let def, def_t = infer_fun_lit ctx params def_body_t def_body in
      let body, body_t = infer_expr ((def_name.data, def_t) :: ctx) body in
      AnnTm (Expr (Let (def_name.data, def_t, def, body)), Type body_t)

    | Ann (tm, ty) -> begin
      match infer ctx ty with
      | AnnTm (Expr _, Type _) -> error tm.loc "expected type, found expression"
      | AnnTm (Type t, Univ0) -> AnnTm (check ctx tm (Type t), Type t)
      | AnnTm (Univ0, Univ1) -> AnnTm (check ctx tm Univ0, Univ0)
    end

    | BoolLit b -> AnnTm (Expr (BoolLit b), Type BoolType)
    | IntLit i -> AnnTm (Expr (IntLit i), Type IntType)

    | FunLit (params, body) ->
      let e, t = infer_fun_lit ctx params None body in
      AnnTm (Expr e, Type t)

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
      AnnTm (Expr (FunApp (head, arg)), Type body_t)

    | IfThenElse (_, _, _) ->
      error tm.loc "ambiguous if expression"

    | Op2 (`Arrow, param_t, body_t) ->
      let param_t = check_type ctx param_t in
      let body_t = check_type ctx body_t in
      AnnTm (Type (FunType (param_t, body_t)), Univ0)

    | Op2 ((`Eq) as prim, tm0, tm1) ->
      let e0 = check_expr ctx tm0 Core.IntType in
      let e1 = check_expr ctx tm1 Core.IntType in
      AnnTm (Expr (PrimApp (prim, [e0; e1])), Type BoolType)

    | Op2 ((`Add | `Sub | `Mul) as prim, tm0, tm1) ->
      let e0 = check_expr ctx tm0 Core.IntType in
      let e1 = check_expr ctx tm1 Core.IntType in
      AnnTm (Expr (PrimApp (prim, [e0; e1])), Type IntType)

    | Op1 ((`Neg) as prim, tm) ->
      let e = check_expr ctx tm IntType in
      AnnTm (Expr (PrimApp (prim, [e])), Type IntType)

  (* Specialised elaboration functions *)

  and check_expr (ctx : context) (tm : tm) (t : Core.ty) : Core.expr =
    let Expr e = check ctx tm (Type t) in e

  and check_type (ctx : context) (tm : tm) : Core.ty =
    let Type t = check ctx tm Univ0 in t

  and infer_expr (ctx : context) (tm : tm) : Core.expr * Core.ty =
    match infer ctx tm with
    | AnnTm (Expr e, Type t) -> e, t
    | AnnTm (Type _, Univ0) -> error tm.loc "expected expression, found type"
    | AnnTm (Univ0, Univ1) -> error tm.loc "expected expression, found universe"

  (* Function elaboration *)

  (** Elaborate a function literal into a core term, given an expected type. *)
  and check_fun_lit (ctx : context) (params : param list) (body : tm) (ty : Core.ty) : Core.expr =
    match params, ty with
    | [], ty ->
      check_expr ctx body ty

    | (name, None) :: params, FunType (param_t, body_t) ->
      let body = check_fun_lit ((name.data, param_t) :: ctx) params body body_t in
        FunLit (name.data, param_t, body)

    | (name, Some param_t) :: params, FunType (param_t', body_t) ->
      let param_t_loc = param_t.loc in
      let param_t = check_type ctx param_t in
      equate_ty param_t_loc param_t param_t';
      let body = check_fun_lit ((name.data, param_t) :: ctx) params body body_t in
      FunLit (name.data, param_t, body)

    | (name, _) :: _, _ ->
      error name.loc "unexpected parameter"

  (** Elaborate a function literal into a core term, inferring its type. *)
  and infer_fun_lit (ctx : context) (params : param list) (body_t : tm option) (body : tm) : Core.expr * Core.ty =
    match params, body_t with
    | [], Some body_t ->
      let body_t = check_type ctx body_t in
      check_expr ctx body body_t, body_t

    | [], None ->
      infer_expr ctx body

    | (name, None) :: _, _ ->
      error name.loc "ambiguous parameter type"

    | (name, Some param_t) :: params, body_t ->
      let param_t = check_type ctx param_t in
      let body, body_t = infer_fun_lit ((name.data, param_t) :: ctx) params body_t body in
      FunLit (name.data, param_t, body), FunType (param_t, body_t)

end
