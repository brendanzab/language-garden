(** An elaborator from an untyped surface language into a well-typed core
    language. Extends [eval_stlc_gadt].

    Based on {{: https://discuss.ocaml.org/t/parsing-terms-into-a-well-typed-representation-a-gadt-puzzle/8688}
    “Parsing” terms into a well-typed representation: a GADT puzzle} by gasche.
*)

[@@@warning "-unused-constructor"]
[@@@warning "-unused-value-declaration"]

(** Well-typed core language *)
module Core = struct

  type 'a ty =
    | Unit_ty : unit ty
    | Fun_ty : 'a ty * 'b ty -> ('a -> 'b) ty

  type ('ctx, 'a) index =
    | Stop : ('a * 'ctx, 'a) index
    | Pop : ('ctx, 'a) index -> ('b * 'ctx, 'a) index

  type ('ctx, 'a) expr =
    | Var : ('ctx, 'a) index -> ('ctx, 'a) expr
    | Let : ('ctx, 'a) expr * ('a * 'ctx, 'b) expr -> ('ctx, 'b) expr
    | Fun_lit : ('a * 'ctx, 'b) expr -> ('ctx, 'a -> 'b) expr
    | Fun_app : ('ctx, 'a -> 'b) expr * ('ctx, 'a) expr -> ('ctx, 'b) expr
    | Unit_lit : ('ctx, unit) expr

  type 'ctx env =
    | [] : unit env
    | ( :: ) : 'a * 'ctx env -> ('a * 'ctx) env

  let rec lookup : type ctx a. (ctx, a) index -> ctx env -> a =
    fun x env ->
      match x, env with
      | Stop, v :: _ -> v
      | Pop x, _ :: env -> lookup x env

  let rec eval : type ctx a. ctx env -> (ctx, a) expr -> a =
    fun env expr ->
      match expr with
      | Var x -> lookup x env
      | Let (def, body) -> eval (eval env def :: env) body
      | Fun_lit body -> fun x -> eval (x :: env) body
      | Fun_app (fn, arg) -> (eval env fn) (eval env arg)
      | Unit_lit -> ()

  (** Existential types *)

  type some_ty = Ty : 'a ty -> some_ty
  type 'ctx some_index = Index : 'a ty * ('ctx, 'a) index -> 'ctx some_index
  type 'ctx some_expr = Expr : 'a ty * ('ctx, 'a) expr -> 'ctx some_expr

  (** Compare two types for equality *)
  let rec eq_ty : type a b . a ty -> b ty -> (a, b) Type.eq option =
    fun ty1 ty2 ->
      match ty1, ty2 with
      | Unit_ty, Unit_ty -> Some Equal
      | Fun_ty (arg_ty1, body_ty1), Fun_ty (arg_ty2, body_ty_2) ->
          begin match eq_ty arg_ty1 arg_ty2, eq_ty body_ty1 body_ty_2 with
          | Some Equal, Some Equal -> Some Equal
          | _, _ -> None
          end
      | _, _ -> None

end

(** Untyped surface language *)
module Surface = struct

  type ty =
    | Unit_ty
    | Fun_ty of ty * ty

  type expr =
    | Let : (string * ty * expr) * expr -> expr
    | Var : string -> expr
    | Fun_lit : (string * ty) * expr -> expr
    | Fun_app : expr * expr -> expr
    | Unit_lit : expr

  type 'ctx env =
    | [] : unit env
    | ( :: ) : (string * 'a Core.ty) * 'ctx env -> ('a * 'ctx) env

  let rec elab_ty : ty -> Core.some_ty =
    function
    | Unit_ty -> Ty Unit_ty
    | Fun_ty (arg_ty, body_ty) ->
        let Ty arg_ty = elab_ty arg_ty in
        let Ty body_ty = elab_ty body_ty in
        Ty (Fun_ty (arg_ty, body_ty))

  exception Unbound_var

  let rec elab_var : type ctx. ctx env -> string -> ctx Core.some_index =
    fun env name ->
      match env with
      | [] -> raise Unbound_var
      | (name', ty) :: env ->
          if name = name' then Index (ty, Stop) else
            let (Index (ty, idx)) = elab_var env name in
            Index (ty, Pop idx)

  exception Type_mismatch

  let rec check_expr : type ctx a. ctx env -> expr -> a Core.ty -> (ctx, a) Core.expr =
    fun env expr expected_ty ->
      let Expr (ty, expr) = synth_expr env expr in
      match Core.eq_ty ty expected_ty with
      | Some Equal -> expr
      | _ -> raise Type_mismatch

  and synth_expr : type ctx. ctx env -> expr -> ctx Core.some_expr =
    fun env expr ->
      match expr with
      | Var name ->
          let Index (ty, index) = elab_var env name in
          Expr (ty, Var index)
      | Let ((name, def_ty, def_expr), body_expr) ->
          let Ty def_ty = elab_ty def_ty in
          let def_expr = check_expr env def_expr def_ty in
          let Expr (body_ty, body_expr) = synth_expr ((name, def_ty) :: env) body_expr in
          Expr (body_ty, Let (def_expr, body_expr))
      | Fun_lit ((name, param_ty), body_expr) ->
          let Ty param_ty = elab_ty param_ty in
          let Expr (body_ty, body_expr) = synth_expr ((name, param_ty) :: env) body_expr in
          Expr (Fun_ty (param_ty, body_ty), Fun_lit body_expr)
      | Fun_app (fn_expr, arg_expr) ->
          let Expr (fn_ty, fn_expr) = synth_expr env fn_expr in
          begin match fn_ty with
          | Fun_ty (param_ty, body_ty) ->
              let arg_expr = check_expr env arg_expr param_ty in
              Expr (body_ty, Fun_app (fn_expr, arg_expr))
          | _ -> raise Type_mismatch
          end
      | Unit_lit ->
          Expr (Unit_ty, Unit_lit)

end
