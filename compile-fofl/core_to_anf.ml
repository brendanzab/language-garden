(** Hoist let expressions and conditionals to the top of expressions. *)

module Translate : sig

  val translate_module : Core.Module.t -> Anf.Module.t

end = struct

  module Env : sig

    type t

    val empty : item_tys:(Core.Item_name.t -> Prim.Ty.t) -> t

    val extend : Anf.Local_id.t * Prim.Ty.t -> t -> t
    val lookup_local_id : Core.Local.Index.t -> t -> Anf.Local_id.t
    val type_of : Core.Expr.t -> t -> Prim.Ty.t

  end = struct

    type t = {
      item_tys : Core.Item_name.t -> Core.Ty.t;
      local_ids : Anf.Local_id.t Core.Local.Env.t;
      local_tys : Core.Ty.t Core.Local.Env.t;
    }

    let empty ~item_tys = {
      item_tys;
      local_ids = Core.Local.Env.empty;
      local_tys = Core.Local.Env.empty;
    }

    let extend (id, ty) env = {
      env with
      local_ids = Core.Local.Env.extend id env.local_ids;
      local_tys = Core.Local.Env.extend ty env.local_tys;
    }

    let lookup_local_id index env =
      Core.Local.Env.lookup index env.local_ids

    let type_of expr env =
      Core.Expr.type_of env.item_tys env.local_tys expr

  end

  type 'a k = 'a -> Anf.(Expr.t * Ty.t)

  let fresh_local_id (name : string option) : Anf.Local_id.t =
    Anf.Local_id.fresh (Option.value name ~default:"")

  let translate_expr (env : Env.t) (expr : Core.Expr.t) : Anf.Expr.t =
    let ( let@ ) = ( @@ ) in

    let rec go_expr (env : Env.t) (expr : Core.Expr.t) : Anf.Expr.comp k k =
      fun k ->
        match expr with
        | Core.Expr.Item (id, None) ->
            k (Anf.Expr.Item (id, None))

        | Core.Expr.Item (id, Some args) ->
            let@ args = go_named_exprs env "arg" (Iarray.to_list args) in
            k (Anf.Expr.Item (id, Some (Iarray.of_list args)))

        | Core.Expr.Var index ->
            k (Anf.Expr.Atom (Var (Env.lookup_local_id index env)))

        | Core.Expr.Let ((name, def_ty, def), body) ->
            let id = fresh_local_id name in
            let@ def = go_expr env def in
            let body, body_ty = go_expr (Env.extend (id, def_ty) env) body k in
            Anf.Expr.Let (id, def_ty, def, body), body_ty

        | Core.Expr.Bool bool ->
            k (Anf.Expr.Atom (Bool bool))

        | Core.Expr.Bool_if (expr1, expr2, expr3) ->
            let@ expr1 = go_named_expr env "cond" expr1 in

            let param_ty = Env.type_of expr2 env in
            let param_id = Anf.Local_id.fresh "p" in
            let cont, cont_ty = k (Anf.Expr.Atom (Var param_id)) in

            let join_id = Anf.Join_id.fresh "j" in
            let jump_k x = Anf.Expr.Jump (join_id, x), cont_ty in

            let expr2, _ = go_named_expr env "result" expr2 jump_k in
            let expr3, _ = go_named_expr env "result" expr3 jump_k in

            Anf.Expr.Join (join_id, (param_id, param_ty), cont_ty, cont,
              Anf.Expr.Bool_if (expr1, expr2, expr3)),
            cont_ty

        | Core.Expr.I32 int ->
            k (Anf.Expr.Atom (I32 int))

        | Core.Expr.Prim (op, args) ->
            let@ args = go_named_exprs env "arg" (Iarray.to_list args) in
            k (Anf.Expr.Prim (op, Iarray.of_list args))

    and go_named_expr (env : Env.t) (name : string) (expr : Core.Expr.t) : Anf.Expr.atom k k =
      fun k ->
        let expr_ty = Env.type_of expr env in
        let@ expr = go_expr env expr in
        match expr with
        (* Don't bother binding definitions for atomic computations *)
        | Anf.Expr.Atom expr -> k expr
        (* Bind definitions for non-atomic computations *)
        | expr ->
            let id = Anf.Local_id.fresh name in
            let body, body_ty = k (Anf.Expr.Var id) in
            Anf.Expr.Let (id, expr_ty, expr, body), body_ty

    (* Compile a series of expressions to intermediate definitions *)
    and go_named_exprs (env : Env.t) (name : string) (exprs : Core.Expr.t list) : Anf.Expr.atom list k k =
      fun k ->
        match exprs with
        | [] -> k []
        | expr :: exprs ->
            let@ expr = go_named_expr env name expr in
            let@ exprs = go_named_exprs env name exprs in
            k (expr :: exprs)
    in

    let expr_ty = Env.type_of expr env in
    go_expr env expr (fun expr -> Anf.Expr.Return expr, expr_ty) |> fst

  let translate_item (item_tys : Core.Item_name.t -> Core.Ty.t) (item : Core.Item.t) : Anf.Item.t =
    let env = Env.empty ~item_tys in

    match item with
    | Core.Item.Val (ty, def) ->
        Anf.Item.Val (ty, translate_expr env def)

    | Core.Item.Fun (params, ty, body) ->
        let params = params |> Iarray.map (fun (name, ty) -> fresh_local_id name, ty) in
        let env = Iarray.fold_right Env.extend params env in
        Anf.Item.Fun (params, ty, translate_expr env body)

  let translate_module (mod_ : Core.Module.t) : Anf.Module.t =
    mod_ |> Core.Item_map.map (translate_item (Core.Module.item_ty mod_))

end

include Translate
