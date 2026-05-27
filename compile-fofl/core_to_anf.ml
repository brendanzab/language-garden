(** Bind the result of each computation to intermediate let bindings and hoist
    let expressions to the top of expressions.

    Join points are used to avoid the exponential code bloat that would
    otherwise result from duplicating the continuation in each branch of
    conditionals during hoisting.
*)

module Translate : sig

  val translate_module : Core.Module.t -> Anf.Module.t

end = struct

  type 'a k = 'a -> Anf.Expr.t

  let fresh_local_id (name : string option) : Anf.Local_id.t =
    Anf.Local_id.fresh (Option.value name ~default:"")

  let translate_expr (local_ids : Anf.Local_id.t Core.Local.Env.t) (expr : Core.Expr.t) : Anf.Expr.t =
    let ( let@ ) = ( @@ ) in

    let rec go_expr local_ids (expr : Core.Expr.t) : Anf.Expr.comp k k =
      fun k ->
        match expr with
        | Core.Expr.Item (id, None) ->
            k (Anf.Expr.Item (id, None))

        | Core.Expr.Item (id, Some args) ->
            let@ args = go_named_exprs local_ids "arg" (Iarray.to_list args) in
            k (Anf.Expr.Item (id, Some (Iarray.of_list args)))

        | Core.Expr.Var index ->
            k (Anf.Expr.Atom (Var (Core.Local.Env.lookup index local_ids)))

        | Core.Expr.Let ((name, def_ty, def), body) ->
            let id = fresh_local_id name in
            let@ def = go_expr local_ids def in
            let body = go_expr (Core.Local.Env.extend id local_ids) body k in
            Anf.Expr.Let (id, Some def_ty, def, body)

        | Core.Expr.Bool bool ->
            k (Anf.Expr.Atom (Bool bool))

        | Core.Expr.Bool_if (expr1, expr2, expr3, expr_ty) ->
            let@ expr1 = go_named_expr local_ids "cond" expr1 in

            let param_id = Anf.Local_id.fresh "p" in
            let cont = k (Anf.Expr.Atom (Var param_id)) in

            let join_id = Anf.Join_id.fresh "j" in
            let jump_k x = Anf.Expr.Jump (join_id, x) in

            let expr2 = go_named_expr local_ids "result" expr2 jump_k in
            let expr3 = go_named_expr local_ids "result" expr3 jump_k in

            Anf.Expr.Join (join_id, (param_id, expr_ty), cont,
              Anf.Expr.Bool_if (expr1, expr2, expr3))

        | Core.Expr.I32 int ->
            k (Anf.Expr.Atom (I32 int))

        | Core.Expr.Prim (op, args) ->
            let@ args = go_named_exprs local_ids "arg" (Iarray.to_list args) in
            k (Anf.Expr.Prim (op, Iarray.of_list args))

    and go_named_expr local_ids (name : string) (expr : Core.Expr.t) : Anf.Expr.atom k k =
      fun k ->
        let@ expr = go_expr local_ids expr in
        match expr with
        (* Don't bother binding definitions for atomic computations *)
        | Anf.Expr.Atom expr -> k expr
        (* Bind definitions for non-atomic computations *)
        | expr ->
            let id = Anf.Local_id.fresh name in
            let body = k (Anf.Expr.Var id) in
            Anf.Expr.Let (id, None, expr, body)

    (* Compile a series of expressions to intermediate definitions *)
    and go_named_exprs local_ids (name : string) (exprs : Core.Expr.t list) : Anf.Expr.atom list k k =
      fun k ->
        match exprs with
        | [] -> k []
        | expr :: exprs ->
            let@ expr = go_named_expr local_ids name expr in
            let@ exprs = go_named_exprs local_ids name exprs in
            k (expr :: exprs)
    in

    let@ expr = go_expr local_ids expr in
    Anf.Expr.Return expr

  let translate_item (item : Core.Item.t) : Anf.Item.t =
    let env = Core.Local.Env.empty in

    match item with
    | Core.Item.Val (ty, def) ->
        Anf.Item.Val (ty, translate_expr env def)

    | Core.Item.Fun (params, ty, body) ->
        let params = params |> Iarray.map (fun (name, ty) -> fresh_local_id name, ty) in
        let env = Iarray.fold_right (fun (id, _) -> Core.Local.Env.extend id) params env in
        Anf.Item.Fun (params, ty, translate_expr env body)

  let translate_module (mod_ : Core.Module.t) : Anf.Module.t =
    mod_ |> Core.Item_map.map translate_item

end

include Translate
