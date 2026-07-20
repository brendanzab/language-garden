(** Bind the result of each computation to intermediate let bindings and hoist
    let expressions to the top of expressions.

    Join points are used to avoid the exponential code bloat that would
    otherwise result from duplicating the continuation in each branch of
    conditionals during hoisting.
*)

module Translate : sig

  val translate_module : Core.Module.t -> Anf.Module.t

end = struct

  module Local_supply = Name.Supply (Anf.Local_id)
  module Join_supply = Name.Supply (Anf.Join_id)

  type 'a k = 'a -> Anf.Expr.t

  let translate_expr
    ~(fresh_local_id : string -> Anf.Local_id.t)
    (local_ids : Anf.Local_id.t Core.Local.Env.t)
    (expr : Core.Expr.t)
  : Anf.Expr.t =
    let ( let@ ) = ( @@ ) in

    let fresh_join_id = Join_supply.(fresh (create ())) in

    let rec go_expr local_ids (expr : Core.Expr.t) : Anf.Expr.comp k k =
      fun k ->
        match expr with
        | Core.Expr.Item (id, None, ty) ->
            k (Anf.Expr.Item (id, None, ty))

        | Core.Expr.Item (id, Some args, ty) ->
            let@ args = go_named_exprs local_ids "arg" (Iarray.to_list args) in
            k (Anf.Expr.Item (id, Some (Iarray.of_list args), ty))

        | Core.Expr.Var (index, ty) ->
            k (Anf.Expr.Atom (Var (Core.Local.Env.lookup index local_ids, ty)))

        | Core.Expr.Let ((name, def_ty, def), body) ->
            let id = fresh_local_id (Option.value name ~default:"_") in
            let@ def = go_expr local_ids def in
            let body = go_expr (Core.Local.Env.extend id local_ids) body k in
            Anf.Expr.Let (id, def_ty, def, body)

        | Core.Expr.Bool bool ->
            k (Anf.Expr.Atom (Bool bool))

        | Core.Expr.Bool_if (expr1, expr2, expr3) ->
            let result_id = fresh_local_id "result" in
            let result_ty = Core.Expr.ty_of expr in

            let join_id = fresh_join_id "if_end" in
            let jump_k result = Anf.Expr.Jump (join_id, result) in

            let@ expr1 = go_named_expr local_ids "cond" expr1 in
            Anf.Expr.Join (join_id, (result_id, result_ty),
              k (Anf.Expr.Atom (Var (result_id, result_ty))),
              Anf.Expr.Bool_if (expr1,
                go_named_expr local_ids "result" expr2 jump_k,
                go_named_expr local_ids "result" expr3 jump_k))

        | Core.Expr.I32 int ->
            k (Anf.Expr.Atom (I32 int))

        | Core.Expr.Prim (op, args) ->
            let@ args = go_named_exprs local_ids "arg" (Iarray.to_list args) in
            k (Anf.Expr.Prim (op, Iarray.of_list args))

    and go_named_expr local_ids (name : string) (expr : Core.Expr.t) : Anf.Expr.atom k k =
      fun k ->
        let expr_ty = Core.Expr.ty_of expr in
        let@ expr = go_expr local_ids expr in
        match expr with
        (* Don't bother binding definitions for atomic computations *)
        | Anf.Expr.Atom expr -> k expr
        (* Bind definitions for non-atomic computations *)
        | expr ->
            let id = fresh_local_id name in
            let body = k (Anf.Expr.Var (id, expr_ty)) in
            Anf.Expr.Let (id, expr_ty, expr, body)

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
    let fresh_local_id = Local_supply.(fresh (create ())) in

    match item with
    | Core.Item.Val (ty, def) ->
        Anf.Item.Val (ty, translate_expr Core.Local.Env.empty def ~fresh_local_id)

    | Core.Item.Fun (params, ty, body) ->
        let param_id name = fresh_local_id (Option.value name ~default:"_") in
        let params = params |> Iarray.map (Pair.map_fst param_id) in
        let local_env = Iarray.to_seq params |> Seq.map Pair.fst |> Core.Local.Env.of_seq in
        Anf.Item.Fun (params, ty, translate_expr local_env body ~fresh_local_id)

  let translate_module (mod_ : Core.Module.t) : Anf.Module.t =
    mod_ |> Core.Item_map.map translate_item

end

include Translate
