(** Hoist let expressions and conditionals to the top of expressions. *)

module Translate : sig

  val translate_expr : Anf.Local_id.t Core.Local.Env.t -> Core.Expr.t -> Anf.Expr.t
  val translate_module : Core.Module.t -> Anf.Module.t

end = struct

  type local_ids = Anf.Local_id.t Core.Local.Env.t

  type 'a k = 'a -> Anf.Expr.t

  let fresh_local_id (name : string option) : Anf.Local_id.t =
    Anf.Local_id.fresh (Option.value name ~default:"")

  let translate_expr (locals : local_ids) (expr : Core.Expr.t) : Anf.Expr.t =
    let ( let@ ) = ( @@ ) in

    let rec go_expr (locals : local_ids) (expr : Core.Expr.t) : Anf.Expr.comp k k =
      fun k ->
        match expr.data with
        | Core.Expr.Item (id, None) ->
            k (Anf.Expr.Item (id, None))

        | Core.Expr.Item (id, Some args) ->
            let@ args = go_named_exprs locals "arg" (Iarray.to_list args) in
            k (Anf.Expr.Item (id, Some (Iarray.of_list args)))

        | Core.Expr.Var index ->
            k (Anf.Expr.Atom (Var (Core.Local.Env.lookup index locals)))

        | Core.Expr.Let (name, def, body) ->
            let id = fresh_local_id name in
            let@ def = go_expr locals def in
            let body = go_expr (Core.Local.Env.extend id locals) body k in
            Anf.Expr.{ data = Let (id, def, body); ty = body.ty }

        | Core.Expr.Bool bool ->
            k (Anf.Expr.Atom (Bool bool))

        | Core.Expr.Bool_if (expr1, expr2, expr3) ->
            let expr_ty = expr2.ty in
            let@ expr1 = go_named_expr locals "cond" expr1 in

            let param_id = Anf.Local_id.fresh "p" in
            let join_id = Anf.Join_id.fresh "j" in
            let cont = k (Anf.Expr.Atom (Var param_id)) in
            let jump_k x = Anf.Expr.{ data = Jump (join_id, x); ty = cont.ty } in

            let expr2 = go_named_expr locals "result" expr2 jump_k in
            let expr3 = go_named_expr locals "result" expr3 jump_k in
            let if_expr = Anf.Expr.{ data = Anf.Expr.Bool_if (expr1, expr2, expr3); ty = expr2.ty } in

            Anf.Expr.{
              data = Join (join_id, (param_id, expr_ty), cont, if_expr);
              ty = if_expr.ty;
            }

        | Core.Expr.I32 int ->
            k (Anf.Expr.Atom (I32 int))

        | Core.Expr.Prim (op, args) ->
            let@ args = go_named_exprs locals "arg" (Iarray.to_list args) in
            k (Anf.Expr.Prim (op, Iarray.of_list args))

    and go_named_expr (locals : local_ids) (name : string) (expr : Core.Expr.t) : Anf.Expr.atom k k =
      fun k ->
        let@ expr = go_expr locals expr in
        match expr with
        (* Don't bother binding definitions for atomic computations *)
        | Anf.Expr.Atom expr -> k expr
        (* Bind definitions for non-atomic computations *)
        | expr ->
            let id = Anf.Local_id.fresh name in
            let body = k (Anf.Expr.Var id) in
            Anf.Expr.{ data = Let (id, expr, body); ty = body.ty }

    (* Compile a series of expressions to intermediate definitions *)
    and go_named_exprs (locals : local_ids) (name : string) (exprs : Core.Expr.t list) : Anf.Expr.atom list k k =
      fun k ->
        match exprs with
        | [] -> k []
        | expr :: exprs ->
            let@ expr = go_named_expr locals name expr in
            let@ exprs = go_named_exprs locals name exprs in
            k (expr :: exprs)
    in

    let ty = expr.ty in
    let@ expr = go_expr locals expr in
    Anf.Expr.{ data = Return expr; ty }

  let translate_item (item : Core.Item.t) : Anf.Item.t =
    match item with
    | Core.Item.Val (ty, def) ->
        Anf.Item.Val (ty, translate_expr Core.Local.Env.empty def)
    | Core.Item.Fun (params, ty, body) ->
        let params = params |> Iarray.map @@ fun (name, ty) -> fresh_local_id name, ty in
        let local_ids = Iarray.to_seq params |> Seq.map fst |> Core.Local.Env.of_seq in
        Anf.Item.Fun (params, ty, translate_expr local_ids body)

  let translate_module (program : Core.Module.t) : Anf.Module.t =
    program |> Core.Item_map.map translate_item

end

include Translate
