(** Hoist let expressions and conditionals to the top of expressions. *)

module Hoist : sig

  val translate_expr : Hoisted.Local_name.t Core.Local.Env.t -> Core.Expr.t -> Hoisted.Expr.t
  val translate_program : Core.Program.t -> Hoisted.Program.t

end = struct

  module C = Core
  module H = Hoisted

  let translate_expr (env : H.Local_name.t C.Local.Env.t) (expr : C.Expr.t) : H.Expr.t =
    let ( let@ ) = ( @@ ) in

    let rec go_expr (env : H.Local_name.t C.Local.Env.t) (expr : C.Expr.t) (k : H.Expr.comp -> H.Expr.t) : H.Expr.t =
      match expr with
      | C.Expr.Item (name, None) ->
          k (H.Expr.Item (name, None))
      | C.Expr.Item (name, Some args) ->
          let@ args = go_exprs env (Iarray.to_list args) in
          k (H.Expr.Item (name, Some (Iarray.of_list args)))
      | C.Expr.Var index ->
          k (H.Expr.Var (C.Local.Env.lookup index env))
      | C.Expr.Let (name, def_ty, def, body) ->
          let name = H.Local_name.fresh (Option.value name ~default:"") in
          let@ def = go_expr env def in
          let body = go_expr (C.Local.Env.extend name env) body k in
          H.Expr.Let (name, def_ty, def, body)
      | C.Expr.Bool bool ->
          k (H.Expr.Bool bool)
      | C.Expr.Bool_if (expr1, expr2, expr3) ->
          let@ expr1 = go_expr env expr1 in
          let expr2 = go_expr env expr2 k in (* FIXME: join point *)
          let expr3 = go_expr env expr3 k in (* FIXME: join point *)
          H.Expr.Bool_if (expr1, expr2, expr3)
      | C.Expr.I32 int ->
          k (H.Expr.I32 int)
      | C.Expr.Prim (op, args) ->
          let@ args = go_exprs env (Iarray.to_list args) in
          k (H.Expr.Prim (op, Iarray.of_list args))

    (* Compile a series of expressions to intermediate definitions *)
    and go_exprs (env : H.Local_name.t C.Local.Env.t) (exprs : C.Expr.t list) (k : H.Expr.comp list -> H.Expr.t) : H.Expr.t =
      match exprs with
      | [] -> k []
      | expr :: exprs ->
          let@ expr = go_expr env expr in
          let@ exprs = go_exprs env exprs in
          k (expr :: exprs)
    in

    let@ expr = go_expr env expr in
    H.Expr.Comp expr

  let translate_item (item : C.Item.t) : H.Item.t =
    match item with
    | C.Item.Val (ty, def) ->
        H.Item.Val (ty, translate_expr C.Local.Env.empty def)
    | C.Item.Fun (params, ty, body) ->
        let params =
          params |> Iarray.map @@ fun (name, ty) ->
            H.Local_name.fresh (Option.value name ~default:""), ty
        in
        let env = Iarray.to_seq params |> Seq.map fst |> C.Local.Env.of_seq in
        H.Item.Fun (params, ty, translate_expr env body)

  let translate_program (program : C.Program.t) : H.Program.t =
    program |> C.Item_map.map translate_item

end

include Hoist
