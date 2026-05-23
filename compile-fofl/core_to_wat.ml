let translate_ty (ty : Core.Ty.t) : Wat.ty =
  match ty with
  | Core.Ty.Bool -> Wat.I32
  | Core.Ty.I32 -> Wat.I32

let fresh_local_id (name : string option) : Wat.Local_id.t =
  Wat.Local_id.fresh (Option.value name ~default:"")

let ( << ) = Fun.compose

(** Translate an expression in the core language to a web assembly expression.
    A list of locals used in the expression is returned as well. *)
let translate_expr
  ~(tail_call : bool)
  (items : Wat.Func_id.t Core.Item_map.t)
  (params : Wat.Local_id.t Core.Local.Env.t)
  (expr : Core.Expr.t)
: (Wat.Local_id.t * Wat.ty) list * Wat.expr =
  (* The locals we've seen while translating the expression *)
  let seen_locals = Dynarray.create () in

  let rec go_expr ~tail_call locals expr : Wat.expr -> Wat.expr =
    match expr with
    | Core.Expr.Item (name, args) when tail_call ->
        go_exprs locals (Option.value args ~default:[||])
          << List.cons (Wat.Return_call (Core.Item_map.find name items))

    | Core.Expr.Item (name, args) ->
        go_exprs locals (Option.value args ~default:[||])
          << List.cons (Wat.Call (Core.Item_map.find name items))

    | Core.Expr.Var index ->
        List.cons (Wat.Local_get (Core.Local.Env.lookup index locals))

    | Core.Expr.Let ((name, ty, def), body) ->
        let def_id = fresh_local_id name in
        Dynarray.add_last seen_locals (def_id, translate_ty ty);
        go_expr ~tail_call:false locals def
          << List.cons (Wat.Local_set def_id)
          << go_expr ~tail_call (Core.Local.Env.extend def_id locals) body

    | Core.Expr.Bool true -> List.cons (Wat.I32_const 1l)
    | Core.Expr.Bool false -> List.cons (Wat.I32_const 0l)

    | Core.Expr.Bool_if (expr1, expr2, expr3, ty) ->
        go_expr ~tail_call:false locals expr1
          << List.cons (Wat.If (
            translate_ty ty,
            go_expr ~tail_call locals expr2 [],
            go_expr ~tail_call locals expr3 []
          ))

    | Core.Expr.I32 i -> List.cons (Wat.I32_const i)

    | Core.Expr.Prim (Bool_eq, args) -> go_exprs locals args << List.cons Wat.I32_eq
    | Core.Expr.Prim (I32_eq, args) -> go_exprs locals args << List.cons Wat.I32_eq
    | Core.Expr.Prim (I32_add, args) -> go_exprs locals args << List.cons Wat.I32_add
    | Core.Expr.Prim (I32_sub, args) -> go_exprs locals args << List.cons Wat.I32_sub
    | Core.Expr.Prim (I32_mul, args) -> go_exprs locals args << List.cons Wat.I32_mul
    | Core.Expr.Prim (I32_neg, args) -> go_exprs locals args << List.cons Wat.I32_neg

  and go_exprs locals exprs : Wat.expr -> Wat.expr =
    Iarray.fold_right (go_expr ~tail_call:false locals) exprs
  in

  let expr = go_expr ~tail_call params expr [] in
  Dynarray.to_list seen_locals, expr

let translate_fun
  ~(tail_call : bool)
  (items : Wat.Func_id.t Core.Item_map.t)
  (name : Core.Item_name.t)
  (params : (string option * Prim.Ty.t) Iarray.t)
  (ty : Prim.Ty.t)
  (body : Core.Expr.t)
: Wat.func =
  let export = Core.Item_name.to_string name
  and name = Core.Item_map.find name items
  and result = translate_ty ty
  and params =
    Iarray.to_seq params
    |> Seq.map (fun (name, ty) -> fresh_local_id name, translate_ty ty)
    |> List.of_seq
  in

  let locals, body =
    let param_ids = List.to_seq params |> Seq.map fst |> Core.Local.Env.of_seq in
    translate_expr ~tail_call items param_ids body
  in

  Wat.{ name; export; params; result; locals; body }

let translate_item
  ~(tail_call : bool)
  (items : Wat.Func_id.t Core.Item_map.t)
  (name, item : Core.(Item_name.t * Item.t)) : Wat.func =
  match item with
  (** FIXME: re-evaluation of top-level values.

      Possible fixes:
      - normalise expressions and store in global
      - store global with a temporary initializer. initialize on startup
  *)
  | Core.Item.Val (ty, expr) -> translate_fun ~tail_call items name [||] ty expr
  | Core.Item.Fun (params, ty, body) -> translate_fun ~tail_call items name params ty body

let translate_module ~(tail_call : bool) (mod_ : Core.Module.t) : Wat.module_ =
  let items =
    mod_ |> Core.Item_map.mapi @@ fun id _ ->
      Wat.Func_id.fresh (Core.Item_name.to_string id)
  in

  Wat.{
    items =
      Core.Item_map.to_seq mod_
      |> Seq.map (translate_item ~tail_call items)
      |> List.of_seq;
  }
