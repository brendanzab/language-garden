(** Translation from the core language to the Web Assembly Text Format (WAT). *)

type opts = {
  enable_tail_call : bool;
}

type item_env = (Wat.Func_id.t * Wat.ty) Core.Item_map.t
type local_env = (Wat.Local_id.t * Wat.ty) Core.Local.Env.t

let translate_ty (ty : Core.Ty.t) : Wat.ty =
  match ty with
  | Core.Ty.Bool -> Wat.I32
  | Core.Ty.I32 -> Wat.I32

let fresh_local_id (name : string option) : Wat.Local_id.t =
  Wat.Local_id.fresh (Option.value name ~default:"")

let translate_prim_op (op : Prim.Op.t) : Wat.instr =
  match op with
  | Prim.Op.Bool_eq -> Wat.I32_eq
  | Prim.Op.I32_eq -> Wat.I32_eq
  | Prim.Op.I32_add -> Wat.I32_add
  | Prim.Op.I32_sub -> Wat.I32_sub
  | Prim.Op.I32_mul -> Wat.I32_mul
  | Prim.Op.I32_neg -> Wat.I32_neg

(** Translate an expression in the core language, returning a list of the locals
    defined in the expression along with the translated web assembly expression. *)
let translate_expr
  (opts : opts)
  (items : item_env)
  (locals : local_env)
  (expr : Core.Expr.t)
: (Wat.Local_id.t * Wat.ty) list * Wat.expr =
  (* The locals we've seen while translating the expression *)
  let seen_locals = Dynarray.create () in

  let ( << ) = Fun.compose in

  (* Translate a sub-expression, while updating the list of locals bindings.
     This returns a function that adds a series of instructions to the start of
     an expression, avoiding the exponential complexity cost of repeated calls
     to [List.append]. *)
  let rec go_expr ~tail_call locals expr : (Wat.expr -> Wat.expr) * Wat.ty =
    match expr with
    | Core.Expr.Item (name, args) when opts.enable_tail_call && tail_call ->
        let id, ty = Core.Item_map.find name items in
        let args = Option.value args ~default:[||] in
        go_exprs locals args << List.cons (Wat.Return_call id), ty

    | Core.Expr.Item (name, args) ->
        let id, ty = Core.Item_map.find name items in
        let args = Option.value args ~default:[||] in
        go_exprs locals args << List.cons (Wat.Call id), ty

    | Core.Expr.Var index ->
        let id, ty = Core.Local.Env.lookup index locals in
        List.cons (Wat.Local_get id), ty

    | Core.Expr.Let ((name, def_ty, def), body) ->
        let def_id = fresh_local_id name in
        let def_ty = translate_ty def_ty in

        Dynarray.add_last seen_locals (def_id, def_ty);
        let def, _ = go_expr locals def ~tail_call:false in
        let body, ty =
          go_expr (Core.Local.Env.extend (def_id, def_ty) locals) body ~tail_call
        in

        def << List.cons (Wat.Local_set def_id) << body, ty

    | Core.Expr.Bool true -> List.cons (Wat.I32_const 1l), Wat.I32
    | Core.Expr.Bool false -> List.cons (Wat.I32_const 0l), Wat.I32

    | Core.Expr.Bool_if (expr1, expr2, expr3) ->
        let expr1, _ = go_expr locals expr1 ~tail_call:false in
        let expr2, ty = go_expr locals expr2 ~tail_call in
        let expr3, ty = go_expr locals expr3 ~tail_call in
        expr1 << List.cons (Wat.If (ty, expr2 [], expr3 [])), ty

    | Core.Expr.I32 i -> List.cons (Wat.I32_const i), Wat.I32

    | Core.Expr.Prim (op, args) ->
        go_exprs locals args << List.cons (translate_prim_op op),
        translate_ty (Prim.Op.ty op |> snd)

  (* Translate a series of expressions. This is useful for translating a series
     of function arguments. *)
  and go_exprs locals exprs : Wat.expr -> Wat.expr =
    Iarray.fold_right (fun expr -> go_expr locals expr ~tail_call:false |> fst) exprs
  in

  let expr, _ = go_expr locals expr ~tail_call:true in
  Dynarray.to_list seen_locals, expr []

let translate_fun
  (opts : opts)
  (items : item_env)
  (name : Core.Item_name.t)
  (params : (string option * Prim.Ty.t) Iarray.t)
  (ty : Prim.Ty.t)
  (body : Core.Expr.t)
: Wat.func =
  let export = Core.Item_name.to_string name
  and name, _ = Core.Item_map.find name items
  and result = translate_ty ty

  and params =
    Iarray.to_seq params
    |> Seq.map (Pair.map fresh_local_id translate_ty)
    |> List.of_seq
  in

  let locals, body =
    let locals = List.to_seq params |> Core.Local.Env.of_seq in
    translate_expr opts items locals body
  in

  Wat.{ name; export; params; result; locals; body }

let translate_module ~(enable_tail_call : bool) (mod_ : Core.Module.t) : Wat.module_ =
  let opts = { enable_tail_call } in

  let items =
    mod_ |> Core.Item_map.mapi @@ fun id item ->
      let id = Wat.Func_id.fresh (Core.Item_name.to_string id) in
      match item with
      | Core.Item.Val (ty, _) -> id, translate_ty ty
      | Core.Item.Fun (_, ty, _) -> id, translate_ty ty
  in

  let translate_item (name, item) =
    match item with
    (** FIXME: re-evaluation of top-level values.

        Possible fixes:
        - normalise expressions (using NbE) and store in global
        - create a global and initialise with a startup function
    *)
    | Core.Item.Val (ty, expr) -> translate_fun opts items name [||] ty expr
    | Core.Item.Fun (params, ty, body) -> translate_fun opts items name params ty body
  in

  Wat.{
    funcs =
      Core.Item_map.to_seq mod_
      |> Seq.map translate_item
      |> List.of_seq;
  }
