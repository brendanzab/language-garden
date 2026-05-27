(** Translation from the core language to the Web Assembly Text Format (WAT). *)

type opts = {
  enable_tail_call : bool;
}

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
  (item_ids : Wat.Func_id.t Core.Item_map.t)
  (local_ids : Wat.Local_id.t Core.Local.Env.t)
  (expr : Core.Expr.t)
: (Wat.Local_id.t * Wat.ty) list * Wat.expr =
  (* The locals we've seen while translating the expression *)
  let seen_locals = Dynarray.create () in

  let ( << ) = Fun.compose in

  (* Translate a sub-expression, while updating the list of locals bindings.
     This returns a function that adds a series of instructions to the start of
     an expression, avoiding the exponential complexity cost of repeated calls
     to [List.append]. *)
  let rec go_expr ~tail_call local_ids expr : Wat.expr -> Wat.expr =
    match expr with
    | Core.Expr.Item (name, args) when opts.enable_tail_call && tail_call ->
        go_exprs local_ids (Option.value args ~default:[||])
          << List.cons (Wat.Return_call (Core.Item_map.find name item_ids))

    | Core.Expr.Item (name, args) ->
        go_exprs local_ids (Option.value args ~default:[||])
          << List.cons (Wat.Call (Core.Item_map.find name item_ids))

    | Core.Expr.Var index ->
        List.cons (Wat.Local_get (Core.Local.Env.lookup index local_ids))

    | Core.Expr.Let ((name, ty, def), body) ->
        let def_id = fresh_local_id name in
        Dynarray.add_last seen_locals (def_id, translate_ty ty);
        go_expr ~tail_call:false local_ids def
          << List.cons (Wat.Local_set def_id)
          << go_expr ~tail_call (Core.Local.Env.extend def_id local_ids) body

    | Core.Expr.Bool true -> List.cons (Wat.I32_const 1l)
    | Core.Expr.Bool false -> List.cons (Wat.I32_const 0l)

    | Core.Expr.Bool_if (expr1, expr2, expr3, ty) ->
        go_expr ~tail_call:false local_ids expr1
          << List.cons (Wat.If (
            translate_ty ty,
            go_expr ~tail_call local_ids expr2 [],
            go_expr ~tail_call local_ids expr3 []
          ))

    | Core.Expr.I32 i -> List.cons (Wat.I32_const i)

    | Core.Expr.Prim (op, args) ->
        go_exprs local_ids args
          << List.cons (translate_prim_op op)

  (* Translate a series of expressions. This is useful for translating a series
     of function arguments. *)
  and go_exprs local_ids exprs : Wat.expr -> Wat.expr =
    Iarray.fold_right (go_expr ~tail_call:false local_ids) exprs
  in

  let expr = go_expr ~tail_call:true local_ids expr [] in
  Dynarray.to_list seen_locals, expr

let translate_fun
  (opts : opts)
  (item_ids : Wat.Func_id.t Core.Item_map.t)
  (name : Core.Item_name.t)
  (params : (string option * Prim.Ty.t) Iarray.t)
  (ty : Prim.Ty.t)
  (body : Core.Expr.t)
: Wat.func =
  let export = Core.Item_name.to_string name
  and name = Core.Item_map.find name item_ids
  and result = translate_ty ty

  and params =
    Iarray.to_seq params
    |> Seq.map (fun (name, ty) -> fresh_local_id name, translate_ty ty)
    |> List.of_seq
  in

  let locals, body =
    let local_ids = List.to_seq params |> Seq.map fst |> Core.Local.Env.of_seq in
    translate_expr opts item_ids local_ids body
  in

  Wat.{ name; export; params; result; locals; body }

let translate_module ~(enable_tail_call : bool) (mod_ : Core.Module.t) : Wat.module_ =
  let opts = { enable_tail_call } in

  let item_ids =
    mod_ |> Core.Item_map.mapi @@ fun id _ ->
      Wat.Func_id.fresh (Core.Item_name.to_string id)
  in

  let translate_item (name, item) =
    match item with
    (** FIXME: re-evaluation of top-level values.

        Possible fixes:
        - normalise expressions (using NbE) and store in global
        - create a global and initialise with a startup function
    *)
    | Core.Item.Val (ty, expr) -> translate_fun opts item_ids name [||] ty expr
    | Core.Item.Fun (params, ty, body) -> translate_fun opts item_ids name params ty body
  in

  Wat.{
    funcs =
      Core.Item_map.to_seq mod_
      |> Seq.map translate_item
      |> List.of_seq;
  }
