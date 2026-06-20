(** Translation from the core language to the Web Assembly Text Format (WAT). *)

module Local_supply = Name.Label.Supply (Wat.Local_id)
module Func_supply = Name.Label.Supply (Wat.Func_id)

(* NOTE: Replace with [Dynarray.to_iarray] when moving to OCaml 5.5.
    See: https://github.com/ocaml/ocaml/pull/14693 *)
let make_iarray xs =
  Iarray.init (Dynarray.length xs) (Dynarray.get xs)

let translate_ty (ty : Core.Ty.t) : Wat.ty =
  match ty with
  | Core.Ty.Bool -> Wat.I32
  | Core.Ty.I32 -> Wat.I32

let translate_prim_op (op : Prim.Op.t) : Wat.instr =
  match op with
  | Prim.Op.Bool_eq -> Wat.I32_eq
  | Prim.Op.I32_eq -> Wat.I32_eq
  | Prim.Op.I32_add -> Wat.I32_add
  | Prim.Op.I32_sub -> Wat.I32_sub
  | Prim.Op.I32_mul -> Wat.I32_mul
  | Prim.Op.I32_neg -> Wat.I32_neg

let translate_expr
  ~(enable_tail_call : bool)
  (fresh_local_id : string option -> Wat.Local_id.t)
  (item_env : Wat.Func_id.t Core.Item_map.t)
  (local_env : Wat.Local_id.t Core.Local.Env.t)
  (expr : Core.Expr.t)
: locals:(Wat.Local_id.t * Wat.ty) Iarray.t * Wat.instr Iarray.t =
  let locals = Dynarray.create () in
  let instrs = Dynarray.create () in

  let rec go ~tail_call instrs local_env expr =
    match expr with
    | Core.Expr.Item (name, args) when enable_tail_call && tail_call ->
        Option.value args ~default:[||] |> Iarray.iter (go instrs local_env ~tail_call:false);
        Dynarray.add_last instrs (Wat.Return_call (Core.Item_map.find name item_env));

    | Core.Expr.Item (name, args) ->
        Option.value args ~default:[||] |> Iarray.iter (go instrs local_env ~tail_call:false);
        Dynarray.add_last instrs (Wat.Call (Core.Item_map.find name item_env));

    | Core.Expr.Var index ->
        Dynarray.add_last instrs (Wat.Local_get (Core.Local.Env.lookup index local_env));

    | Core.Expr.Let ((name, ty, def), body) ->
        let def_id = fresh_local_id name in
        Dynarray.add_last locals (def_id, translate_ty ty);
        go instrs local_env def ~tail_call:false;
        Dynarray.add_last instrs (Wat.Local_set def_id);
        go instrs (Core.Local.Env.extend def_id local_env) body ~tail_call

    | Core.Expr.Bool true -> Dynarray.add_last instrs (Wat.I32_const 1l)
    | Core.Expr.Bool false -> Dynarray.add_last instrs (Wat.I32_const 0l)

    | Core.Expr.Bool_if (expr1, expr2, expr3, ty) ->
        let instrs2 = Dynarray.create () in
        let instrs3 = Dynarray.create () in

        go instrs ~tail_call:false local_env expr1;
        go instrs2 local_env expr2 ~tail_call;
        go instrs3 local_env expr3 ~tail_call;

        Dynarray.add_last instrs (Wat.If (
          translate_ty ty,
          make_iarray instrs2,
          make_iarray instrs3
        ));

    | Core.Expr.I32 i -> Dynarray.add_last instrs (Wat.I32_const i)

    | Core.Expr.Prim (op, args) ->
        args |> Iarray.iter (go instrs local_env ~tail_call:false);
        Dynarray.add_last instrs (translate_prim_op op);
  in

  go instrs local_env expr ~tail_call:enable_tail_call;

  ~locals:(make_iarray locals), make_iarray instrs

let translate_module ~(enable_tail_call : bool) (mod_ : Core.Module.t) : Wat.module_ =
  (* Arrays to store exports and functions *)
  let exports = Dynarray.create () in
  let funcs = Dynarray.create () in

  (* Generate function ids for each item *)
  let item_env =
    let fresh_func_id = Func_supply.(fresh (create ())) in
    mod_ |> Core.Item_map.mapi @@ fun name _ ->
      fresh_func_id (Core.Item_name.to_string name)
  in

  item_env |> Core.Item_map.iter begin fun name id ->
    let fresh_local_id =
      Option.value ~default:""
      |> Fun.compose Local_supply.(fresh (create ()))
    in

    match Core.Item_map.find name mod_ with
    (** FIXME: re-evaluation of top-level values.

        Possible fixes:
        - normalise expressions (using NbE) and store in global
        - create a global and initialise with a startup function
    *)
    | Core.Item.Val (ty, expr) ->
        let result = translate_ty ty in

        let ~locals, body =
          translate_expr fresh_local_id item_env Core.Local.Env.empty expr
            ~enable_tail_call
        in
        Dynarray.add_last exports (Core.Item_name.to_string name, Wat.Func id);
        Dynarray.add_last funcs Wat.{ id; params = [||]; result; locals; body }

    | Core.Item.Fun (params, ty, body) ->
        let params = params |> Iarray.map (Pair.map fresh_local_id translate_ty) in
        let result = translate_ty ty in

        let ~locals, body =
          let local_env = Iarray.to_seq params |> Seq.map Pair.fst |> Core.Local.Env.of_seq in
          translate_expr fresh_local_id item_env local_env body
            ~enable_tail_call
        in
        Dynarray.add_last exports (Core.Item_name.to_string name, Wat.Func id);
        Dynarray.add_last funcs Wat.{ id; params; result; locals; body }
  end;

  Wat.{
    exports = make_iarray exports;
    funcs = make_iarray funcs;
  }
