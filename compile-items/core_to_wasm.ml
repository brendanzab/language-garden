(** Translation from the core language to the Web Assembly Text Format (WAT). *)

module Local_supply = Name.Supply (Wasm.Local_id)
module Func_supply = Name.Supply (Wasm.Func_id)

(* NOTE: Replace with [Dynarray.to_iarray] when moving to OCaml 5.5.
    See: https://github.com/ocaml/ocaml/pull/14693 *)
let make_iarray xs =
  Iarray.init (Dynarray.length xs) (Dynarray.get xs)

let translate_ty (ty : Core.Ty.t) : Wasm.value_type =
  match ty with
  | Core.Ty.Bool -> Wasm.I32
  | Core.Ty.I32 -> Wasm.I32

let translate_expr
  ~(enable_tail_call : bool)
  ~(fresh_local_id : string -> Wasm.Local_id.t)
  (item_env : Wasm.Func_id.t Core.Item_map.t)
  (local_env : Wasm.Local_id.t Core.Local.Env.t)
  (expr : Core.Expr.t)
: Wasm.local Iarray.t * Wasm.expr =
  let locals = Dynarray.create () in
  let instrs = Dynarray.create () in

  let rec go_expr ~tail_call instrs local_env expr =
    match expr with
    | Core.Expr.Item (name, args, _) ->
        Option.value args ~default:[||] |> Iarray.iter (go_expr instrs local_env ~tail_call:false);
        begin match enable_tail_call && tail_call with
        | true -> Dynarray.add_last instrs (Wasm.Return_call (Core.Item_map.find name item_env));
        | false -> Dynarray.add_last instrs (Wasm.Call (Core.Item_map.find name item_env));
        end

    | Core.Expr.Var (index, _) ->
        Dynarray.add_last instrs (Wasm.Local_get (Core.Local.Env.lookup index local_env));

    | Core.Expr.Let ((name, ty, def), body) ->
        let def_id = fresh_local_id (Option.value name ~default:"_") in
        Dynarray.add_last locals (def_id, translate_ty ty);
        go_expr instrs local_env def ~tail_call:false;
        Dynarray.add_last instrs (Wasm.Local_set def_id);
        go_expr instrs (Core.Local.Env.extend def_id local_env) body ~tail_call;

    | Core.Expr.Bool true -> Dynarray.add_last instrs (Wasm.I32_const 1l);
    | Core.Expr.Bool false -> Dynarray.add_last instrs (Wasm.I32_const 0l);

    | Core.Expr.Bool_if (expr1, expr2, expr3) ->
        let instrs2 = Dynarray.create () in
        let instrs3 = Dynarray.create () in

        go_expr instrs ~tail_call:false local_env expr1;
        go_expr instrs2 local_env expr2 ~tail_call;
        go_expr instrs3 local_env expr3 ~tail_call;

        Dynarray.add_last instrs (Wasm.If (
          translate_ty (Core.Expr.ty_of expr),
          make_iarray instrs2,
          make_iarray instrs3
        ));

    | Core.Expr.I32 i -> Dynarray.add_last instrs (Wasm.I32_const i)

    | Core.Expr.Prim (op, args) ->
        begin match op with
        | Prim.Op.I32_neg -> Dynarray.add_last instrs (Wasm.I32_const 0l);
        | _ -> ()
        end;
        args |> Iarray.iter (go_expr instrs local_env ~tail_call:false);
        begin match op with
        | Prim.Op.Bool_eq -> Dynarray.add_last instrs Wasm.I32_eq;
        | Prim.Op.I32_eq -> Dynarray.add_last instrs Wasm.I32_eq;
        | Prim.Op.I32_add -> Dynarray.add_last instrs Wasm.I32_add;
        | Prim.Op.I32_sub -> Dynarray.add_last instrs Wasm.I32_sub;
        | Prim.Op.I32_mul -> Dynarray.add_last instrs Wasm.I32_mul;
        | Prim.Op.I32_neg -> Dynarray.add_last instrs Wasm.I32_sub;
        end;
  in

  go_expr instrs local_env expr ~tail_call:enable_tail_call;

  (make_iarray locals), make_iarray instrs

let translate_module ~(enable_tail_call : bool) (mod_ : Core.Module.t) : Wasm.module_ =
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
    let fresh_local_id = Local_supply.(fresh (create ())) in
    let translate_expr = translate_expr item_env ~enable_tail_call ~fresh_local_id in

    match Core.Item_map.find name mod_ with
    (** FIXME: re-evaluation of top-level values.

        Possible fixes:
        - normalise expressions (using NbE) and store in global
        - create a global and initialise with a startup function
    *)
    | Core.Item.Val (ty, expr) ->
        let result_ty = translate_ty ty in

        let locals, body = translate_expr Core.Local.Env.empty expr in
        Dynarray.add_last exports (Core.Item_name.to_string name, Wasm.Func id);
        Dynarray.add_last funcs Wasm.{ id; params = [||]; results = [|result_ty|]; locals; body }

    | Core.Item.Fun (params, ty, body) ->
        let param_id name = fresh_local_id (Option.value name ~default:"_") in
        let params = params |> Iarray.map (Pair.map param_id translate_ty) in
        let result_ty = translate_ty ty in

        let local_env = Iarray.to_seq params |> Seq.map Pair.fst |> Core.Local.Env.of_seq in
        let locals, body = translate_expr local_env body in
        Dynarray.add_last exports (Core.Item_name.to_string name, Wasm.Func id);
        Dynarray.add_last funcs Wasm.{ id; params; results = [|result_ty|]; locals; body }
  end;

  Wasm.{
    exports = make_iarray exports;
    funcs = make_iarray funcs;
  }
