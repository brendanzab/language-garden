(** Translation from the core language to the Web Assembly Text Format (WAT). *)

module Module_ctx = Wat.Emit.Module_ctx
module Expr_ctx = Wat.Emit.Expr_ctx

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
  (ctx : 'a Expr_ctx.t)
  (item_env : Wat.Func_id.t Core.Item_map.t)
  (local_env : Wat.Local_id.t Core.Local.Env.t)
  (expr : Core.Expr.t)
: unit =
  let rec go : 'a. tail_call:bool -> 'a Expr_ctx.t -> Wat.Local_id.t Core.Local.Env.t -> Core.Expr.t -> unit =
    fun ~tail_call ctx local_env expr ->
      match expr with
      | Core.Expr.Item (name, args) when enable_tail_call && tail_call ->
          Option.value args ~default:[||] |> Iarray.iter (go ctx local_env ~tail_call:false);
          Expr_ctx.emit_instr ctx (Wat.Return_call (Core.Item_map.find name item_env));

      | Core.Expr.Item (name, args) ->
          Option.value args ~default:[||] |> Iarray.iter (go ctx local_env ~tail_call:false);
          Expr_ctx.emit_instr ctx (Wat.Call (Core.Item_map.find name item_env));

      | Core.Expr.Var index ->
          Expr_ctx.emit_instr ctx (Wat.Local_get (Core.Local.Env.lookup index local_env));

      | Core.Expr.Let ((name, ty, def), body) ->
          let def_id = Expr_ctx.emit_local ctx (Option.value name ~default:"") (translate_ty ty) in
          go ctx local_env def ~tail_call:false;
          Expr_ctx.emit_instr ctx (Wat.Local_set def_id);
          go ctx (Core.Local.Env.extend def_id local_env) body ~tail_call

      | Core.Expr.Bool true -> Expr_ctx.emit_instr ctx (Wat.I32_const 1l)
      | Core.Expr.Bool false -> Expr_ctx.emit_instr ctx (Wat.I32_const 0l)

      | Core.Expr.Bool_if (expr1, expr2, expr3, ty) ->
          go ctx ~tail_call:false local_env expr1;
          Expr_ctx.emit_instr ctx (Wat.If (
            translate_ty ty,
            Expr_ctx.emit_expr ctx { run = fun (type a) (ctx : a Expr_ctx.t) ->
              go ctx local_env expr2 ~tail_call },
            Expr_ctx.emit_expr ctx { run = fun (type a) (ctx : a Expr_ctx.t) ->
              go ctx local_env expr3 ~tail_call }
          ));

      | Core.Expr.I32 i -> Expr_ctx.emit_instr ctx (Wat.I32_const i)

      | Core.Expr.Prim (op, args) ->
          args |> Iarray.iter (go ctx local_env ~tail_call:false);
          Expr_ctx.emit_instr ctx (translate_prim_op op);
  in

  go ctx local_env expr ~tail_call:true

let translate_module ~(enable_tail_call : bool) (mod_ : Core.Module.t) : Wat.module_ =
  Module_ctx.build {
    run = fun (type a) (ctx : a Module_ctx.t) : unit ->
      let funcs =
        mod_ |> Core.Item_map.mapi @@ fun id _ ->
          Module_ctx.emit_func_deferred ctx (Core.Item_name.to_string id)
      in
      let item_env = Core.Item_map.map Pair.fst funcs in

      funcs |> Core.Item_map.iter begin fun name (id, define_func) ->
        match Core.Item_map.find name mod_ with
        | Core.Item.Val (ty, expr) ->
            (** FIXME: re-evaluation of top-level values.

                Possible fixes:
                - normalise expressions (using NbE) and store in global
                - create a global and initialise with a startup function
            *)
            Module_ctx.emit_export ctx (Core.Item_name.to_string name) (Func id);
            define_func ctx (translate_ty ty) Expr_ctx.{
              run = fun (type a) (ctx : a Expr_ctx.t) ->
                translate_expr ctx item_env Core.Local.Env.empty expr
                  ~enable_tail_call
            }
        | Core.Item.Fun (params, ty, body) ->
            Module_ctx.emit_export ctx (Core.Item_name.to_string name) (Func id);
            define_func ctx (translate_ty ty) Expr_ctx.{
              run = fun (type a) (ctx : a Expr_ctx.t) ->
                let compile_param (name, ty) =
                  Expr_ctx.emit_param ctx (Option.value name ~default:"") (translate_ty ty)
                in
                let local_env =
                  Iarray.to_seq params
                  |> Seq.map compile_param
                  |> Core.Local.Env.of_seq
                in
                translate_expr ctx item_env local_env body
                  ~enable_tail_call
            }
      end;
  }
