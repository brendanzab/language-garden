(** Translation from the core language to the Web Assembly Text Format (WAT). *)

(** Context for emitting Wasm modules *)
module Ctx : sig

  (** The state of the context, parameterised by the capabilities it supports. *)
  type 'a t

  val create : unit -> [`Module] t

  val build_module : [`Module] t -> Wasm.module_

  val build_func :
    [`Module] t ->
    Wasm.Func_id.t ->
    (string * Wasm.value_type) Iarray.t ->
    Wasm.value_type Iarray.t ->
    ([`Module | `Func] t -> Wasm.Local_id.t Iarray.t -> unit) ->
    Wasm.func

  val build_subexpr : [`Module | `Func] t -> ([`Module | `Func] t -> unit) -> Wasm.expr

  (** Module-level operations *)

  val fresh_type_id : [> `Module] t -> string -> Wasm.Type_id.t
  val fresh_func_id : [> `Module] t -> string -> Wasm.Func_id.t

  val emit_export : [> `Module] t -> Wasm.export -> unit
  val emit_func : [> `Module] t -> Wasm.func -> unit
  val emit_type_def : [> `Module] t -> string -> Wasm.comp_type -> Wasm.Type_id.t
  val emit_func_ref : [> `Module] t -> Wasm.Func_id.t -> unit

  (** Function-level operations *)

  val fresh_local_id : [> `Func] t -> string -> Wasm.Local_id.t

  val emit_local : [> `Func] t -> Wasm.local -> unit
  val emit_instr : [> `Func] t -> Wasm.instr -> unit

end = struct

  module Type_id_supply = Name.Supply (Wasm.Type_id)
  module Func_id_supply = Name.Supply (Wasm.Func_id)
  module Local_id_supply = Name.Supply (Wasm.Local_id)

  type 'a t = {
    type_id_supply : Type_id_supply.t;
    func_id_supply : Func_id_supply.t;
    local_id_supply : Local_id_supply.t;

    (* Array of function references, to be used in a single element declaration *)
    func_refs : Wasm.Func_id.t Dynarray.t;
    (* Map for interning types as unique type ids *)
    type_def_ids : (Wasm.comp_type, Wasm.Type_id.t) Hashtbl.t;

    exports : Wasm.export Dynarray.t;
    types : Wasm.rec_type Dynarray.t;
    elems : Wasm.elem Dynarray.t;
    funcs : Wasm.func Dynarray.t;

    locals : Wasm.local Dynarray.t;
    instrs : Wasm.instr Dynarray.t;
  }

  let create () : [`Module] t = {
    type_id_supply = Type_id_supply.create ();
    func_id_supply = Func_id_supply.create ();
    local_id_supply = Local_id_supply.create ();

    func_refs = Dynarray.create ();
    type_def_ids = Hashtbl.create 0;

    exports = Dynarray.create ();
    types = Dynarray.create ();
    elems = Dynarray.create ();
    funcs = Dynarray.create ();

    locals = Dynarray.create ();
    instrs = Dynarray.create ();
  }

  let fresh_type_id (ctx : [> `Module] t) = Type_id_supply.fresh ctx.type_id_supply
  let fresh_func_id (ctx : [> `Module] t) = Func_id_supply.fresh ctx.func_id_supply
  let fresh_local_id (ctx : [> `Func] t) = Local_id_supply.fresh ctx.local_id_supply

  let emit_type_def (ctx : [> `Module] t) (name : string) (ty : Wasm.comp_type) =
    match Hashtbl.find_opt ctx.type_def_ids ty with
    | Some id -> id
    | None ->
        let id = fresh_type_id ctx name in
        Dynarray.add_last ctx.types (Wasm.Type_def (id, ty));
        Hashtbl.add ctx.type_def_ids ty id;
        id

  let emit_func_ref (ctx : [> `Module] t) id =
    if Dynarray.mem id ctx.func_refs then () else
      Dynarray.add_last ctx.func_refs id

  let emit_export (ctx : [> `Module] t) = Dynarray.add_last ctx.exports
  let emit_func (ctx : [> `Module] t) = Dynarray.add_last ctx.funcs
  let emit_local (ctx : [> `Func] t) = Dynarray.add_last ctx.locals
  let emit_instr (ctx : [> `Func] t) = Dynarray.add_last ctx.instrs

  let take_iarray xs =
    (* NOTE: Use [Dynarray.to_iarray] when moving to OCaml 5.5.
       See: https://github.com/ocaml/ocaml/pull/14693 *)
    let old_xs = Iarray.init (Dynarray.length xs) (Dynarray.get xs) in
    Dynarray.clear xs;
    old_xs

  let build_module (ctx : [`Module] t) =
    (* Construct an element declaration containing all of the function references
       used in the module. *)
    if Dynarray.length ctx.func_refs <> 0 then
      Dynarray.add_last ctx.elems Wasm.(Declare, Func (take_iarray ctx.func_refs));

    Type_id_supply.reset ctx.type_id_supply;
    Func_id_supply.reset ctx.func_id_supply;

    Wasm.{
      exports = take_iarray ctx.exports;
      types = take_iarray ctx.types;
      elems = take_iarray ctx.elems;
      funcs = take_iarray ctx.funcs;
    }

  let build_func
    (ctx : [`Module] t)
    (id : Wasm.Func_id.t)
    (params : (string * Wasm.value_type) Iarray.t)
    (results : Wasm.value_type Iarray.t)
    (f : [`Module | `Func] t -> Wasm.Local_id.t Iarray.t -> unit)
  : Wasm.func =
    let ctx = (ctx :> [`Module | `Func] t) in
    let params = params |> Iarray.map (Pair.map_fst (fresh_local_id ctx)) in

    f ctx (params |> Iarray.map Pair.fst);

    Local_id_supply.reset ctx.local_id_supply;

    Wasm.{
      id;
      params;
      results;
      locals = take_iarray ctx.locals;
      body = take_iarray ctx.instrs;
    }

  let build_subexpr (ctx : [`Module | `Func] t) (f : [`Module | `Func] t -> unit) : Wasm.expr =
    let instrs = Dynarray.create () in
    f { ctx with instrs };
    take_iarray instrs

end


(** Translation *)

type item_decl =
  | Val of Core.Item.vis * Wasm.Func_id.t
  | Fun of Core.Item.vis * Wasm.Func_id.t

let rec translate_ty (ctx : [> `Module] Ctx.t) (ty : Core.Ty.t) : Wasm.value_type =
  match ty with
  | Core.Ty.Bool -> Wasm.I32
  | Core.Ty.I32 -> Wasm.I32
  | Core.Ty.Fun (param_tys, result_ty) ->
      Wasm.Ref (None, Id (Ctx.emit_type_def ctx "funty" (Wasm.Func {
        params = Iarray.map (translate_ty ctx) param_tys;
        results = [|translate_ty ctx result_ty|];
      })))

let translate_expr
  ~(enable_tail_call : bool)
  (ctx : [> `Module | `Func] Ctx.t)
  (item_env : item_decl Core.Item_map.t)
  (local_env : Wasm.Local_id.t Core.Local.Env.t)
  (expr : Core.Expr.t)
: unit =
  let rec go_expr ~tail_call ctx local_env (expr : Core.Expr.t) =
    match expr with
    | Core.Expr.Item (name, _) ->
        begin match Core.Item_map.find name item_env with
        | Val (_, id) -> go_direct_call ctx id ~tail_call

        (* Create a reference to a function definition. If the function is never
           exported then Wasm requires us to add it to an element declaration at
           the top-level of the module. *)
        | Fun (vis, id) ->
            (* Whenever we create a reference to a non-exported function we need
               to add it to a list of references in an element declaration *)
            if vis = Priv then Ctx.emit_func_ref ctx id;
            Ctx.emit_instr ctx (Wasm.Ref_func id);
        end

    | Core.Expr.Var (index, _) ->
        let id = Core.Local.Env.lookup index local_env in
        Ctx.emit_instr ctx (Wasm.Local_get id);

    | Core.Expr.Let ((name, ty, def), body) ->
        let def_id = Ctx.fresh_local_id ctx (Option.value name ~default:"_") in
        Ctx.emit_local ctx (def_id, translate_ty ctx ty);
        go_expr ctx local_env def ~tail_call:false;
        Ctx.emit_instr ctx (Wasm.Local_set def_id);
        go_expr ctx (Core.Local.Env.extend def_id local_env) body ~tail_call;

    (* Applications of items (possibly indirect, if calling through a value) *)
    | Core.Expr.Fun_app (Item (name, ty) as fun_, args) ->
        args |> Iarray.iter (go_expr ctx local_env ~tail_call:false);
        begin match Core.Item_map.find name item_env with
        | Val (_, id) -> go_indirect_call local_env ctx fun_ ~tail_call
        | Fun (_, id) -> go_direct_call ctx id ~tail_call
        end

    (* Applications of anything else (always indirect) *)
    | Core.Expr.Fun_app (fun_, args) ->
        args |> Iarray.iter (go_expr ctx local_env ~tail_call:false);
        go_indirect_call local_env ctx fun_ ~tail_call;

    | Core.Expr.Bool true -> Ctx.emit_instr ctx (Wasm.I32_const 1l);
    | Core.Expr.Bool false -> Ctx.emit_instr ctx (Wasm.I32_const 0l);

    | Core.Expr.Bool_if (expr1, expr2, expr3) ->
        go_expr ctx ~tail_call:false local_env expr1;
        let expr2 = Ctx.build_subexpr ctx (fun ctx -> go_expr ctx local_env expr2 ~tail_call) in
        let expr3 = Ctx.build_subexpr ctx (fun ctx -> go_expr ctx local_env expr3 ~tail_call) in

        Ctx.emit_instr ctx (Wasm.If (translate_ty ctx (Core.Expr.ty_of expr), expr2, expr3));

    | Core.Expr.I32 i -> Ctx.emit_instr ctx (Wasm.I32_const i);

    | Core.Expr.Prim (op, args) ->
        begin match op with
        | Prim.Op.I32_neg -> Ctx.emit_instr ctx (Wasm.I32_const 0l);
        | _ -> ()
        end;
        args |> Iarray.iter (go_expr ctx local_env ~tail_call:false);
        begin match op with
        | Prim.Op.Bool_eq -> Ctx.emit_instr ctx Wasm.I32_eq;
        | Prim.Op.I32_eq -> Ctx.emit_instr ctx Wasm.I32_eq;
        | Prim.Op.I32_add -> Ctx.emit_instr ctx Wasm.I32_add;
        | Prim.Op.I32_sub -> Ctx.emit_instr ctx Wasm.I32_sub;
        | Prim.Op.I32_mul -> Ctx.emit_instr ctx Wasm.I32_mul;
        | Prim.Op.I32_neg -> Ctx.emit_instr ctx Wasm.I32_sub;
        end;

  and go_direct_call ~tail_call ctx (id : Wasm.Func_id.t) =
    match enable_tail_call && tail_call with
    | true -> Ctx.emit_instr ctx (Wasm.Return_call id);
    | false -> Ctx.emit_instr ctx (Wasm.Call id);

  and go_indirect_call ~tail_call local_env ctx (fun_ : Core.Expr.t) =
    go_expr ctx local_env fun_ ~tail_call:false;
    match translate_ty ctx (Core.Expr.ty_of fun_), enable_tail_call && tail_call with
    | Wasm.Ref (_, Id id), true -> Ctx.emit_instr ctx (Wasm.Return_call_ref id)
    | Wasm.Ref (_, Id id), false -> Ctx.emit_instr ctx (Wasm.Call_ref id)
    | _ -> failwith "expected reference to type id"
  in

  go_expr ctx local_env expr ~tail_call:enable_tail_call

let translate_fun
  ~(enable_tail_call : bool)
  (ctx : [> `Module] Ctx.t)
  (item_env : item_decl Core.Item_map.t)
  (id : Wasm.Func_id.t)
  (params : (string option * Core.Ty.t) Iarray.t)
  (result_ty : Core.Ty.t)
  (body : Core.Expr.t)
: Wasm.func =
  let params = params |> Iarray.map (Pair.map (Option.value ~default:"_") (translate_ty ctx)) in
  let result_ty = translate_ty ctx result_ty in

  Ctx.build_func ctx id params [|result_ty|] @@ fun ctx param_ids ->
    let local_env = Iarray.to_seq param_ids |> Core.Local.Env.of_seq in
    translate_expr ctx item_env local_env body ~enable_tail_call

let translate_module ~(enable_tail_call : bool) (mod_ : Core.Module.t) : Wasm.module_ =
  let ctx = Ctx.create () in

  (* Generate function ids for each item *)
  let item_env =
    mod_ |> Core.Item_map.mapi @@ fun name item ->
      match item with
      | Core.Item.Val (vis, ty, _) -> Val (vis, Ctx.fresh_func_id ctx (Core.Item_name.to_string name))
      | Core.Item.Fun (vis, params, ty, _) -> Fun (vis, Ctx.fresh_func_id ctx (Core.Item_name.to_string name))
  in

  item_env |> Core.Item_map.iter begin fun name item_decl ->
    match Core.Item_map.find name mod_, item_decl with
    (* FIXME: re-evaluation of top-level values.

        Possible fixes:
        - normalise expressions (using NbE) and store in global
        - create a global and initialise with a startup function
    *)
    | Core.Item.Val (vis, ty, body), Val (_, id) ->
        if vis = Pub then Ctx.emit_export ctx (Core.Item_name.to_string name, Wasm.Func id);
        Ctx.emit_func ctx (translate_fun ctx item_env id [||] ty body ~enable_tail_call);

    | Core.Item.Fun (vis, params, ty, body), Fun (_, id) ->
        if vis = Pub then Ctx.emit_export ctx (Core.Item_name.to_string name, Wasm.Func id);
        Ctx.emit_func ctx (translate_fun ctx item_env id params ty body ~enable_tail_call );

    | _, _ ->
        failwith "mismatched items"
  end;

  Ctx.build_module ctx
