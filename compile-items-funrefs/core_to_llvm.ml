(** Translation from the core language into LLVM IR.

    We don’t have mutation in our core language, so it turns out (as Appel has
    noted in the past) that the mapping to LLVM’s static single assignment is
    {e relatively} straightforward, and is quite similar to the {!Core_to_anf}
    translation.

    The main complication in this translation is conditionals. These require us
    to create “join nodes” with phi-instructions where the branching paths of
    computation come together, similar to the “join points” found in the {!Anf}
    language.

    - Andrew Appel. 1998. {{: https://dl.acm.org/doi/10.1145/278283.278285}
      SSA is Functional Programming}
    - {{: https://mapping-high-level-constructs-to-llvm-ir.readthedocs.io}
      Mapping High Level Constructs to LLVM IR}
*)

(** Context for emitting LLVM modules *)
module Ctx : sig

  (** The state of the context, parameterised by the capabilities it supports. *)
  type 'a t

  val create : unit -> [`Module] t

  val build_module : [`Module] t -> Llvm.module_

  val build_fun :
    [`Module] t ->
    Llvm.linkage ->
    Llvm.ty ->
    (Llvm.ty * string) Iarray.t ->
    ([`Module | `Cfg] t -> Llvm.Local_id.t Iarray.t -> unit) ->
    Llvm.fun_

  (** Module-level operations *)

  val fresh_global_id : [> `Module] t -> string -> Llvm.Global_id.t

  val emit_fun : [> `Module ] t -> Llvm.Global_id.t -> Llvm.fun_ -> unit

  (** Control flow graph operations *)

  val fresh_local_id : [> `Cfg] t -> string -> Llvm.Local_id.t
  val fresh_label : [> `Cfg] t -> string -> Llvm.Label.t

  val start_block :  [> `Cfg] t -> Llvm.Label.t -> unit
  val close_block :  [> `Cfg] t -> Llvm.term_instr -> unit
  val current_label :  [> `Cfg] t -> Llvm.Label.t

  val assign_instr :  [> `Cfg] t -> string -> Llvm.value_instr -> Llvm.opr

end = struct

  module Global_id_supply = Name.Supply (Llvm.Global_id)
  module Local_id_supply = Name.Supply (Llvm.Local_id)
  module Label_supply = Name.Supply (Llvm.Label)

  type 'a t = {
    global_id_supply : Global_id_supply.t;
    local_id_supply : Local_id_supply.t;
    label_supply : Label_supply.t;

    funs : (Llvm.Global_id.t * Llvm.fun_) Dynarray.t;
    blocks : Llvm.block Dynarray.t;
    current_label : Llvm.Label.t option ref;
    current_instrs : Llvm.instr Dynarray.t;
  }

  let create () : [`Module] t = {
    global_id_supply = Global_id_supply.create ();
    local_id_supply = Local_id_supply.create ();
    label_supply = Label_supply.create ();

    funs = Dynarray.create ();
    blocks = Dynarray.create ();
    current_label = ref None;
    current_instrs = Dynarray.create ();
  }

  let take_iarray xs =
    (* NOTE: Use [Dynarray.to_iarray] when moving to OCaml 5.5.
       See: https://github.com/ocaml/ocaml/pull/14693 *)
    let old_xs = Iarray.init (Dynarray.length xs) (Dynarray.get xs) in
    Dynarray.clear xs;
    old_xs

  let fresh_global_id (ctx : [> `Module] t) = Global_id_supply.fresh ctx.global_id_supply
  let fresh_local_id (ctx : [> `Cfg] t) = Local_id_supply.fresh ctx.local_id_supply
  let fresh_label (ctx : [> `Cfg] t) = Label_supply.fresh ctx.label_supply

  let emit_fun (ctx : [> `Module] t) (name : Llvm.Global_id.t) (fun_ : Llvm.fun_) =
    Dynarray.add_last ctx.funs (name, fun_)

  let ensure_closed (ctx : [> `Cfg] t) =
    assert (Option.is_none !(ctx.current_label));
    assert (Dynarray.is_empty ctx.current_instrs)

  let start_block (ctx : [> `Cfg] t) (label : Llvm.Label.t) =
    ensure_closed ctx;
    ctx.current_label := Some label

  let current_label (ctx : [> `Cfg] t) : Llvm.Label.t =
    Option.get !(ctx.current_label)

  let assign_instr (ctx : [> `Cfg] t) name (instr : Llvm.value_instr) : Llvm.opr =
    let id = fresh_local_id ctx name in
    Dynarray.add_last ctx.current_instrs Llvm.(Assign (id, instr));
    Llvm.Local id

  let close_block (ctx : [> `Cfg] t) (term : Llvm.term_instr) =
    let label = current_label ctx in
    let instrs = take_iarray ctx.current_instrs in
    Dynarray.add_last ctx.blocks Llvm.{ label; instrs; term };

    (* Reset the current block *)
    ctx.current_label := None;
    Dynarray.clear ctx.current_instrs

  let build_module (ctx : [`Module] t) =
    Llvm.{ funs = take_iarray ctx.funs }

  let build_fun
    (ctx : [`Module] t)
    (linkage : Llvm.linkage)
    (result_ty : Llvm.ty)
    (params : (Llvm.ty * string) Iarray.t)
    (f : [`Module | `Cfg] t -> Llvm.Local_id.t Iarray.t -> unit)
  : Llvm.fun_ =
    let ctx = (ctx :> [`Module | `Cfg] t) in

    (* Create fresh ids for the parameter names *)
    let params = params |> Iarray.map (Pair.map_snd (fresh_local_id ctx)) in

    (* Construct the control flow graph *)
    let cfg =
      start_block ctx (fresh_label ctx "entry");
      f ctx (params |> Iarray.map Pair.snd);
      ensure_closed ctx;

      Llvm.{ blocks = take_iarray ctx.blocks }
    in

    Label_supply.reset ctx.label_supply;
    Local_id_supply.reset ctx.local_id_supply;

    Llvm.{ linkage; result_ty; params; cfg }

end


(** Translation *)

(** Translate a core language type into an LLVM type *)
let rec translate_ty (ty : Core.Ty.t) : Llvm.ty =
  match ty with
  | Core.Ty.Bool -> Llvm.I1
  | Core.Ty.I32 -> Llvm.I32
  | Core.Ty.Fun (_, _) -> Llvm.Ptr

(** Item declarations *)
type item_decl =
  | Val of Llvm.Global_id.t
  | Fun of Llvm.Global_id.t

let translate_vis (vis : Core.Item.vis) : Llvm.linkage =
  match vis with
  | Pub -> Llvm.External
  | Priv -> Llvm.Private

(** Translate an expression in the current block. While doing this, more
    blocks might be added to the control flow graph. *)
let translate_expr
  (ctx : [> `Cfg] Ctx.t)
  (item_env : item_decl Core.Item_map.t)
  (local_env : Llvm.opr Core.Local.Env.t)
  (result_name : string)
  (expr : Core.Expr.t)
: Llvm.opr =
  let rec go_expr local_env result_name (expr : Core.Expr.t) : Llvm.opr =
    match expr with
    | Core.Expr.Item (name, ty) ->
        begin match Core.Item_map.find name item_env with
        | Val item_id -> Ctx.assign_instr ctx result_name Llvm.(Call (translate_ty ty, Global item_id, [||]))
        | Fun item_id -> Llvm.Global item_id
        end

    | Core.Expr.Var (index, _) ->
        Core.Local.Env.lookup index local_env

    | Core.Expr.Let ((name, def_ty, def), body) ->
        let def = go_expr local_env (Option.value name ~default:"_") def in
        go_expr (Core.Local.Env.extend def local_env) result_name body

    | Core.Expr.Fun_app (fun_, args) ->
        let result_ty, param_tys =
          match Core.Expr.ty_of fun_ with
          | Fun (param_tys, ty) -> translate_ty ty, param_tys |> Iarray.map translate_ty
          | _ -> failwith "function type expected"
        in
        let fun_ = go_expr local_env "fun" fun_ in
        let args = args |> Iarray.map (go_expr local_env "arg") in
        Ctx.assign_instr ctx result_name Llvm.(Call (result_ty, fun_, Iarray.combine param_tys args))

    | Core.Expr.Bool b -> Llvm.I1 b

    | Core.Expr.Bool_if (expr1, expr2, expr3) ->
        (* Generate some fresh labels to allow us to wire together the basic
           blocks of the if expression *)
        let true_label = Ctx.fresh_label ctx "if_true" in
        let false_label = Ctx.fresh_label ctx "if_false" in
        let end_label = Ctx.fresh_label ctx "if_end" in

        (* Translate the entrypoint of the if expression *)
        let cond = go_expr local_env "cond" expr1 in
        Ctx.close_block ctx Llvm.(Br_i1 (cond, true_label, false_label)) |> ignore;

        Ctx.start_block ctx true_label;
        let true_result = go_expr local_env "true_result" expr2 in
        let true_end_label = Ctx.current_label ctx in
        Ctx.close_block ctx Llvm.(Br end_label);

        Ctx.start_block ctx false_label;
        let false_result = go_expr local_env "false_result" expr3 in
        let false_end_label = Ctx.current_label ctx in
        Ctx.close_block ctx Llvm.(Br end_label);

        (* Start the block that will be executed after the if expression (i.e.
           the continuation). In order to maintain static single assignment,
           the results of the true and false branches will be bound to a single
           variable using a phi-node. *)
        Ctx.start_block ctx end_label;
        let result_ty = translate_ty (Core.Expr.ty_of expr) in
        Ctx.assign_instr ctx result_name Llvm.(Phi (result_ty, [|
          true_result, true_end_label;
          false_result, false_end_label;
        |]))

    | Core.Expr.I32 i -> Llvm.I32 i

    | Core.Expr.Prim (op, args) ->
        begin match op, args |> Iarray.map (go_expr local_env "arg") with
        | Prim.Op.Bool_eq, [|x; y|] -> Ctx.assign_instr ctx result_name Llvm.(Icmp (Eq, I1, x, y))
        | Prim.Op.I32_eq, [|x; y|] -> Ctx.assign_instr ctx result_name Llvm.(Icmp (Eq, I32, x, y))
        | Prim.Op.I32_add, [|x; y|] -> Ctx.assign_instr ctx result_name Llvm.(Add (I32, x, y))
        | Prim.Op.I32_sub, [|x; y|] -> Ctx.assign_instr ctx result_name Llvm.(Sub (I32, x, y))
        | Prim.Op.I32_mul, [|x; y|] -> Ctx.assign_instr ctx result_name Llvm.(Mul (I32, x, y))
        | Prim.Op.I32_neg, [|x|] -> Ctx.assign_instr ctx result_name Llvm.(Sub (I32, I32 0l, x))
        | _, _ -> Format.kasprintf failwith "mismatched arity for %t" (Prim.Op.pp op)
        end
  in

  go_expr local_env result_name expr

let translate_fun
  (ctx : [> `Module] Ctx.t)
  (item_env : item_decl Core.Item_map.t)
  (vis : Core.Item.vis)
  (params : (string option * Core.Ty.t) Iarray.t)
  (result_ty : Core.Ty.t)
  (expr : Core.Expr.t)
: Llvm.fun_ =
  let linkage = translate_vis vis in
  let result_ty = translate_ty result_ty in
  let params =
    params |> Iarray.map @@ fun (name, ty) ->
      translate_ty ty, Option.value name ~default:"_"
  in

  Ctx.build_fun ctx linkage result_ty params @@ fun ctx param_ids ->
    let local_env = Iarray.to_seq param_ids |> Seq.map (fun id -> Llvm.Local id) |> Core.Local.Env.of_seq in
    let result = translate_expr ctx item_env local_env "result" expr in
    Ctx.close_block ctx (Ret (result_ty, result)) |> ignore

let translate_module (mod_ : Core.Module.t) : Llvm.module_ =
  let ctx = Ctx.create () in

  (* Top-level items might be mutually recursive, so we need to process their
     declarations before we can translate them to definitions. *)
  let item_env =
    mod_ |> Core.Item_map.mapi @@ fun name item ->
      match item with
      | Core.Item.Val _ -> Val (Ctx.fresh_global_id ctx (Core.Item_name.to_string name))
      | Core.Item.Fun _ -> Fun (Ctx.fresh_global_id ctx (Core.Item_name.to_string name))
  in

  (* Translate items in the core language into LLVM function definitions *)
  item_env |> Core.Item_map.iter begin fun name item_decl ->
    match Core.Item_map.find name mod_, item_decl with
    | Core.Item.Val (vis, ty, body), Val id ->
        Ctx.emit_fun ctx id (translate_fun ctx item_env vis [||] ty body);
    | Core.Item.Fun (vis, params, result_ty, body), Fun id ->
        Ctx.emit_fun ctx id (translate_fun ctx item_env vis params result_ty body);
    | _, _ ->
        failwith "mismatched items"
  end;

  Ctx.build_module ctx
