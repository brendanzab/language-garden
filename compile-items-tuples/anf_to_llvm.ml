(** Translation from ANF into LLVM IR.

    The main difference between this translation and the translation from
    direct style core language (implemented in {!Core_to_llvm}) is in how
    join blocks are introduced. These already correspond to the join points in
    the ANF language so we don’t need to create them from scratch. Unfortunately
    we don’t easily know the arguments to the phi instructions ahead of time,
    and have to update the join-blocks whenever we see a jump expression.

    Unless there’s a pressing need for A-Normal form I think it’s easier to go
    straight from a direct style IR to SSA. That said, I still think it’s
    interesting to compare the approaches. I’m also pretty sure that an approach
    similar to this could be used to translate from block argument SSA to phi
    nodes. This is something I've not been able to find documented anywhere.

    - Richard A. Kelsey. 1995. {{: https://doi.org/10.1145/202529.202532}
      A correspondence between continuation passing style and static single
      assignment form}.
*)

(* NOTE: Replace with [Dynarray.to_iarray] when moving to OCaml 5.5.
    See: https://github.com/ocaml/ocaml/pull/14693 *)
let make_iarray xs =
  Iarray.init (Dynarray.length xs) (Dynarray.get xs)

(** Context for emitting LLVM modules *)
module Ctx : sig

  (** The state of the context, parameterised by the capabilities it supports. *)
  type 'a t

  val create : unit -> [`Module] t

  val build_module : [`Module] t -> Llvm.module_

  val build_fun_def :
    [`Module] t ->
    Llvm.linkage ->
    Llvm.ty ->
    (Llvm.ty * string) Iarray.t ->
    ([`Module | `Cfg] t -> Llvm.Local_id.t Iarray.t -> Llvm.block) ->
    Llvm.fun_def

  (** Module-level operations *)

  val fresh_global_id : [> `Module] t -> string -> Llvm.Global_id.t

  val emit_fun_decl : [> `Module ] t -> Llvm.Global_id.t -> Llvm.fun_decl -> unit
  val emit_fun_def : [> `Module ] t -> Llvm.Global_id.t -> Llvm.fun_def -> unit

  (** Control flow graph operations *)

  val fresh_local_id : [> `Cfg] t -> string -> Llvm.Local_id.t
  val fresh_label : [> `Cfg] t -> string -> Llvm.Label.t

  val emit_block : [> `Cfg] t -> Llvm.block -> unit

end = struct

  module Global_id_supply = Name.Supply (Llvm.Global_id)
  module Local_id_supply = Name.Supply (Llvm.Local_id)
  module Label_supply = Name.Supply (Llvm.Label)

  type 'a t = {
    global_id_supply : Global_id_supply.t;
    local_id_supply : Local_id_supply.t;
    label_supply : Label_supply.t;

    fun_decls : (Llvm.Global_id.t * Llvm.fun_decl) Dynarray.t;
    fun_defs : (Llvm.Global_id.t * Llvm.fun_def) Dynarray.t;

    blocks : Llvm.block Dynarray.t;
  }

  let create () : [`Module] t = {
    global_id_supply = Global_id_supply.create ();
    local_id_supply = Local_id_supply.create ();
    label_supply = Label_supply.create ();

    fun_decls = Dynarray.create ();
    fun_defs = Dynarray.create ();
    blocks = Dynarray.create ();
  }

  let take_iarray xs =
    let old_xs = make_iarray xs in
    Dynarray.clear xs;
    old_xs

  let fresh_global_id (ctx : [> `Module] t) = Global_id_supply.fresh ctx.global_id_supply
  let fresh_local_id (ctx : [> `Cfg] t) = Local_id_supply.fresh ctx.local_id_supply
  let fresh_label (ctx : [> `Cfg] t) = Label_supply.fresh ctx.label_supply

  let emit_fun_decl (ctx : [> `Module] t) (name : Llvm.Global_id.t) (fun_decl : Llvm.fun_decl) =
    Dynarray.add_last ctx.fun_decls (name, fun_decl)

  let emit_fun_def (ctx : [> `Module] t) (name : Llvm.Global_id.t) (fun_def : Llvm.fun_def) =
    Dynarray.add_last ctx.fun_defs (name, fun_def)

  let emit_block (ctx : [> `Cfg] t) = Dynarray.add_last ctx.blocks

  let build_module (ctx : [`Module] t) = Llvm.{
    fun_decls = take_iarray ctx.fun_decls;
    fun_defs = take_iarray ctx.fun_defs;
  }

  let build_fun_def
    (ctx : [`Module] t)
    (linkage : Llvm.linkage)
    (result_ty : Llvm.ty)
    (params : (Llvm.ty * string) Iarray.t)
    (f : [`Module | `Cfg] t -> Llvm.Local_id.t Iarray.t -> Llvm.block)
  : Llvm.fun_def =
    let ctx = (ctx :> [`Module | `Cfg] t) in

    (* Create fresh ids for the parameter names *)
    let param_ids = params |> Iarray.map (fun (_, n) -> fresh_local_id ctx n) in
    let params = Iarray.map2 (fun (ty, _) id -> ty, Some id) params param_ids in

    (* Construct the control flow graph *)
    let entry = f ctx param_ids in
    let cfg = Llvm.{ blocks = Iarray.append [|entry|] (take_iarray ctx.blocks) } in

    Label_supply.reset ctx.label_supply;
    Local_id_supply.reset ctx.local_id_supply;

    Llvm.{ linkage; result_ty; params; cfg }

end


(** Packed layout computation for 64-bit architectures

    For a demonstration of computing aligned layouts, see
    [scraps/misc_struct_layout.ml].
*)

(* TODO:
  - target-specific layout
  - aligned offsets
  - cache layouts
*)

let size_of (ty : Llvm.ty) : int =
  match ty with
  | Llvm.I1 -> 1
  | Llvm.I8 -> 1
  | Llvm.I32 -> 4
  | Llvm.Ptr -> 8   (* Assume 64-bit pointers *)

let size_of_fields (tys : Llvm.ty Iarray.t) : int =
  Iarray.fold_left (fun acc ty -> acc + size_of ty) 0 tys

let offset_of (tys : Llvm.ty Iarray.t) (i : int) =
  let rec go acc i =
    if i <= 0 then acc else
      go (acc + size_of (Iarray.get tys i)) (i - 1)
  in
  go 0 i


(** Partially constructed LLVM datatypes *)

type partial_phi = {
  id : Llvm.Local_id.t;
  ty : Llvm.ty;
  args : (Llvm.opr * Llvm.Label.t) Dynarray.t;
}

let create_phi (ctx : [> `Cfg] Ctx.t) (name : string) (ty : Llvm.ty) =
  let id = Ctx.fresh_local_id ctx name in
  { id; ty; args = Dynarray.create () }

type join_block = {
  label : Llvm.Label.t;
  phi : partial_phi;
  instrs : Llvm.(Local_id.t option * value_instr) Iarray.t;
  term : Llvm.term_instr;
}

type partial_block = {
  label : Llvm.Label.t;
  instrs : Llvm.(Local_id.t option * value_instr) Dynarray.t;
}

let create_block (label : Llvm.Label.t) : partial_block =
  { label; instrs = Dynarray.create () }

let assign_instr
  (ctx : [> `Cfg] Ctx.t)
  (block : partial_block)
  (name : string)
  (instr : Llvm.value_instr)
: Llvm.opr =
  let id = Ctx.fresh_local_id ctx name in
  Dynarray.add_last block.instrs (Some id, instr);
  Llvm.Local id

let command_instr
  (block : partial_block)
  (instr : Llvm.value_instr)
: unit =
  Dynarray.add_last block.instrs (None, instr)

let finish_block (block : partial_block) (term : Llvm.term_instr) : Llvm.block =
  Llvm.{ label = block.label; instrs = make_iarray block.instrs; term }


(** Translation *)

(** Translate an ANF type into an LLVM type *)
let rec translate_ty (ty : Anf.Ty.t) : Llvm.ty =
  match ty with
  | Anf.Ty.Bool -> Llvm.I1
  | Anf.Ty.I32 -> Llvm.I32
  | Anf.Ty.Fun (_, _) -> Llvm.Ptr
  | Anf.Ty.Tuple _ -> Llvm.Ptr

(** Item declarations *)
type item_decl =
  | Val of Llvm.Global_id.t
  | Fun of Llvm.Global_id.t

let translate_vis (vis : Core.Item.vis) : Llvm.linkage =
  match vis with
  | Pub -> Llvm.External
  | Priv -> Llvm.Private

let translate_expr
  ~(malloc : Llvm.Global_id.t Lazy.t)
  (ctx : [> `Cfg] Ctx.t)
  (item_env : item_decl Anf.Item_map.t)
  (local_env : Llvm.opr Anf.Local_map.t)
  (join_blocks : join_block Anf.Join_map.t ref)
  (block : partial_block)
  (result_name : string)
  (expr : Anf.Expr.t)
: Llvm.block =
  let assign_gep (block : partial_block) name ty src path =
    assign_instr ctx block name Llvm.(Getelementptr (ty, src, path))
  in

  (* Translate a sub-expression in the current block. While doing this, more
     blocks might be added to the control flow graph. *)
  let rec go_expr local_env (block : partial_block) (result_name : string) (expr : Anf.Expr.t) : Llvm.block =
    match expr with
    | Anf.Expr.Let (id, def_ty, def, body) ->
        let def = go_comp local_env block (Anf.Local_id.to_string id) def in
        go_expr (Anf.Local_map.add id def local_env) block result_name body

    | Anf.Expr.Bool_if (expr1, expr2, expr3) ->
        (* Generate some fresh labels to allow us to wire together the basic
           blocks of the if expression *)
        let true_label = Ctx.fresh_label ctx "if_true" in
        let false_label = Ctx.fresh_label ctx "if_false" in

        let true_block = go_expr local_env (create_block true_label) "true_result" expr2 in
        let false_block = go_expr local_env (create_block false_label) "false_result" expr3 in

        Ctx.emit_block ctx true_block;
        Ctx.emit_block ctx false_block;

        (* Translate the entrypoint of the if expression *)
        let cond = go_atom local_env block "cond" expr1 in
        finish_block block Llvm.(Br_i1 (cond, true_label, false_label))

    | Anf.Expr.Return expr ->
        let result_ty = translate_ty (Anf.Expr.ty_of_comp expr) in
        let result = go_comp local_env block result_name expr in
        finish_block block Llvm.(Ret (result_ty, result))

    | Anf.Expr.Join (join_id, (result_id, result_ty), cont, body) ->
        (* Create a join block that we can jump to later *)
        let phi = create_phi ctx (Anf.Local_id.to_string result_id) (translate_ty result_ty) in
        let Llvm.{ label; instrs; term } =
          let label = Ctx.fresh_label ctx (Anf.Join_id.to_string join_id) in
          let local_env = local_env |> Anf.Local_map.add result_id (Llvm.Local phi.id) in
          go_expr local_env (create_block label) result_name cont
        in
        join_blocks := Anf.Join_map.add join_id { label; phi; instrs; term } !join_blocks;

        (* Continue compiling the body of the expression, updating the join
           block whenever we see a jump instruction *)
        go_expr local_env block result_name body

    | Anf.Expr.Jump (join_id, arg) ->
        (* Find the corresponding join block and add the argument to its phi instruction *)
        let join_block = Anf.Join_map.find join_id !join_blocks in
        let result = go_atom local_env block result_name arg in
        Dynarray.add_last join_block.phi.args (result, block.label);

        (* Break to the corresponding join block *)
        finish_block block Llvm.(Br join_block.label)

  and go_comp local_env (block : partial_block) (result_name : string) (expr : Anf.Expr.comp) : Llvm.opr =
    match expr with
    | Anf.Expr.Fun_app (fun_, args) ->
        let result_ty, param_tys =
          match Anf.Expr.ty_of_atom fun_ with
          | Anf.Ty.Fun (param_tys, ty) ->
              translate_ty ty, param_tys |> Iarray.map translate_ty
          | _ -> failwith "function type expected"
        in
        let fun_ = go_atom local_env block "fun" fun_ in
        let args = args |> Iarray.map (go_atom local_env block "arg") in
        assign_instr ctx block result_name Llvm.(Call (result_ty, fun_, Iarray.combine param_tys args))

    | Anf.Expr.Tuple_proj (tuple, index) ->
        let elem_tys =
          match Anf.Expr.ty_of_atom tuple with
          | Tuple elem_tys -> elem_tys |> Iarray.map translate_ty
          | _ -> failwith "tuple type expected"
        in

        let tuple = go_atom local_env block "tuple" tuple in

        (* Get the pointer to the projected element *)
        let elem_offset = Int32.of_int (offset_of elem_tys index) in
        let elem_ptr = assign_gep block (result_name ^ ".ptr") Llvm.I8 tuple [|I32, I32 elem_offset|] in
        let elem_ty = Iarray.get elem_tys index in

        (* Load the value from the pointer*)
        assign_instr ctx block result_name Llvm.(Load (elem_ty, elem_ptr))

    | Anf.Expr.Prim (op, args) ->
        begin match op, args |> Iarray.map (go_atom local_env block "arg") with
        | Prim.Op.Bool_eq, [|x; y|] -> assign_instr ctx block result_name Llvm.(Icmp (Eq, I1, x, y))
        | Prim.Op.I32_eq, [|x; y|] -> assign_instr ctx block result_name Llvm.(Icmp (Eq, I32, x, y))
        | Prim.Op.I32_add, [|x; y|] -> assign_instr ctx block result_name Llvm.(Add (I32, x, y))
        | Prim.Op.I32_sub, [|x; y|] -> assign_instr ctx block result_name Llvm.(Sub (I32, x, y))
        | Prim.Op.I32_mul, [|x; y|] -> assign_instr ctx block result_name Llvm.(Mul (I32, x, y))
        | Prim.Op.I32_neg, [|x|] -> assign_instr ctx block result_name Llvm.(Sub (I32, I32 0l, x))
        | _, _ -> Format.kasprintf failwith "mismatched arity for %t" (Prim.Op.pp op)
        end

    | Anf.Expr.Atom expr ->
        go_atom local_env block result_name expr

  and go_atom local_env (block : partial_block) (result_name : string) (expr : Anf.Expr.atom) : Llvm.opr =
    match expr with
    | Anf.Expr.Item (name, ty) ->
        begin match Anf.Item_map.find name item_env with
        | Val item_id -> assign_instr ctx block result_name Llvm.(Call (translate_ty ty, Global item_id, [||]))
        | Fun item_id -> Llvm.Global item_id
        end
    | Anf.Expr.Var (id, _) -> Anf.Local_map.find id local_env
    | Anf.Expr.Tuple elems ->
        let elem_tys = elems |> Iarray.map (fun ty -> translate_ty (Anf.Expr.ty_of_atom ty)) in

        (* Allocate space for the tuple *)
        let malloc = Llvm.Global (Lazy.force malloc) in
        let size = Int32.of_int (size_of_fields elem_tys) in
        let tuple = assign_instr ctx block result_name Llvm.(Call (Ptr, malloc, [|I32, I32 size|])) in

        (* Initialise elements of the tuple in the allocated space *)
        elem_tys |> Iarray.iteri begin fun i ty ->
          let elem = go_atom local_env block "elem" (Iarray.get elems i) in
          let elem_offset = Int32.of_int (offset_of elem_tys i) in
          let elem_ptr = assign_gep block "elem.dst" Llvm.I8 tuple [|I32, I32 elem_offset|] in
          command_instr block Llvm.(Store (ty, elem, elem_ptr));
        end;

        (* Return a pointer to the tuple *)
        tuple

    | Anf.Expr.Bool b -> Llvm.I1 b
    | Anf.Expr.I32 i -> Llvm.I32 i
  in

  go_expr local_env block result_name expr

let translate_fun
  ~(malloc : Llvm.Global_id.t Lazy.t)
  (ctx : [> `Module] Ctx.t)
  (item_env : item_decl Anf.Item_map.t)
  (vis : Anf.Item.vis)
  (params : (Anf.Local_id.t * Core.Ty.t) Iarray.t)
  (result_ty : Anf.Ty.t)
  (expr : Anf.Expr.t)
: Llvm.fun_def =
  let linkage = translate_vis vis in
  let result_ty = translate_ty result_ty in
  let params' =
    params |> Iarray.map @@ fun (id, ty) ->
      translate_ty ty, Anf.Local_id.to_string id
  in

  Ctx.build_fun_def ctx linkage result_ty params' @@ fun ctx param_ids ->
    let join_blocks = ref Anf.Join_map.empty in

    (* Compile the entry block *)
    let entry_block =
      let local_env =
        Seq.zip (Iarray.to_seq params) (Iarray.to_seq param_ids)
        |> Seq.map (fun ((id, _), id') -> id, Llvm.Local id')
        |> Anf.Local_map.of_seq
      in
      let entry_label = Ctx.fresh_label ctx "entry" in
      translate_expr ctx item_env local_env join_blocks (create_block entry_label) "result" expr
        ~malloc
    in

    (* Finish constructing the join blocks *)
    !join_blocks |> Anf.Join_map.iter begin fun _ { label; phi; instrs; term } ->
      let result = Some phi.id, Llvm.Phi (phi.ty, make_iarray phi.args) in
      Ctx.emit_block ctx Llvm.{ label; instrs = Iarray.append [|result|] instrs; term };
    end;

    entry_block

let translate_module (mod_ : Anf.Module.t) : Llvm.module_ =
  let ctx = Ctx.create () in

  let malloc = lazy begin
    let name = Ctx.fresh_global_id ctx "malloc" in
    Ctx.emit_fun_decl ctx name Llvm.{
      linkage = Llvm.External;
      result_ty = Ptr;
      params = [|I32, None|];
    };
    name
  end in

  (* Top-level items might be mutually recursive, so we need to process their
     declarations before we can translate them to definitions. *)
  let item_env =
    mod_ |> Anf.Item_map.mapi @@ fun name item ->
      match item with
      | Anf.Item.Val _ -> Val (Ctx.fresh_global_id ctx (Anf.Item_name.to_string name))
      | Anf.Item.Fun _ -> Fun (Ctx.fresh_global_id ctx (Anf.Item_name.to_string name))
  in

  (* Translate items in the core language into LLVM function definitions *)
  item_env |> Anf.Item_map.iter begin fun name item_decl ->
    match Anf.Item_map.find name mod_, item_decl with
    | Anf.Item.Val (vis, ty, body), Val id ->
        Ctx.emit_fun_def ctx id (translate_fun ctx item_env vis [||] ty body ~malloc);
    | Anf.Item.Fun (vis, params, result_ty, body), Fun id ->
        Ctx.emit_fun_def ctx id (translate_fun ctx item_env vis params result_ty body ~malloc);
    | _, _ ->
        failwith "mismatched items"
  end;

  Ctx.build_module ctx
