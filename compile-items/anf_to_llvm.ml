(** Translation from ANF into LLVM IR.

    This is similar to the translation in {!Core_to_llvm}, but here we translate
    the join points already present in ANF as opposed to creating join blocks
    for conditionals specifically. When encountering {!Anf.Expr.Jump}s, the
    current block is terminated with a {!Llvm.Br} instruction, and an argument
    is added to the phi-node at the start of the corresponding join block.

    - Richard A. Kelsey. 1995. {{: https://doi.org/10.1145/202529.202532}
      A correspondence between continuation passing style and static single
      assignment form}.
*)

module Global_supply = Name.Supply (Llvm.Global_id)
module Local_supply = Name.Supply (Llvm.Local_id)
module Label_supply = Name.Supply (Llvm.Label)

(* NOTE: Replace with [Dynarray.to_iarray] when moving to OCaml 5.5.
    See: https://github.com/ocaml/ocaml/pull/14693 *)
let make_iarray xs =
  Iarray.init (Dynarray.length xs) (Dynarray.get xs)

(** Translate a primitive type into an LLVM type *)
let translate_prim_ty (ty : Prim.Ty.t) : Llvm.ty =
  match ty with
  | Prim.Ty.Bool -> Llvm.I1
  | Prim.Ty.I32 -> Llvm.I32

(** Translate a type in the core language into an LLVM type *)
let rec translate_ty (ty : Anf.Ty.t) : Llvm.ty =
  match ty with
  | Anf.Ty.Bool -> Llvm.I1
  | Anf.Ty.I32 -> Llvm.I32

type join_phi = {
  id : Llvm.Local_id.t;
  ty : Llvm.ty;
  args : (Llvm.opr * Llvm.Label.t) Dynarray.t;
}

(** Translate an expression into a control flow graph. *)
let translate_expr
  ~(fresh_local_id : string -> Llvm.Local_id.t)
  ~(fresh_label : string -> Llvm.Label.t)
  (item_env : Llvm.Global_id.t Core.Item_map.t)
  (local_env : Llvm.opr Anf.Local_map.t)
  (expr : Anf.Expr.t)
: Llvm.cfg =
  let join_blocks = ref Anf.Join_map.empty in (* Join blocks *)
  let blocks = Dynarray.create () in (* Finished blocks *)

  let bind_instr instrs name (instr : Llvm.value_instr) : Llvm.opr =
    let id = fresh_local_id name in
    Dynarray.add_last instrs Llvm.(Assign (id, instr));
    Local id
  in

  (* Translate a sub-expression in the current block. While doing this, more
     blocks might be added to the control flow graph. *)
  let rec go_expr local_env label instrs result_name (expr : Anf.Expr.t) : Llvm.block =
    match expr with
    | Anf.Expr.Let (id, def_ty, def, body) ->
        let def = go_comp local_env instrs (Anf.Local_id.to_string id) def in
        go_expr (Anf.Local_map.add id def local_env) label instrs result_name body

    | Anf.Expr.Bool_if (expr1, expr2, expr3) ->
        (* Generate some fresh labels to allow us to wire together the basic
           blocks of the if expression *)
        let true_label = fresh_label "if_true" in
        let false_label = fresh_label "if_false" in

        let true_block = go_expr local_env true_label (Dynarray.create ()) "true_result" expr2 in
        let false_block = go_expr local_env false_label (Dynarray.create ()) "false_result" expr3 in

        Dynarray.add_last blocks true_block;
        Dynarray.add_last blocks false_block;

        (* Translate the entrypoint of the if expression *)
        let cond = go_atom local_env instrs "cond" expr1 in
        Llvm.{ label; instrs = make_iarray instrs; term = Br_i1 (cond, true_label, false_label) }

    | Anf.Expr.Return expr ->
        let result_ty = translate_ty (Anf.Expr.ty_of_comp expr) in
        let result = go_comp local_env instrs result_name expr in
        Llvm.{ label; instrs = make_iarray instrs; term = Ret (result_ty, result) }

    | Anf.Expr.Join (join_id, (result_id, result_ty), cont, body) ->
        (* An empty phi instruction at the start of the join block *)
        let join_phi = {
          id = fresh_local_id (Anf.Local_id.to_string result_id);
          ty = translate_ty result_ty;
          args = Dynarray.create ();
        } in
        (* The block that the phi instruction will be added to *)
        let join_block =
          let label = fresh_label (Anf.Join_id.to_string join_id) in
          let local_env = local_env |> Anf.Local_map.add result_id (Llvm.Local join_phi.id) in
          go_expr local_env label (Dynarray.create ()) result_name cont
        in
        join_blocks := Anf.Join_map.add join_id (join_phi, join_block) !join_blocks;
        go_expr local_env label instrs result_name body

    | Anf.Expr.Jump (join_id, arg) ->
        (* Find the corresponding join block and add the argument to its phi instruction *)
        let join_phi, join_block = Anf.Join_map.find join_id !join_blocks in
        let result = go_atom local_env instrs result_name arg in
        Dynarray.add_last join_phi.args (result, label);

        (* Break to the corresponding join block *)
        Llvm.{ label; instrs = make_iarray instrs; term = Br join_block.label }

  and go_comp local_env instrs result_name (expr : Anf.Expr.comp) : Llvm.opr =
    match expr with
    | Anf.Expr.Item (name, args, result_ty) ->
        let item_id = Anf.Item_map.find name item_env in
        let args = args |> Iarray.map @@ fun arg ->
          translate_ty (Anf.Expr.ty_of_atom arg), go_atom local_env instrs "arg" arg
        in
        bind_instr instrs result_name Llvm.(Call (translate_ty result_ty, Global item_id, args))

    | Anf.Expr.Prim (op, args) ->
        let args =  args |> Iarray.map (go_atom local_env instrs "arg") in
        begin match op, args with
        | Prim.Op.Bool_eq, [|x; y|] -> bind_instr instrs result_name Llvm.(Icmp (Eq, I1, x, y))
        | Prim.Op.I32_eq, [|x; y|] -> bind_instr instrs result_name Llvm.(Icmp (Eq, I32, x, y))
        | Prim.Op.I32_add, [|x; y|] -> bind_instr instrs result_name Llvm.(Add (I32, x, y))
        | Prim.Op.I32_sub, [|x; y|] -> bind_instr instrs result_name Llvm.(Sub (I32, x, y))
        | Prim.Op.I32_mul, [|x; y|] -> bind_instr instrs result_name Llvm.(Mul (I32, x, y))
        | Prim.Op.I32_neg, [|x|] -> bind_instr instrs result_name Llvm.(Sub (I32, I32 0l, x))
        | _, _ -> Format.kasprintf failwith "mismatched arity for %t" (Prim.Op.pp op)
        end

    | Anf.Expr.Atom expr ->
        go_atom local_env instrs result_name expr

  and go_atom local_env instrs result_name (expr : Anf.Expr.atom) : Llvm.opr =
    match expr with
    | Anf.Expr.Item (name, ty) ->
        let item_id = Anf.Item_map.find name item_env in
        bind_instr instrs result_name Llvm.(Call (translate_ty ty, Global item_id, [||]))
    | Anf.Expr.Var (id, _) -> Anf.Local_map.find id local_env
    | Anf.Expr.Bool b -> Llvm.I1 b
    | Anf.Expr.I32 i -> Llvm.I32 i
  in

  (* Compile the entry block *)
  let entry = go_expr local_env (fresh_label "entry") (Dynarray.create ()) "result" expr in

  (* Finish constructing the join blocks *)
  !join_blocks |> Anf.Join_map.iter begin fun _ (phi, block) ->
    let result = Llvm.Assign (phi.id, Phi (phi.ty, make_iarray phi.args)) in
    Dynarray.add_last blocks Llvm.{ block with instrs = Iarray.append [|result|] block.instrs };
  end;

  Llvm.{ blocks = Iarray.append [|entry|] (make_iarray blocks) }

let translate_vis (vis : Anf.Item.vis) :  [`Private] option =
  match vis with
  | Pub -> None
  | Priv -> Some `Private

(** Translate a core language module into an LLVM module  *)
let translate_module (mod_ : Anf.Module.t) : Llvm.module_ =
  let fresh_global_id = Global_supply.(fresh (create ())) in

  (* Top-level items might be mutually recursive, so we need to process their
     declarations before we can translate them to definitions. *)
  let item_env =
    mod_ |> Anf.Item_map.mapi @@ fun name item ->
      fresh_global_id (Anf.Item_name.to_string name)
  in

  let funs = Dynarray.create () in

  (* Translate items in the core language into LLVM function definitions *)
  item_env |> Anf.Item_map.iter begin fun name item_decl ->
    let fresh_local_id = Local_supply.(fresh (create ())) in
    let fresh_label = Label_supply.(fresh (create ())) in
    let translate_expr = translate_expr item_env ~fresh_local_id ~fresh_label in

    match Anf.Item_map.find name mod_, item_decl with
    | Anf.Item.Val (vis, ty, def), id ->
        let visibility = translate_vis vis in
        let cfg = translate_expr Anf.Local_map.empty def in
        let result_ty = translate_ty ty in
        Dynarray.add_last funs Llvm.(id, { visibility; result_ty; params = [||]; cfg });

    | Anf.Item.Fun (vis, params, result_ty, body), id ->
        let visibility = translate_vis vis in
        let param_ids =
          Iarray.to_seq params
          |> Seq.map (fun (id, _) -> id, fresh_local_id (Anf.Local_id.to_string id))
          |> Anf.Local_map.of_seq
        in
        let params = params |> Iarray.map (fun (id, ty) -> translate_ty ty, Anf.Local_map.find id param_ids) in
        let result_ty = translate_ty result_ty in
        let local_env = param_ids |> Anf.Local_map.map (fun id -> Llvm.Local id) in
        let cfg = translate_expr local_env body in
        Dynarray.add_last funs Llvm.(id, { visibility; result_ty; params; cfg });
  end;

  Llvm.{
    funs = make_iarray funs;
  }
