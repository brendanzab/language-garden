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

module Global_supply = Name.Supply (Llvm.Global_id)
module Local_supply = Name.Supply (Llvm.Local_id)
module Label_supply = Name.Supply (Llvm.Label)

(* NOTE: Replace with [Dynarray.to_iarray] when moving to OCaml 5.5.
    See: https://github.com/ocaml/ocaml/pull/14693 *)
let make_iarray xs =
  Iarray.init (Dynarray.length xs) (Dynarray.get xs)

(** Translate an ANF type into an LLVM type *)
let rec translate_ty (ty : Anf.Ty.t) : Llvm.ty =
  match ty with
  | Anf.Ty.Bool -> Llvm.I1
  | Anf.Ty.I32 -> Llvm.I32
  | Anf.Ty.Fun (_, _) -> Llvm.Ptr
  | Anf.Ty.Tuple tys ->
      (* Llvm.Struct (tys |> Iarray.map translate_ty) *)
      failwith "TODO"

(** Item declarations *)
type item_decl =
  | Val of Llvm.Global_id.t
  | Fun of Llvm.Global_id.t

let translate_vis (vis : Core.Item.vis) :  Llvm.linkage option =
  match vis with
  | Pub -> None
  | Priv -> Some Llvm.Private

type partial_phi = {
  id : Llvm.Local_id.t;
  ty : Llvm.ty;
  args : (Llvm.opr * Llvm.Label.t) Dynarray.t;
}

type partial_block = {
  label : Llvm.Label.t;
  instrs : Llvm.instr Dynarray.t;
}

let translate_fun
  (item_env : item_decl Anf.Item_map.t)
  (vis : Anf.Item.vis)
  (params : (Anf.Local_id.t * Core.Ty.t) Iarray.t)
  (result_ty : Anf.Ty.t)
  (expr : Anf.Expr.t)
: Llvm.fun_ =
  let fresh_local_id = Local_supply.(fresh (create ())) in
  let fresh_label = Label_supply.(fresh (create ())) in

  let join_blocks = ref Anf.Join_map.empty in (* Join blocks *)
  let blocks = Dynarray.create () in (* Finished blocks *)

  let begin_block (label : Llvm.Label.t) : partial_block =
    { label; instrs = Dynarray.create () }
  in

  let assign_instr (block : partial_block) (name : string) (instr : Llvm.value_instr) : Llvm.opr =
    let id = fresh_local_id name in
    Dynarray.add_last block.instrs Llvm.(Assign (id, instr));
    Local id
  in

  let finish_block (block : partial_block) (term : Llvm.term_instr) : Llvm.block =
    Llvm.{ label = block.label; instrs = make_iarray block.instrs; term }
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
        let true_label = fresh_label "if_true" in
        let false_label = fresh_label "if_false" in

        let true_block = go_expr local_env (begin_block true_label) "true_result" expr2 in
        let false_block = go_expr local_env (begin_block false_label) "false_result" expr3 in

        Dynarray.add_last blocks true_block;
        Dynarray.add_last blocks false_block;

        (* Translate the entrypoint of the if expression *)
        let cond = go_atom local_env block "cond" expr1 in
        finish_block block Llvm.(Br_i1 (cond, true_label, false_label))

    | Anf.Expr.Return expr ->
        let result_ty = translate_ty (Anf.Expr.ty_of_comp expr) in
        let result = go_comp local_env block result_name expr in
        finish_block block Llvm.(Ret (result_ty, result))

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
          go_expr local_env (begin_block label) result_name cont
        in
        join_blocks := Anf.Join_map.add join_id (join_phi, join_block) !join_blocks;
        go_expr local_env block result_name body

    | Anf.Expr.Jump (join_id, arg) ->
        (* Find the corresponding join block and add the argument to its phi instruction *)
        let join_phi, join_block = Anf.Join_map.find join_id !join_blocks in
        let result = go_atom local_env block result_name arg in
        Dynarray.add_last join_phi.args (result, block.label);

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
        assign_instr block result_name Llvm.(Call (result_ty, fun_, Iarray.combine param_tys args))

    | Anf.Expr.Prim (op, args) ->
        begin match op, args |> Iarray.map (go_atom local_env block "arg") with
        | Prim.Op.Bool_eq, [|x; y|] -> assign_instr block result_name Llvm.(Icmp (Eq, I1, x, y))
        | Prim.Op.I32_eq, [|x; y|] -> assign_instr block result_name Llvm.(Icmp (Eq, I32, x, y))
        | Prim.Op.I32_add, [|x; y|] -> assign_instr block result_name Llvm.(Add (I32, x, y))
        | Prim.Op.I32_sub, [|x; y|] -> assign_instr block result_name Llvm.(Sub (I32, x, y))
        | Prim.Op.I32_mul, [|x; y|] -> assign_instr block result_name Llvm.(Mul (I32, x, y))
        | Prim.Op.I32_neg, [|x|] -> assign_instr block result_name Llvm.(Sub (I32, I32 0l, x))
        | _, _ -> Format.kasprintf failwith "mismatched arity for %t" (Prim.Op.pp op)
        end

    | Anf.Expr.Atom expr ->
        go_atom local_env block result_name expr

  and go_atom local_env (block : partial_block) (result_name : string) (expr : Anf.Expr.atom) : Llvm.opr =
    match expr with
    | Anf.Expr.Item (name, ty) ->
        begin match Anf.Item_map.find name item_env with
        | Val item_id -> assign_instr block result_name Llvm.(Call (translate_ty ty, Global item_id, [||]))
        | Fun item_id -> Llvm.Global item_id
        end
    | Anf.Expr.Var (id, _) -> Anf.Local_map.find id local_env
    | Anf.Expr.Bool b -> Llvm.I1 b
    | Anf.Expr.I32 i -> Llvm.I32 i
  in

  let linkage = translate_vis vis in
  let result_ty = translate_ty result_ty in
  let param_ids =
    Iarray.to_seq params
    |> Seq.map (fun (id, _) -> id, fresh_local_id (Anf.Local_id.to_string id))
    |> Anf.Local_map.of_seq
  in
  let params =
    params |> Iarray.map @@ fun (id, ty) ->
      translate_ty ty, Anf.Local_map.find id param_ids
  in

  let cfg =
    (* Compile the entry block *)
    let entry_block =
      let local_env = param_ids |> Anf.Local_map.map (fun id -> Llvm.Local id) in
      go_expr local_env (begin_block (fresh_label "entry")) "result" expr
    in

    (* Finish constructing the join blocks *)
    !join_blocks |> Anf.Join_map.iter begin fun _ (phi, block) ->
      let result = Llvm.Assign (phi.id, Phi (phi.ty, make_iarray phi.args)) in
      Dynarray.add_last blocks Llvm.{ block with instrs = Iarray.append [|result|] block.instrs };
    end;

    Llvm.{ blocks = Iarray.append [|entry_block|] (make_iarray blocks) }
  in

  Llvm.{ linkage; result_ty; params; cfg }

(** Translate an ANF module into an LLVM module *)
let translate_module (mod_ : Anf.Module.t) : Llvm.module_ =
  let fresh_global_id = Global_supply.(fresh (create ())) in

  (* Top-level items might be mutually recursive, so we need to process their
     declarations before we can translate them to definitions. *)
  let item_env =
    mod_ |> Anf.Item_map.mapi @@ fun name item ->
      match item with
      | Anf.Item.Val _ -> Val (fresh_global_id (Anf.Item_name.to_string name))
      | Anf.Item.Fun _ -> Fun (fresh_global_id (Anf.Item_name.to_string name))
  in

  let funs = Dynarray.create () in

  item_env |> Anf.Item_map.iter begin fun name item_decl ->
    match Anf.Item_map.find name mod_, item_decl with
    | Anf.Item.Val (vis, ty, body), Val id ->
        Dynarray.add_last funs Llvm.(id, translate_fun item_env vis [||] ty body);
    | Anf.Item.Fun (vis, params, result_ty, body), Fun id ->
        Dynarray.add_last funs Llvm.(id, translate_fun item_env vis params result_ty body);
    | _, _ ->
        failwith "mismatched items"
  end;

  Llvm.{
    funs = make_iarray funs;
  }
