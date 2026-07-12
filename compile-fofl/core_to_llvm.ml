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

module Global_supply = Name.Supply (Llvm.Global_id)
module Local_supply = Name.Supply (Llvm.Local_id)
module Label_supply = Name.Supply (Llvm.Label)

(* NOTE: Replace with [Dynarray.to_iarray] when moving to OCaml 5.5.
    See: https://github.com/ocaml/ocaml/pull/14693 *)
let make_iarray xs =
  Iarray.init (Dynarray.length xs) (Dynarray.get xs)

(** Translate a type in the core language into an LLVM type *)
let translate_ty (ty : Core.Ty.t) : Llvm.ty =
  match ty with
  | Core.Ty.Bool -> Llvm.I1
  | Core.Ty.I32 -> Llvm.I32

(** Item declarations *)
type item_decl =
  | Fun of Llvm.ty * Llvm.Global_id.t * Llvm.ty Iarray.t

(** Translate an expression into a control flow graph. *)
let translate_expr
  ~(fresh_local_id : string -> Llvm.Local_id.t)
  ~(fresh_label : string -> Llvm.Label.t)
  (item_env : item_decl Core.Item_map.t)
  (local_env : Llvm.opr Core.Local.Env.t)
  (expr : Core.Expr.t)
  (ty : Llvm.ty)
: Llvm.cfg =
  let blocks = Dynarray.create () in
  let current_label = ref None in
  let current_instrs = Dynarray.create () in

  (* Bind an instruction to variable in the current block *)
  let bind_instr name (instr : Llvm.value_instr) : Llvm.opr =
    let id = fresh_local_id name in
    Dynarray.add_last current_instrs Llvm.(Assign (id, instr));
    Local id
  in

  (* Set the label of the current block *)
  let start_block (label : Llvm.Label.t) =
    assert (Option.is_none !current_label);
    assert (Dynarray.is_empty current_instrs);
    current_label := Some label;
  in

  (* Finish the current block *)
  let close_block (term : Llvm.term_instr) : Llvm.Label.t =
    let label = Option.get !current_label in
    let instrs = make_iarray current_instrs in
    Dynarray.add_last blocks Llvm.{ label; instrs; term };
    current_label := None;
    Dynarray.clear current_instrs;
    label
  in

  (* Translate a sub-expression in the current block. While doing this, more
     blocks might be added to the control flow graph. *)
  let rec go_expr local_env result_name (expr : Core.Expr.t) : Llvm.opr =
    match expr with
    | Core.Expr.Item (name, args) ->
        let Fun (result_ty, item_id, param_tys) = Core.Item_map.find name item_env in
        let args = Option.value args ~default:[||] |> Iarray.map (go_expr local_env "arg") in
        bind_instr result_name Llvm.(Call (result_ty, Global item_id, Iarray.combine param_tys args))

    | Core.Expr.Var index ->
        Core.Local.Env.lookup index local_env

    | Core.Expr.Let ((name, _, def), body) ->
        let def = go_expr local_env (Option.value name ~default:"_") def in
        go_expr (Core.Local.Env.extend def local_env) result_name body

    | Core.Expr.Bool true -> Llvm.(I1 true)
    | Core.Expr.Bool false -> Llvm.(I1 false)

    | Core.Expr.Bool_if (expr1, expr2, expr3, result_ty) ->
        (* Generate some fresh labels to allow us to wire together the basic
           blocks of the if expression *)
        let true_label = fresh_label "if_true" in
        let false_label = fresh_label "if_false" in
        let end_label = fresh_label "if_end" in

        (* Translate the entrypoint of the if expression *)
        let cond = go_expr local_env "cond" expr1 in
        close_block Llvm.(Br_i1 (cond, true_label, false_label)) |> ignore;

        start_block true_label;
        let true_result = go_expr local_env "true_result" expr2 in
        let true_end_label = close_block Llvm.(Br end_label) in

        start_block false_label;
        let false_result = go_expr local_env "false_result" expr3 in
        let false_end_label = close_block Llvm.(Br end_label) in

        (* Translate the blocks that will be run after the if expression has
           been evaluated (i.e. the continuation). The results of the true and
           false branches will be bound to a phi-node. *)
        start_block end_label;
        let result_ty = translate_ty result_ty in
        bind_instr result_name Llvm.(Phi (result_ty, [|
          true_result, true_end_label;
          false_result, false_end_label;
        |]))

    | Core.Expr.I32 i -> Llvm.(I32 i)

    | Core.Expr.Prim (op, args) ->
        let args =  args |> Iarray.map (go_expr local_env "arg") in
        begin match op, args with
        | Prim.Op.Bool_eq, [|x; y|] -> bind_instr result_name Llvm.(Icmp (Eq, I1, x, y))
        | Prim.Op.I32_eq, [|x; y|] -> bind_instr result_name Llvm.(Icmp (Eq, I32, x, y))
        | Prim.Op.I32_add, [|x; y|] -> bind_instr result_name Llvm.(Add (I32, x, y))
        | Prim.Op.I32_sub, [|x; y|] -> bind_instr result_name Llvm.(Sub (I32, x, y))
        | Prim.Op.I32_mul, [|x; y|] -> bind_instr result_name Llvm.(Mul (I32, x, y))
        | Prim.Op.I32_neg, [|x|] -> bind_instr result_name Llvm.(Sub (I32, I32 0l, x))
        | _, _ -> Format.kasprintf failwith "mismatched arity for %t" (Prim.Op.pp op)
        end
  in

  start_block (fresh_label "entry");
  let result = go_expr local_env "result" expr in
  close_block (Ret (ty, result)) |> ignore;

  Llvm.{ blocks = make_iarray blocks }

(** Translate a core language module into an LLVM module  *)
let translate_module (mod_ : Core.Module.t) : Llvm.module_ =
  let fresh_global_id = Global_supply.(fresh (create ())) in

  (* Top-level items might be mutually recursive, so we need to process their
     declarations before we can translate them to definitions. *)
  let item_env =
    mod_ |> Core.Item_map.mapi @@ fun name item ->
      let id = fresh_global_id (Core.Item_name.to_string name) in
      match item with
      | Core.Item.Val (ty, def) ->
          Fun (translate_ty ty, id, [||])
      | Core.Item.Fun (params, ty, body) ->
          Fun (translate_ty ty, id, params |> Iarray.map (fun (_, ty) -> translate_ty ty))
  in

  let funs = Dynarray.create () in

  (* Translate items in the core language into LLVM function definitions *)
  item_env |> Core.Item_map.iter begin fun name item_decl ->
    let fresh_local_id = Local_supply.(fresh (create ())) in
    let fresh_label = Label_supply.(fresh (create ())) in
    let translate_expr = translate_expr item_env ~fresh_local_id ~fresh_label in

    match Core.Item_map.find name mod_, item_decl with
    | Core.Item.Val (_, body), Fun (result_ty, id, param_tys) ->
        let cfg = translate_expr Core.Local.Env.empty body result_ty in
        Dynarray.add_last funs Llvm.(id, { result_ty; params = [||]; cfg });

    | Core.Item.Fun (params, _, body), Fun (result_ty, id, param_tys) ->
        let param_id name = fresh_local_id (Option.value name ~default:"_") in
        let params = Iarray.map2 (fun ty (name, _) -> ty, param_id name) param_tys params in
        let local_env =
          Iarray.to_seq params
          |> Seq.map (fun (_, id) -> Llvm.Local id)
          |> Core.Local.Env.of_seq
        in
        let cfg = translate_expr local_env body result_ty in
        Dynarray.add_last funs Llvm.(id, { result_ty; params; cfg });
  end;

  Llvm.{
    funs = make_iarray funs;
  }
