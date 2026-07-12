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

type 'a k = 'a -> Llvm.block

(** Translate an expression into a control flow graph. *)
let translate_expr
  ~(fresh_local_id : string -> Llvm.Local_id.t)
  ~(fresh_label : string -> Llvm.Label.t)
  (item_env : item_decl Core.Item_map.t)
  (local_env : Llvm.opr Core.Local.Env.t)
  (expr : Core.Expr.t)
  (ty : Llvm.ty)
: Llvm.cfg =
  let ( let@ ) = ( @@ ) in

  let blocks = Dynarray.create () in

  (* Bind an instruction to variable in the current block  *)
  let bind_instr name (instr : Llvm.instr) (k : Llvm.opr k) : Llvm.block =
    let id = fresh_local_id name in
    Llvm.Instr (id, instr, k (Local id))
  in

  (* Translate a sub-expression in the current block. While doing this, more
     blocks might be added to the control flow graph. *)
  let rec go_expr local_env result_name (expr : Core.Expr.t) (k : Llvm.opr k) : Llvm.block =
    match expr with
    | Core.Expr.Item (name, args) ->
        let Fun (result_ty, item_id, param_tys) = Core.Item_map.find name item_env in
        let@ args = go_exprs local_env "arg" (Option.value args ~default:[||] |> Iarray.to_list) in
        let args = Iarray.combine param_tys (Iarray.of_list args) in
        let@ result = bind_instr result_name Llvm.(Call (result_ty, Global item_id, args)) in
        k result

    | Core.Expr.Var index ->
        k (Core.Local.Env.lookup index local_env)

    | Core.Expr.Let ((name, _, def), body) ->
        let@ def = go_expr local_env (Option.value name ~default:"_") def in
        go_expr (Core.Local.Env.extend def local_env) result_name body k

    | Core.Expr.Bool true -> k Llvm.(I1 true)
    | Core.Expr.Bool false -> k Llvm.(I1 false)

    | Core.Expr.Bool_if (expr1, expr2, expr3, result_ty) ->
        (* Generate some fresh labels to allow us to wire together the basic
           blocks of the if expression *)
        let true_label = fresh_label "if_true" in
        let false_label = fresh_label "if_false" in
        let true_end_label = fresh_label "if_true_end" in
        let false_end_label = fresh_label "if_false_end" in
        let end_label = fresh_label "if_end" in

        (* These operands store the result of executing either branch of the if
           expression, and will be used as arguments to the phi instruction in
           the end block *)
        let true_result = ref None in
        let false_result = ref None in

        Dynarray.add_last blocks (true_label, begin
          let@ true_result' = go_expr local_env "true_result" expr2 in
          true_result := Some true_result';
          Llvm.(Term (Br true_end_label))
        end);

        Dynarray.add_last blocks (false_label, begin
          let@ false_result' = go_expr local_env "false_result" expr3 in
          false_result := Some false_result';
          Llvm.(Term (Br false_end_label))
        end);

        (* We don't know what the exit block of the branches are, so these
           blocks will be used by the phi instruction to identify which branch
           we have come from.

           Another solution would be to return the label of the exit block when
           compiling expressions, but that would involve threading through a lot
           of state, and this seems like a simpler and less invasive approach,
           even if it results in more blocks being generated. *)
        Dynarray.add_last blocks (true_end_label, Llvm.(Term (Br end_label)));
        Dynarray.add_last blocks (false_end_label, Llvm.(Term (Br end_label)));

        (* Translate the blocks that will be run after the if expression has
           been evaluated (i.e. the continuation). The result of the true and
           false branches will be bound to a phi-node. *)
        Dynarray.add_last blocks (end_label, begin
          let result_ty = translate_ty result_ty in
          let@ result = bind_instr result_name Llvm.(Phi (result_ty, [|
            Option.get !true_result, true_end_label;
            Option.get !false_result, false_end_label;
          |])) in
          k result
        end);

        (* Translate the entrypoint of the if expression *)
        let@ cond = go_expr local_env "cond" expr1 in
        Llvm.(Term (Br_i1 (cond, true_label, false_label)))

    | Core.Expr.I32 i -> k Llvm.(I32 i)

    | Core.Expr.Prim (op, args) ->
        let@ args = go_exprs local_env "arg" (Iarray.to_list args) in
        begin match op, args with
        | Prim.Op.Bool_eq, [x; y] -> bind_instr result_name Llvm.(Icmp (Eq, I1, x, y)) k
        | Prim.Op.I32_eq, [x; y] -> bind_instr result_name Llvm.(Icmp (Eq, I32, x, y)) k
        | Prim.Op.I32_add, [x; y] -> bind_instr result_name Llvm.(Add (I32, x, y)) k
        | Prim.Op.I32_sub, [x; y] -> bind_instr result_name Llvm.(Sub (I32, x, y)) k
        | Prim.Op.I32_mul, [x; y] -> bind_instr result_name Llvm.(Mul (I32, x, y)) k
        | Prim.Op.I32_neg, [x] -> bind_instr result_name Llvm.(Sub (I32, I32 0l, x)) k
        | _, _ -> Format.kasprintf failwith "mismatched arity for %t" (Prim.Op.pp op)
        end

  (* Translate a series of expressions in the current block *)
  and go_exprs local_env name (exprs : Core.Expr.t list) (k : Llvm.opr list k) : Llvm.block =
    match exprs with
    | [] -> k []
    | expr :: exprs ->
        let@ expr = go_expr local_env name expr in
        let@ exprs = go_exprs local_env name exprs in
        k (expr :: exprs)
  in

  let entry_label = fresh_label "entry" in
  let entry_block =
    let@ result = go_expr local_env "result" expr in
    Llvm.(Term (Ret (ty, result)))
  in

  Llvm.{
    entry = entry_label, entry_block;
    blocks = make_iarray blocks;
  }

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
