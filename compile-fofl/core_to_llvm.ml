(** Translate the core language into LLVM IR

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

let translate_ty (ty : Core.Ty.t) : Llvm.ty =
  match ty with
  | Core.Ty.Bool -> Llvm.I1
  | Core.Ty.I32 -> Llvm.I32

type 'a k = 'a -> Llvm.block

type item_decl =
  | Fun of Llvm.ty * Llvm.Global_id.t * Llvm.ty Iarray.t

let translate_expr
  (fresh_local_id : string -> Llvm.Local_id.t)
  (fresh_label : string -> Llvm.Label.t)
  (item_env : item_decl Core.Item_map.t)
  (local_env : Llvm.opr Core.Local.Env.t)
  (expr : Core.Expr.t)
  (ty : Llvm.ty)
: Llvm.cfg =
  let ( let@ ) = ( @@ ) in

  let blocks = Dynarray.create () in

  let bind_instr name (instr : Llvm.instr) (k : Llvm.opr k) : Llvm.block =
    let id = fresh_local_id name in
    Llvm.Instr (id, instr, k (Local id))
  in

  let rec go_expr local_env result_name expr (k : Llvm.opr k) : Llvm.block =
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
        let@ def = go_expr local_env (Option.value name ~default:"") def in
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

        let result_true = ref None in
        let result_false = ref None in

        Dynarray.add_last blocks (true_label, begin
          let@ result_true' = go_expr local_env "true_result" expr2 in
          result_true := Some result_true';
          Llvm.(Term (Br true_end_label))
        end);

        Dynarray.add_last blocks (false_label, begin
          let@ result_false' = go_expr local_env "false_result" expr3 in
          result_false := Some result_false';
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
            Option.get !result_true, true_end_label;
            Option.get !result_false, false_end_label;
          |])) in
          k result
        end);

        (* Translate the entrypoint of the if expression *)
        let@ cond = go_expr local_env "cond" expr1 in
        Llvm.(Term (Br_i1 (cond, true_label, false_label)))

    | Core.Expr.I32 i -> k Llvm.(I32 i)

    | Core.Expr.Prim (Bool_eq, [|x; y|]) ->
        let@ x = go_expr local_env "arg" x in
        let@ y = go_expr local_env "arg" y in
        bind_instr result_name Llvm.(Icmp (Eq, I1, x, y)) k

    | Core.Expr.Prim (I32_eq, [|x; y|]) ->
        let@ x = go_expr local_env "arg" x in
        let@ y = go_expr local_env "arg" y in
        bind_instr result_name Llvm.(Icmp (Eq, I32, x, y)) k

    | Core.Expr.Prim (I32_add, [|x; y|]) ->
        let@ x = go_expr local_env "arg" x in
        let@ y = go_expr local_env "arg" y in
        bind_instr result_name Llvm.(Add (I32, x, y)) k

    | Core.Expr.Prim (I32_sub, [|x; y|]) ->
        let@ x = go_expr local_env "arg" x in
        let@ y = go_expr local_env "arg" y in
        bind_instr result_name Llvm.(Sub (I32, x, y)) k

    | Core.Expr.Prim (I32_mul, [|x; y|]) ->
        let@ x = go_expr local_env "arg" x in
        let@ y = go_expr local_env "arg" y in
        bind_instr result_name Llvm.(Mul (I32, x, y)) k

    | Core.Expr.Prim (I32_neg, [|x|]) ->
        let@ x = go_expr local_env "arg" x in
        bind_instr result_name Llvm.(Sub (I32, I32 0l, x)) k

    | Core.Expr.Prim (op, _) ->
        Format.kasprintf failwith "mismatched arity for %t" (Prim.Op.pp op)

  (* Compile a series of expressions to intermediate definitions *)
  and go_exprs local_ids name exprs (k : Llvm.opr list k) : Llvm.block =
    match exprs with
    | [] -> k []
    | expr :: exprs ->
        let@ expr = go_expr local_ids name expr in
        let@ exprs = go_exprs local_ids name exprs in
        k (expr :: exprs)
  in

  let entry =
    let@ result = go_expr local_env "result" expr in
    Llvm.(Term (Ret (ty, result)))
  in
  let blocks = make_iarray blocks in

  Llvm.{ entry; blocks }

let translate_module (mod_ : Core.Module.t) : Llvm.module_ =
  let fresh_global_id = Global_supply.(fresh (create ())) in
  let item_env =
    mod_ |> Core.Item_map.mapi @@ fun name item ->
      let id = fresh_global_id (Core.Item_name.to_string name) in
      match item with
      | Core.Item.Val (ty, def) ->
          Fun (translate_ty ty, id, ([||] : _ Iarray.t))
      | Core.Item.Fun (params, ty, body) ->
          Fun (translate_ty ty, id, params |> Iarray.map (fun (_, ty) -> translate_ty ty))
  in

  let funs = Dynarray.create () in

  item_env |> Core.Item_map.iter begin fun name (Fun (result_ty, id, param_tys)) ->
    let fresh_local_id = Local_supply.(fresh (create ())) in
    let fresh_label = Label_supply.(fresh (create ())) in

    let params, body =
      match Core.Item_map.find name mod_ with
      | Core.Item.Val (_, def) -> ([||] : _ Iarray.t), def
      | Core.Item.Fun (params, _, body) -> params, body
    in
    let params = params |> Iarray.mapi @@ fun i (name, _) ->
      Iarray.get param_tys i,
      fresh_local_id (Option.value name ~default:"")
    in
    let cfg =
      let local_env =
        Iarray.to_seq params
        |> Seq.map (fun (_, id) -> Llvm.Local id)
        |> Core.Local.Env.of_seq
      in
      translate_expr fresh_local_id fresh_label item_env local_env body result_ty
     in

    Dynarray.add_last funs Llvm.(id, { result_ty; params; cfg });
  end;

  Llvm.{
    funs = make_iarray funs;
  }
