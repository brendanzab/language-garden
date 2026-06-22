(** Translate the core language into LLVM IR

    - {{: https://mapping-high-level-constructs-to-llvm-ir.readthedocs.io}
      Mapping High Level Constructs to LLVM IR}
*)

module Global_supply = Name.Label.Supply (Llvm.Global_id)
module Local_supply = Name.Label.Supply (Llvm.Local_id)
module Label_supply = Name.Label.Supply (Llvm.Label)

(* NOTE: Replace with [Dynarray.to_iarray] when moving to OCaml 5.5.
    See: https://github.com/ocaml/ocaml/pull/14693 *)
let make_iarray xs =
  Iarray.init (Dynarray.length xs) (Dynarray.get xs)

let translate_ty (ty : Core.Ty.t) : Llvm.ty =
  match ty with
  | Core.Ty.Bool -> Llvm.I1
  | Core.Ty.I32 -> Llvm.I32

let translate_expr
  (fresh_local_id : string -> Llvm.Local_id.t)
  (fresh_label : string -> Llvm.Label.t)
  (item_env : Llvm.(ty * Global_id.t) Core.Item_map.t)
  (local_env : Llvm.(ty * opr) Core.Local.Env.t)
  (expr : Core.Expr.t)
: Llvm.cfg =
  let ( let@ ) = ( @@ ) in

  let blocks = Dynarray.create () in

  let bind_instr name instr (k : Llvm.(Local_id.t -> block)) : Llvm.block =
    let id = fresh_local_id name in
    Llvm.Instr (id, instr, k id)
  in

  let rec go_expr local_env result_name expr (k : Llvm.(ty * opr -> block)) : Llvm.block =
    match expr with
    | Core.Expr.Item (name, args) ->
        let result_ty, item_id = Core.Item_map.find name item_env in
        let@ args = go_exprs local_env "arg" (Option.value args ~default:[||] |> Iarray.to_list) in
        let@ result_id = bind_instr result_name Llvm.(Call (result_ty, Global item_id, Iarray.of_list args)) in
        k (result_ty, Llvm.Local result_id)

    | Core.Expr.Var index ->
        k (Core.Local.Env.lookup index local_env)

    | Core.Expr.Let ((name, ty, def), body) ->
        let@ def_ty, def = go_expr local_env (Option.value name ~default:"") def in
        go_expr (Core.Local.Env.extend (def_ty, def) local_env) result_name body k

    | Core.Expr.Bool true -> k Llvm.(I1, I1 true)
    | Core.Expr.Bool false -> k Llvm.(I1, I1 false)

    | Core.Expr.Bool_if (expr1, expr2, expr3, result_ty) ->
        (* Generate some fresh labels to allow us to wire together the basic
           blocks of the if expression *)
        let if_true = fresh_label "if_true" in
        let if_false = fresh_label "if_false" in
        let if_end = fresh_label "if_end" in

        let result_true = ref None in
        Dynarray.add_last blocks (if_true, begin
          let@ ty, result_true' = go_expr local_env "true_result" expr2 in
          result_true := Some result_true';
          Llvm.(Term (Br if_end))
        end);

        let result_false = ref None in
        Dynarray.add_last blocks (if_false, begin
          let@ ty, result_false' = go_expr local_env "false_result" expr3 in
          result_false := Some result_false';
          Llvm.(Term (Br if_end))
        end);

        (* Translate the code that will be run after the if expression has been
           evaluated (i.e. the continuation). The result of the true and false
           branches will be bound to a phi-node. *)
        Dynarray.add_last blocks (if_end, begin
          let result_ty = translate_ty result_ty in
          let@ result_id = bind_instr result_name Llvm.(Phi (result_ty, [|
            Option.get !result_true, if_true;
            Option.get !result_false, if_false;
          |])) in
          k (result_ty, Llvm.Local result_id)
        end);

        (* Translate the entrypoint of the if expression *)
        let@ _, cond = go_expr local_env "cond" expr1 in
        Llvm.(Term (Br_i1 (cond, if_true, if_false)))

    | Core.Expr.I32 i -> k Llvm.(I32, I32 i)

    | Core.Expr.Prim (Bool_eq, [|x; y|]) ->
        let@ _, x = go_expr local_env "arg" x in
        let@ _, y = go_expr local_env "arg" y in
        let@ result_id = bind_instr result_name Llvm.(Icmp (Eq, I1, x, y)) in
        k (I1, Local result_id)

    | Core.Expr.Prim (I32_eq, [|x; y|]) ->
        let@ _, x = go_expr local_env "arg" x in
        let@ _, y = go_expr local_env "arg" y in
        let@ result_id = bind_instr result_name Llvm.(Icmp (Eq, I32, x, y)) in
        k (I1, Local result_id)

    | Core.Expr.Prim (I32_add, [|x; y|]) ->
        let@ _, x = go_expr local_env "arg" x in
        let@ _, y = go_expr local_env "arg" y in
        let@ result_id = bind_instr result_name Llvm.(Add (I32, x, y)) in
        k (I32, Local result_id)

    | Core.Expr.Prim (I32_sub, [|x; y|]) ->
        let@ _, x = go_expr local_env "arg" x in
        let@ _, y = go_expr local_env "arg" y in
        let@ result_id = bind_instr result_name Llvm.(Sub (I32, x, y)) in
        k (I32, Local result_id)

    | Core.Expr.Prim (I32_mul, [|x; y|]) ->
        let@ _, x = go_expr local_env "arg" x in
        let@ _, y = go_expr local_env "arg" y in
        let@ result_id = bind_instr result_name Llvm.(Mul (I32, x, y)) in
        k (I32, Local result_id)

    | Core.Expr.Prim (I32_neg, [|x|]) ->
        let@ _, x = go_expr local_env "arg" x in
        let@ result_id = bind_instr result_name Llvm.(Sub (I32, I32 0l, x)) in
        k (I32, Local result_id)

    | Core.Expr.Prim (op, _) ->
        Format.kasprintf failwith "mismatched arity for %t" (Prim.Op.pp op)

  (* Compile a series of expressions to intermediate definitions *)
  and go_exprs local_ids name exprs (k : Llvm.((ty * opr) list -> block)) : Llvm.block =
    match exprs with
    | [] -> k []
    | expr :: exprs ->
        let@ expr = go_expr local_ids name expr in
        let@ exprs = go_exprs local_ids name exprs in
        k (expr :: exprs)
  in

  let entry =
    let@ result_ty, result = go_expr local_env "result" expr in
    Llvm.(Term (Ret (result_ty, result)))
  in
  let blocks = make_iarray blocks in

  Llvm.{ entry; blocks }

let translate_module (mod_ : Core.Module.t) : Llvm.module_ =
  let item_env =
    let fresh_global_id = Global_supply.(fresh (create ())) in
    mod_ |> Core.Item_map.mapi @@ fun name item ->
      let Core.Item.(Val (ty, _) | Fun (_, ty, _)) = item in
      translate_ty ty, fresh_global_id (Core.Item_name.to_string name)
  in

  let funs = Dynarray.create () in

  item_env |> Core.Item_map.iter begin fun name (result_ty, id) ->
    let fresh_local_id = Local_supply.(fresh (create ())) in
    let fresh_label = Label_supply.(fresh (create ())) in

    let params, body =
      match Core.Item_map.find name mod_ with
      | Core.Item.Val (_, def) -> ([||] : _ Iarray.t), def
      | Core.Item.Fun (params, _, body) ->
          let params = params |> Iarray.map @@ fun (name, ty) ->
            translate_ty ty, fresh_local_id (Option.value name ~default:"")
          in
          params, body
    in

    let cfg =
      let local_env =
        Iarray.to_seq params
        |> Seq.map (Pair.map_snd (fun id -> Llvm.Local id))
        |> Core.Local.Env.of_seq
      in
      translate_expr fresh_local_id fresh_label item_env local_env body
     in

    Dynarray.add_last funs Llvm.(id, { result_ty; params; cfg });
  end;

  Llvm.{
    funs = make_iarray funs;
  }
