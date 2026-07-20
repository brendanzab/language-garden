(** Translation from the core language to the Web Assembly Text Format (WAT). *)

module Type_supply = Name.Supply (Wasm.Type_id)
module Func_supply = Name.Supply (Wasm.Func_id)
module Local_supply = Name.Supply (Wasm.Local_id)

(* NOTE: Replace with [Dynarray.to_iarray] when moving to OCaml 5.5.
    See: https://github.com/ocaml/ocaml/pull/14693 *)
let make_iarray xs =
  Iarray.init (Dynarray.length xs) (Dynarray.get xs)

type item_decl =
  | Val of Wasm.Func_id.t
  | Fun of Wasm.Func_id.t

let rec translate_ty
  ~(add_type_def : string -> Wasm.comp_type -> Wasm.Type_id.t)
  (ty : Core.Ty.t)
: Wasm.value_type =
  let rec go ty =
    match ty with
    | Core.Ty.Bool -> Wasm.I32
    | Core.Ty.I32 -> Wasm.I32
    | Core.Ty.Fun (param_tys, result_ty) ->
        Wasm.Ref (None, Id (add_type_def "funty" (Wasm.Func {
          params = Iarray.map go param_tys;
          results = [|go result_ty|];
        })))
  in
  go ty

let translate_expr
  ~(enable_tail_call : bool)
  ~(add_type_def : string -> Wasm.comp_type -> Wasm.Type_id.t)
  ~(add_func_ref : Wasm.Func_id.t -> unit)
  ~(fresh_local_id : string -> Wasm.Local_id.t)
  (item_env : item_decl Core.Item_map.t)
  (local_env : Wasm.Local_id.t Core.Local.Env.t)
  (expr : Core.Expr.t)
: locals:Wasm.local Iarray.t * Wasm.expr =
  let locals = Dynarray.create () in
  let instrs = Dynarray.create () in
  let translate_ty = translate_ty ~add_type_def in

  let rec go_expr ~tail_call instrs local_env (expr : Core.Expr.t) =
    match expr with
    | Core.Expr.Item (name, _) ->
        begin match Core.Item_map.find name item_env with
        | Val id ->
            begin match enable_tail_call && tail_call with
            | true -> Dynarray.add_last instrs (Wasm.Return_call id);
            | false -> Dynarray.add_last instrs (Wasm.Call id);
            end
        | Fun id ->
            add_func_ref id;
            Dynarray.add_last instrs (Wasm.Ref_func id);
        end

    | Core.Expr.Var (index, _) ->
        let id = Core.Local.Env.lookup index local_env in
        Dynarray.add_last instrs (Wasm.Local_get id);

    | Core.Expr.Let ((name, ty, def), body) ->
        let def_id = fresh_local_id (Option.value name ~default:"_") in
        Dynarray.add_last locals (def_id, translate_ty ty);
        go_expr instrs local_env def ~tail_call:false;
        Dynarray.add_last instrs (Wasm.Local_set def_id);
        go_expr instrs (Core.Local.Env.extend def_id local_env) body ~tail_call;

    (* Direct call *)
    | Core.Expr.Fun_app (Item (name, ty), args) ->
        let id =
          match Core.Item_map.find name item_env with
          | Val _ -> failwith "function expected"
          | Fun id -> id
        in
        args |> Iarray.iter (fun arg -> ignore (go_expr instrs local_env ~tail_call:false arg));
        begin match enable_tail_call && tail_call with
        | true -> Dynarray.add_last instrs (Wasm.Return_call id);
        | false -> Dynarray.add_last instrs (Wasm.Call id);
        end

    (* Indirect call *)
    | Core.Expr.Fun_app (fun_, args) ->
        args |> Iarray.iter (fun arg -> ignore (go_expr instrs local_env ~tail_call:false arg));
        go_expr instrs local_env fun_ ~tail_call:false;
        let id =
          match translate_ty (Core.Expr.ty_of fun_) with
          | Wasm.Ref (_, Id id) -> id
          | _ -> failwith "function expected"
        in
        begin match enable_tail_call && tail_call with
        | true -> Dynarray.add_last instrs (Wasm.Return_call_ref id);
        | false -> Dynarray.add_last instrs (Wasm.Call_ref id);
        end

    | Core.Expr.Bool true -> Dynarray.add_last instrs (Wasm.I32_const 1l);
    | Core.Expr.Bool false -> Dynarray.add_last instrs (Wasm.I32_const 0l);

    | Core.Expr.Bool_if (expr1, expr2, expr3) ->
        let instrs2 = Dynarray.create () in
        let instrs3 = Dynarray.create () in

        go_expr instrs ~tail_call:false local_env expr1;
        go_expr instrs2 local_env expr2 ~tail_call;
        go_expr instrs3 local_env expr3 ~tail_call;

        Dynarray.add_last instrs (Wasm.If (
          translate_ty (Core.Expr.ty_of expr),
          make_iarray instrs2,
          make_iarray instrs3
        ));

    | Core.Expr.I32 i -> Dynarray.add_last instrs (Wasm.I32_const i);

    | Core.Expr.Prim (op, args) ->
        begin match op with
        | Prim.Op.I32_neg -> Dynarray.add_last instrs (Wasm.I32_const 0l);
        | _ -> ()
        end;
        args |> Iarray.iter (fun arg -> ignore (go_expr instrs local_env ~tail_call:false arg));
        begin match op with
        | Prim.Op.Bool_eq -> Dynarray.add_last instrs Wasm.I32_eq;
        | Prim.Op.I32_eq -> Dynarray.add_last instrs Wasm.I32_eq;
        | Prim.Op.I32_add -> Dynarray.add_last instrs Wasm.I32_add;
        | Prim.Op.I32_sub -> Dynarray.add_last instrs Wasm.I32_sub;
        | Prim.Op.I32_mul -> Dynarray.add_last instrs Wasm.I32_mul;
        | Prim.Op.I32_neg -> Dynarray.add_last instrs Wasm.I32_sub;
        end;
  in

  go_expr instrs local_env expr ~tail_call:enable_tail_call;

  ~locals:(make_iarray locals), make_iarray instrs

let translate_module ~(enable_tail_call : bool) (mod_ : Core.Module.t) : Wasm.module_ =
  (* Arrays to store exports and functions *)
  let exports : Wasm.export Dynarray.t = Dynarray.create () in
  let types : Wasm.rec_type Dynarray.t = Dynarray.create () in
  let elems : Wasm.elem Dynarray.t = Dynarray.create () in
  let funcs : Wasm.func Dynarray.t = Dynarray.create () in

  (* Array of function references, to be used in a single element declaration *)
  let func_refs = Dynarray.create () in

  (* Map for interning types as unique type ids *)
  let type_def_ids : (Wasm.comp_type, Wasm.Type_id.t) Hashtbl.t = Hashtbl.create 0 in

  (* Global id generators *)
  let fresh_type_id = Type_supply.(fresh (create ())) in
  let fresh_func_id = Func_supply.(fresh (create ())) in

  (* Add a new type definition the module *)
  let add_type_def name (ty : Wasm.comp_type) =
    match Hashtbl.find_opt type_def_ids ty with
    | Some id -> id
    | None ->
        let id = fresh_type_id name in
        Dynarray.add_last types (Wasm.Type_def (id, ty));
        Hashtbl.add type_def_ids ty id;
        id
  in

  let add_func_ref id =
    if Dynarray.mem id func_refs then () else
      Dynarray.add_last func_refs id
  in

  (* Generate function ids for each item *)
  let item_env =
    mod_ |> Core.Item_map.mapi @@ fun name item ->
      match item with
      | Core.Item.Val (ty, _) -> Val (fresh_func_id (Core.Item_name.to_string name))
      | Core.Item.Fun (params, ty, _) -> Fun (fresh_func_id (Core.Item_name.to_string name))
  in

  item_env |> Core.Item_map.iter begin fun name item_decl ->
    let fresh_local_id = Local_supply.(fresh (create ())) in
    let translate_ty = translate_ty ~add_type_def in
    let translate_expr = translate_expr item_env ~add_type_def ~add_func_ref ~fresh_local_id in

    match Core.Item_map.find name mod_, item_decl with
    (* FIXME: re-evaluation of top-level values.

        Possible fixes:
        - normalise expressions (using NbE) and store in global
        - create a global and initialise with a startup function
    *)
    | Core.Item.Val (ty, expr), Val id ->
        let result_ty = translate_ty ty in
        let ~locals, body = translate_expr Core.Local.Env.empty expr ~enable_tail_call in
        Dynarray.add_last exports (Core.Item_name.to_string name, Wasm.Func id);
        Dynarray.add_last funcs Wasm.{ id; params = [||]; results = [|result_ty|]; locals; body }

    | Core.Item.Fun (params, ty, body), Fun id ->
        let param_id name = fresh_local_id (Option.value name ~default:"_") in
        let params = params |> Iarray.map (fun (name, ty) -> param_id name, translate_ty ty) in
        let result_ty = translate_ty ty in
        let ~locals, body =
          let local_env = Iarray.to_seq params |> Seq.map fst |> Core.Local.Env.of_seq in
          translate_expr local_env body ~enable_tail_call
        in
        Dynarray.add_last exports (Core.Item_name.to_string name, Wasm.Func id);
        Dynarray.add_last funcs Wasm.{ id; params; results = [|result_ty|]; locals; body }

    | _, _ ->
        failwith "mismatched items"
  end;

  (* Construct an element declaration containing all of the function references
     used in the module. *)
  if Dynarray.length func_refs <> 0 then
    Dynarray.add_last elems Wasm.(Declare, Func (make_iarray func_refs));

  Wasm.{
    exports = make_iarray exports;
    types = make_iarray types;
    elems = make_iarray elems;
    funcs = make_iarray funcs;
  }
