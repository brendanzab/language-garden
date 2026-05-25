(** Translation from the core language to the Web Assembly Text Format (WAT). *)

module Opts = struct

  type t = {
    enable_tail_call : bool;
  }

end

module Env : sig

  type t

  val empty :
    item_ids:(Core.Item_name.t -> Wat.Func_id.t) ->
    item_tys:(Core.Item_name.t -> Prim.Ty.t) ->
    t

  val extend : Wat.Local_id.t * Prim.Ty.t -> t -> t
  val lookup_item_id : Core.Item_name.t -> t -> Wat.Func_id.t
  val lookup_local_id : Core.Local.Index.t -> t -> Wat.Local_id.t
  val type_of : Core.Expr.t -> t -> Prim.Ty.t

end = struct

  type t = {
    item_ids : Core.Item_name.t -> Wat.Func_id.t;
    item_tys : Core.Item_name.t -> Core.Ty.t;
    local_ids : Wat.Local_id.t Core.Local.Env.t;
    local_tys : Core.Ty.t Core.Local.Env.t;
  }

  let empty ~item_ids ~item_tys = {
    item_ids;
    item_tys;
    local_ids = Core.Local.Env.empty;
    local_tys = Core.Local.Env.empty;
  }

  let extend (id, ty) env = {
    env with
    local_ids = Core.Local.Env.extend id env.local_ids;
    local_tys = Core.Local.Env.extend ty env.local_tys;
  }

  let lookup_item_id name env =
    env.item_ids name

  let lookup_local_id index env =
    Core.Local.Env.lookup index env.local_ids

  let type_of expr env =
    Core.Expr.type_of env.item_tys env.local_tys expr

end

let translate_ty (ty : Core.Ty.t) : Wat.ty =
  match ty with
  | Core.Ty.Bool -> Wat.I32
  | Core.Ty.I32 -> Wat.I32

let fresh_local_id (name : string option) : Wat.Local_id.t =
  Wat.Local_id.fresh (Option.value name ~default:"")

(** Translate an expression in the core language, returning a list of the locals
    defined in the expression along with the translated web assembly expression. *)
let translate_expr (opts : Opts.t) (env : Env.t) (expr : Core.Expr.t) : (Wat.Local_id.t * Wat.ty) list * Wat.expr =
  (* The locals we've seen while translating the expression *)
  let seen_locals = Dynarray.create () in

  let ( << ) = Fun.compose in

  (* Translate a sub-expression, while updating the list of locals bindings.
     This returns a function that adds a series of instructions to the start of
     an expression which avoids the exponential performance cost of repeated
     calls to [List.append]. *)
  let rec go_expr ~tail_call env expr : Wat.expr -> Wat.expr =
    match expr with
    | Core.Expr.Item (name, args) when opts.enable_tail_call && tail_call ->
        go_exprs env (Option.value args ~default:[||])
          << List.cons (Wat.Return_call (Env.lookup_item_id name env))

    | Core.Expr.Item (name, args) ->
        go_exprs env (Option.value args ~default:[||])
          << List.cons (Wat.Call (Env.lookup_item_id name env))

    | Core.Expr.Var index ->
        List.cons (Wat.Local_get (Env.lookup_local_id index env))

    | Core.Expr.Let ((name, ty, def), body) ->
        let def_id = fresh_local_id name in
        Dynarray.add_last seen_locals (def_id, translate_ty ty);
        go_expr ~tail_call:false env def
          << List.cons (Wat.Local_set def_id)
          << go_expr ~tail_call (Env.extend (def_id, ty) env) body

    | Core.Expr.Bool true -> List.cons (Wat.I32_const 1l)
    | Core.Expr.Bool false -> List.cons (Wat.I32_const 0l)

    | Core.Expr.Bool_if (expr1, expr2, expr3) ->
        go_expr ~tail_call:false env expr1
          << List.cons (Wat.If (
            translate_ty (Env.type_of expr2 env),
            go_expr ~tail_call env expr2 [],
            go_expr ~tail_call env expr3 []
          ))

    | Core.Expr.I32 i -> List.cons (Wat.I32_const i)

    | Core.Expr.Prim (Bool_eq, args) -> go_exprs env args << List.cons Wat.I32_eq
    | Core.Expr.Prim (I32_eq, args) -> go_exprs env args << List.cons Wat.I32_eq
    | Core.Expr.Prim (I32_add, args) -> go_exprs env args << List.cons Wat.I32_add
    | Core.Expr.Prim (I32_sub, args) -> go_exprs env args << List.cons Wat.I32_sub
    | Core.Expr.Prim (I32_mul, args) -> go_exprs env args << List.cons Wat.I32_mul
    | Core.Expr.Prim (I32_neg, args) -> go_exprs env args << List.cons Wat.I32_neg

  (* Translate a series of expressions. This is useful for translating a series
     of function arguments. *)
  and go_exprs env exprs : Wat.expr -> Wat.expr =
    Iarray.fold_right (go_expr ~tail_call:false env) exprs
  in

  let expr = go_expr ~tail_call:true env expr [] in
  Dynarray.to_list seen_locals, expr

let translate_fun
  (opts : Opts.t)
  (env : Env.t)
  (name : Core.Item_name.t)
  (params : (string option * Prim.Ty.t) Iarray.t)
  (ty : Prim.Ty.t)
  (body : Core.Expr.t)
: Wat.func =
  let export = Core.Item_name.to_string name in
  let name = Env.lookup_item_id name env in
  let result = translate_ty ty in

  let params = params |> Iarray.map (fun (name, ty) -> fresh_local_id name, ty) in
  let env = Iarray.fold_right Env.extend params env in
  let params = Iarray.to_seq params |> Seq.map (Pair.map_snd translate_ty) |> List.of_seq in

  let locals, body = translate_expr opts env body in

  Wat.{ name; export; params; result; locals; body }

let translate_module ~(enable_tail_call : bool) (mod_ : Core.Module.t) : Wat.module_ =
  let item_ids =
    mod_ |> Core.Item_map.mapi @@ fun id _ ->
      Wat.Func_id.fresh (Core.Item_name.to_string id)
  in

  let opts = Opts.{ enable_tail_call } in

  let env = Env.empty
    ~item_ids:(fun name -> Core.Item_map.find name item_ids)
    ~item_tys:(fun name -> Core.Module.item_ty mod_ name)
  in

  let translate_item (name, item) =
    match item with
    (** FIXME: re-evaluation of top-level values.

        Possible fixes:
        - normalise expressions (using NbE) and store in global
        - create a global and initialise with a startup function
    *)
    | Core.Item.Val (ty, expr) -> translate_fun opts env name [||] ty expr
    | Core.Item.Fun (params, ty, body) -> translate_fun opts env name params ty body
  in

  Wat.{
    funcs =
      Core.Item_map.to_seq mod_
      |> Seq.map translate_item
      |> List.of_seq;
  }
