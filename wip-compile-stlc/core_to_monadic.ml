(** Translation from the core language into monadic form. *)

module Src_env : sig

  type t

  val empty : t
  val extend : Monadic.id -> Core.ty -> t -> t

  val lookup_id : int -> t -> Monadic.id
  val type_of : Core.expr -> t -> Monadic.ty

end = struct

  type t = {
    ids : Monadic.id list;  (* mapping from source variables to target ids *)
    tys : Core.ty list;     (* mapping from source variables to source types *)
  }

  let empty : t = {
    ids = [];
    tys = [];
  }

  let extend (id : Monadic.id) (ty : Core.ty) (env : t) = {
    ids = id :: env.ids;
    tys = ty :: env.tys;
  }

  let lookup_id (index : Core.index) (env : t) : Monadic.id =
    List.nth env.ids index

  let type_of (expr : Core.expr) (env : t) : Anf.ty =
    Core.type_of env.tys expr

end

let ( let@ ) : type a b. (a -> b) -> a -> b =
  ( @@ )

let rec translate (env : Src_env.t) (expr : Core.expr) : Monadic.expr =
  match expr with
  | Var index ->
      Atom (Var (Src_env.lookup_id index env))
  | Prim prim ->
      Atom (Prim prim)
  | Let (def_name, def_ty, def, body) ->
      let def_id = Monadic.Id.fresh () in
      Let (def_name, def_id, def_ty, translate env def,
        translate (Src_env.extend def_id def_ty env) body)
  | FunLit (param_name, param_ty, body) ->
      let param_id = Monadic.Id.fresh () in
      Atom (FunLit (param_name, param_id, param_ty,
        translate (Src_env.extend param_id param_ty env) body))
  | FunApp (head, arg) ->
      let@ head = translate_name env "head" head in
      let@ arg = translate_name env "arg" arg in
      Monadic.FunApp (head, arg)
  | TupleLit elems ->
      let@ elems = translate_names env "elem" elems in
      Monadic.Atom (TupleLit elems)
  | TupleProj (head, label) ->
      let@ head = translate_name env "head" head in
      Monadic.TupleProj (head, label)
  | BoolLit b ->
      Atom (BoolLit b)
  | BoolElim (head, on_true, on_false) ->
      let@ head = translate_name env "head" head in
      Monadic.BoolElim (head, translate env on_true, translate env on_false)
  | IntLit i ->
      Atom (IntLit i)

and translate_name (env : Src_env.t) (name : string) (expr : Core.expr) (k : Monadic.aexpr -> Monadic.expr) : Monadic.expr =
  let expr_id = Monadic.Id.fresh () in
  let expr_ty = Src_env.type_of expr env in
  match translate env expr with
  | Atom expr -> k expr
  | expr -> Let (Machine name, expr_id, expr_ty, expr, k (Var expr_id))

and translate_names (env : Src_env.t) (name : string) (exprs : Core.expr list) (k : Monadic.aexpr list -> Monadic.expr) : Monadic.expr =
  match exprs with
  | [] -> k []
  | expr :: exprs ->
      let@ expr = translate_name env name expr in
      let@ exprs = translate_names env name exprs in
      k (expr :: exprs)

let translate (expr : Core.expr) : Monadic.expr =
  translate Src_env.empty expr
