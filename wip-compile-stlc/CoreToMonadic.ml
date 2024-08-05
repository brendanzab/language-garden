(** Translation from the core language into monadic form. *)

module SrcEnv = struct

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

end

let ( let@ ) : type a b. (a -> b) -> a -> b =
  ( @@ )

let rec translate (env : SrcEnv.t) (expr : Core.expr) : Monadic.expr =
  match expr with
  | Var index ->
      Atom (Var (List.nth env.ids index))
  | Let (def_name, def_ty, def, body) ->
      let def_id = Monadic.Id.fresh () in
      Let (def_name, def_id, def_ty, translate env def,
        translate (SrcEnv.extend def_id def_ty env) body)
  | PrimApp (prim, args) ->
      let@ args = translate_names env "arg" args in
      Monadic.PrimApp (prim, args)
  | FunApp (head, arg) ->
      let@ head = translate_name env "head" head in
      let@ arg = translate_name env "arg" arg in
      Monadic.FunApp (head, arg)
  | FunLit (param_name, param_ty, body) ->
      let param_id = Monadic.Id.fresh () in
      Atom (FunLit (param_name, param_id, param_ty,
        translate (SrcEnv.extend param_id param_ty env) body))
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

and translate_name (env : SrcEnv.t) (name : string) (expr : Core.expr) (k : Monadic.aexpr -> Monadic.expr) : Monadic.expr =
  let expr_id = Monadic.Id.fresh () in
  let expr_ty = Core.type_of env.tys expr in
  match translate env expr with
  | Atom expr -> k expr
  | expr -> Let (Machine name, expr_id, expr_ty, expr, k (Var expr_id))

and translate_names (env : SrcEnv.t) (name : string) (exprs : Core.expr list) (k : Monadic.aexpr list -> Monadic.expr) : Monadic.expr =
  match exprs with
  | [] -> k []
  | expr :: exprs ->
      let@ expr = translate_name env name expr in
      let@ exprs = translate_names env name exprs in
      k (expr :: exprs)

let translate (expr : Core.expr) : Monadic.expr =
  translate SrcEnv.empty expr
