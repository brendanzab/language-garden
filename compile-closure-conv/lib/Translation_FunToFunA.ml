(** {0 Alpha-renaming translation}

    This translation assigns unique names to each variable binding as a way to
    simplify later compilation passes.
*)

module Fun = Lang.Fun
module FunA = Lang.FunA

(** Generate a fresh variable *)
let fresh_var name : FunA.var =
  { name; id = FunA.Id.fresh () }


(** {1 Translation} *)

let rec translate env : Fun.tm -> FunA.tm =
  function
  | Var index -> Var (List.nth env index)
  | Let (def_name, def_ty, def, body) ->
      let def_var = fresh_var def_name in
      let def = translate env def in
      let body = translate (def_var :: env) body in
      Let (def_var, def_ty, def, body)
  | BoolLit b -> BoolLit b
  | IntLit i -> IntLit i
  | PrimApp (prim, args) ->
      let args = List.map (translate env) args in
      PrimApp (prim, args)
  | FunLit (param_name, param_ty, body) ->
      let param_var = fresh_var param_name in
      let body = translate (param_var :: env) body in
      FunLit (param_var, param_ty, body)
  | FunApp (head, arg) ->
      let head = translate env head in
      let arg = translate env arg in
      FunApp (head, arg)
