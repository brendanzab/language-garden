(** {0 Alpha-renaming translation}

    This translation assigns unique names to each variable binding as a way to
    simplify later compilation passes.
*)

module Fun = Lang.Fun
module Fun_a = Lang.Fun_a


(** {1 Translation} *)

let rec translate env : Fun.tm -> Fun_a.tm =
  function
  | Var index -> Var (List.nth env index)
  | Let (def_name, def_ty, def, body) ->
      let def_var = Fun_a.Var.fresh def_name in
      let def = translate env def in
      let body = translate (def_var :: env) body in
      Let (def_var, def_ty, def, body)
  | Bool_lit b -> Bool_lit b
  | Int_lit i -> Int_lit i
  | Prim_app (prim, args) ->
      let args = List.map (translate env) args in
      Prim_app (prim, args)
  | Fun_lit (param_name, param_ty, body) ->
      let param_var = Fun_a.Var.fresh param_name in
      let body = translate (param_var :: env) body in
      Fun_lit (param_var, param_ty, body)
  | Fun_app (head, arg) ->
      let head = translate env head in
      let arg = translate env arg in
      Fun_app (head, arg)
