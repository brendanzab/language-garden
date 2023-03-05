(** {0 Surface language} *)

(** Terms in the surface language *)
type tm =
  | Name of string
  | Let of string * tm * tm
  | IntLit of int
  | FunLit of string * tm
  | FunApp of tm * tm
  | Op2 of [`Add | `Sub | `Mul] * tm * tm
  | Op1 of [`Neg] * tm


(** {1 Elaboration} *)

(** A stack of bindings currently in scope *)
type context =
  (string * Core.ty) Core.env

(** An error thrown during elaboration *)
exception Error of string

(** Elaborate a surface term into a core term, given an expected type. *)
let rec check (context : context) (tm : tm) (ty : Core.ty) : Core.tm =
  let tm, ty' = infer context tm in
  try Core.unify ty ty'; tm with
  | Core.InfiniteType _ ->
      raise (Error
        (Format.asprintf "@[<v 2>@[infinite type:@]@ @[expected: %a@]@ @[found: %a@]@]"
          Core.pp_ty (Core.zonk_ty ty)
          Core.pp_ty (Core.zonk_ty ty')))
  | Core.MismatchedTypes (_, _) ->
      raise (Error
        (Format.asprintf "@[<v 2>@[mismatched types:@]@ @[expected: %a@]@ @[found: %a@]@]"
          Core.pp_ty (Core.zonk_ty ty)
          Core.pp_ty (Core.zonk_ty ty')))

(** Elaborate a surface term into a core term, inferring the type. *)
and infer (context : context) (tm : tm) : Core.tm * Core.ty =
  match tm with
  | Name name ->
      let rec go index context : Core.tm * Core.ty =
        match context with
        | (name', ty) :: _ when name = name' -> Var index, ty
        | (_, _) :: context -> go (index + 1) context
        | [] -> raise (Error (Format.asprintf "the variable `%s` is not bound in the current scope" name))
      in
      go 0 context
  | Let (def_name, def, body) ->
      let def, def_ty = infer context def in
      let body, body_ty = infer ((def_name, def_ty) :: context) body in
      Let (def_name, def_ty, def, body), body_ty
  | IntLit i -> IntLit i, IntType
  | FunLit (param_name, body) ->
      let param_ty = Core.fresh_meta () in
      let body, body_ty = infer ((param_name, param_ty) :: context) body in
      FunLit (param_name, param_ty, body), FunType (param_ty, body_ty)
  | FunApp (head, arg) ->
      let arg, arg_ty = infer context arg in
      let body_ty = Core.fresh_meta () in
      let head = check context head (FunType (arg_ty, body_ty)) in
      FunApp (head, arg), body_ty
  | Op2 ((`Add | `Sub | `Mul) as prim, tm0, tm1) ->
      let tm0 = check context tm0 IntType in
      let tm1 = check context tm1 IntType in
      PrimApp (prim, [tm0; tm1]), IntType
  | Op1 ((`Neg) as prim, tm) ->
      let tm = check context tm IntType in
      PrimApp (prim, [tm]), IntType

(* TODO: check unsolved metas - perhaps store in a matacontext? *)
