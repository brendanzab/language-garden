(** {0 Surface language} *)

(** {1 Syntax} *)

(** The start and end position in a source file *)
type loc =
  Lexing.position * Lexing.position

(** Located nodes *)
type 'a located = {
  loc : loc;
  data : 'a;
}

(** Terms in the surface language *)
type tm =
  tm_data located

and tm_data =
  | Name of string
  | Let of string * string list * tm * tm
  | IntLit of int
  | FunLit of string list * tm
  | FunApp of tm * tm
  | Op2 of [`Add | `Sub | `Mul] * tm * tm
  | Op1 of [`Neg] * tm


(** {1 Elaboration} *)

(** A stack of bindings currently in scope *)
type context =
  (string * Core.ty) Core.env

(** Lookup a name in the context *)
let lookup (context : context) (name : string) : (Core.index * Core.ty) option =
  let rec go index context =
    match context with
    | (name', ty) :: _ when name = name' -> Some (index, ty)
    | (_, _) :: context -> go (index + 1) context
    | [] -> None
  in
  go 0 context

(** An error thrown during elaboration *)
exception Error of loc * string

(** Elaborate a surface term into a core term, given an expected type. *)
let rec check (context : context) (tm : tm) (ty : Core.ty) : Core.tm =
  let tm', ty' = infer context tm in
  try Core.unify ty ty'; tm' with
  | Core.InfiniteType _ ->
      raise (Error (tm.loc, "infinite type"))
  | Core.MismatchedTypes (_, _) ->
      raise (Error
        (tm.loc, Format.asprintf "@[<v 2>@[mismatched types:@]@ @[expected: %a@]@ @[found: %a@]@]"
          Core.pp_ty (Core.zonk_ty ty)
          Core.pp_ty (Core.zonk_ty ty')))

(** Elaborate a surface term into a core term, inferring its type. *)
and infer (context : context) (tm : tm) : Core.tm * Core.ty =
  match tm.data with
  | Name name ->
      begin match lookup context name with
      | Some (index, ty) -> Var index, ty
      | None -> raise (Error (tm.loc, Format.asprintf "unbound name `%s`" name))
      end
  | Let (def_name, param_names, def_body, body) ->
      let def, def_ty = infer_fun_lit context param_names def_body in
      let body, body_ty = infer ((def_name, def_ty) :: context) body in
      Let (def_name, def_ty, def, body), body_ty
  | IntLit i -> IntLit i, IntType
  | FunLit (param_names, body) ->
      infer_fun_lit context param_names body
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

(** Elaborate a function literal, inferring its type. *)
and infer_fun_lit (context : context) (names : string list) (body : tm) : Core.tm * Core.ty =
  match names with
  | [] -> infer context body
  | name :: names ->
      let param_ty = Core.fresh_meta () in
      let body, body_ty = infer_fun_lit ((name, param_ty) :: context) names body in
      FunLit (name, param_ty, body), FunType (param_ty, body_ty)

(* TODO: check unsolved metas - perhaps store in a metacontext? *)
