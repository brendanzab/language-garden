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

type binder = string located

(** Terms in the surface language *)
type tm =
  tm_data located

and tm_data =
  | Name of string
  | Let of binder * binder list * tm * tm
  | IntLit of int
  | FunLit of binder list * tm
  | FunApp of tm * tm
  | Op2 of [`Add | `Sub | `Mul] * tm * tm
  | Op1 of [`Neg] * tm


(** {1 Elaboration} *)

(** The reason why a metavariable was inserted *)
type meta_info = [
  | `FunParam of loc
  | `FunBody of loc
]

(** A global list of the metavariables inserted during elaboration. This is used
    to generate a list of unsolved metavariables at the end of elaboration. *)
let metas : (meta_info * Core.meta_state ref) list ref = ref []

(** Generate a fresh metavariable, recording it in the list of metavariables *)
let fresh_meta (info : meta_info) : Core.ty =
  let state = Core.fresh_meta () in
  metas := (info, state) :: !metas;
  MetaVar state

(** Return a list of unsolved metavariables *)
let unsolved_metas () : meta_info list =
  let rec go acc =
    function
    | [] -> acc
    | (info, m) :: metas ->
        begin match !m with
        | Core.Unsolved _ -> go (info :: acc) metas
        | Core.Solved _ -> go acc metas
        end
  in
  go [] !metas

(** A stack of bindings currently in scope *)
type context = (string * Core.ty) Core.env

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
      let body, body_ty = infer ((def_name.data, def_ty) :: context) body in
      Let (def_name.data, def_ty, def, body), body_ty
  | IntLit i -> IntLit i, IntType
  | FunLit (param_names, body) ->
      infer_fun_lit context param_names body
  | FunApp (head, arg) ->
      let arg, arg_ty = infer context arg in
      let body_ty = fresh_meta (`FunBody tm.loc) in
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
and infer_fun_lit (context : context) (names : binder list) (body : tm) : Core.tm * Core.ty =
  match names with
  | [] -> infer context body
  | name :: names ->
      let param_ty = fresh_meta (`FunParam name.loc) in
      let body, body_ty = infer_fun_lit ((name.data, param_ty) :: context) names body in
      FunLit (name.data, param_ty, body), FunType (param_ty, body_ty)
