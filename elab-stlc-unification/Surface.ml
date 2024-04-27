(** {0 Surface language}

    The surface language closely mirrors what the programmer originaly wrote,
    including syntactic sugar and higher level language features that make
    programming more convenient (in comparison to the {!Core}).
*)

(** {1 Syntax} *)

(** The start and end position in a source file *)
type loc =
  Lexing.position * Lexing.position

(** Located nodes *)
type 'a located = {
  loc : loc;
  data : 'a;
}

(** Types in the surface language *)
type ty =
  ty_data located

and ty_data =
  | Name of string
  | FunType of ty * ty
  | Placeholder

(** Names that bind definitions or parameters *)
type binder = string located

(** Terms in the surface language *)
type tm =
  tm_data located

and tm_data =
  | Name of string
  | Let of binder * param list * ty option * tm * tm
  | Ann of tm * ty
  | FunLit of param list * tm
  | IntLit of int
  | BoolLit of bool
  | App of tm * tm
  | IfThenElse of tm * tm * tm
  | Op2 of [`Eq | `Add | `Sub | `Mul] * tm * tm
  | Op1 of [`Neg] * tm

(** Parameters, with optional type annotations *)
and param =
  binder * ty option


(** {1 Elaboration} *)

(** This is where we implement user-facing type checking, while also translating
    the surface language into the simpler, more explicit core language.

    While we {e could} translate syntactic sugar in the parser, by leaving
    this to elaboration time we make it easier to report higher quality error
    messages that are more relevant to what the programmer originally wrote.
*)

(** {2 Metavariables} *)

(** The reason why a metavariable was inserted *)
type meta_info = [
  | `FunParam
  | `FunBody
  | `IfBranches
  | `Placeholder
]

(** A global list of the metavariables inserted during elaboration. This is used
    to generate a list of unsolved metavariables at the end of elaboration. *)
let metas : (loc * meta_info * Core.meta_state ref) list ref = ref []

(** Generate a fresh metavariable, recording it in the list of metavariables *)
let fresh_meta (loc: loc) (info : meta_info) : Core.ty =
  let state = Core.fresh_meta () in
  metas := (loc, info, state) :: !metas;
  MetaVar state

(** Return a list of unsolved metavariables *)
let unsolved_metas () : (loc * meta_info) list =
  let rec go acc metas =
    match metas with
    | [] -> acc
    | (loc, info, m) :: metas -> begin
        match !m with
        | Core.Unsolved _ -> go ((loc, info) :: acc) metas
        | Core.Solved _ -> go acc metas
    end
  in
  go [] !metas


(** {2 Local bindings} *)

(** A stack of bindings currently in scope *)
type context = (string * Core.ty) Core.env

(** Lookup a name in the context *)
let lookup (ctx : context) (name : string) : (Core.index * Core.ty) option =
  ctx |> List.find_mapi @@ fun index (name', ty) ->
    match name = name' with
    | true -> Some (index, ty)
    | false -> None


(** {2 Elaboration errors} *)

(** An error that will be raised if there was a problem in the surface syntax,
    usually as a result of type errors. This is normal, and should be rendered
    nicely to the programmer. *)
exception Error of loc * string

(** Raises an {!Error} exception *)
let error (loc : loc) (message : string) : 'a =
  raise (Error (loc, message))

let unify (loc : loc) (ty1 : Core.ty) (ty2 : Core.ty) =
  try Core.unify ty1 ty2 with
  | Core.InfiniteType _ -> error loc "infinite type"
  | Core.MismatchedTypes (_, _) ->
      error loc
        (Format.asprintf "@[<v 2>@[mismatched types:@]@ @[expected: %a@]@ @[found: %a@]@]"
          Core.pp_ty (Core.zonk_ty ty1)
          Core.pp_ty (Core.zonk_ty ty2))


(** {2 Bidirectional type checking} *)

(** The algorithm is structured {i bidirectionally}, divided into mutually
    recursive {i checking} and {i inference} modes. By supplying type
    annotations as early as possible using the checking mode, we can improve
    the locality of type errors. We can also extend the type system with
    advanced features like dependent types, higher rank types, and subtyping
    while maintaining decidability by allowing the programmer to supply
    annotations where necessary. *)

(** Elaborate a type, checking that it is well-formed. *)
let rec elab_ty (ty : ty) : Core.ty =
  match ty.data with
  | Name "Bool" -> BoolType
  | Name "Int" -> IntType
  | Name name ->
      error ty.loc (Format.asprintf "unbound type `%s`" name)
  | FunType (ty1, ty2) ->
      FunType (elab_ty ty1, elab_ty ty2)
  | Placeholder ->
      fresh_meta ty.loc `Placeholder

(** Elaborate a surface term into a core term, given an expected type. *)
let rec elab_check (ctx : context) (tm : tm) (ty : Core.ty) : Core.tm =
  match tm.data with
  | Let (def_name, params, def_body_ty, def_body, body) ->
      let def, def_ty = elab_infer_fun_lit ctx params def_body_ty def_body in
      let body = elab_check ((def_name.data, def_ty) :: ctx) body ty in
      Let (def_name.data, def_ty, def, body)

  | FunLit (params, body) ->
      elab_check_fun_lit ctx params body ty

  | IfThenElse (head, tm1, tm2) ->
      let head = elab_check ctx head BoolType in
      let tm1 = elab_check ctx tm1 ty in
      let tm2 = elab_check ctx tm2 ty in
      BoolElim (head, tm1, tm2)

  (* Fall back to type inference *)
  | _ ->
      let tm', ty' = elab_infer ctx tm in
      unify tm.loc ty ty';
      tm'

(** Elaborate a surface term into a core term, inferring its type. *)
and elab_infer (ctx : context) (tm : tm) : Core.tm * Core.ty =
  match tm.data with
  | Name name -> begin
      match lookup ctx name with
      | Some (index, ty) -> Var index, ty
      | None -> error tm.loc (Format.asprintf "unbound name `%s`" name)
  end

  | Let (def_name, params, def_body_ty, def_body, body) ->
      let def, def_ty = elab_infer_fun_lit ctx params def_body_ty def_body in
      let body, body_ty = elab_infer ((def_name.data, def_ty) :: ctx) body in
      Let (def_name.data, def_ty, def, body), body_ty

  | Ann (tm, ty) ->
      let ty = elab_ty ty in
      elab_check ctx tm ty, ty

  | FunLit (params, body) ->
      elab_infer_fun_lit ctx params None body

  | IntLit i ->
      IntLit i, IntType

  | BoolLit b ->
      BoolLit b, BoolType

  | App (head, arg) ->
      let head_loc = head.loc in
      let head, head_ty = elab_infer ctx head in
      let param_ty, body_ty =
        match Core.force head_ty with
        | FunType (param_ty, body_ty) -> param_ty, body_ty
        | head_ty ->
            let param_ty = fresh_meta head_loc `FunParam in
            let body_ty = fresh_meta head_loc `FunBody in
            unify head_loc head_ty (FunType (param_ty, body_ty));
            param_ty, body_ty
      in
      let arg = elab_check ctx arg param_ty in
      FunApp (head, arg), body_ty

  | IfThenElse (head, tm1, tm2) ->
      let head = elab_check ctx head BoolType in
      let ty = fresh_meta tm.loc `IfBranches in
      let tm1 = elab_check ctx tm1 ty in
      let tm2 = elab_check ctx tm2 ty in
      BoolElim (head, tm1, tm2), ty

  | Op2 ((`Eq) as prim, tm1, tm2) ->
      let tm1 = elab_check ctx tm1 IntType in
      let tm2 = elab_check ctx tm2 IntType in
      PrimApp (prim, [tm1; tm2]), BoolType

  | Op2 ((`Add | `Sub | `Mul) as prim, tm1, tm2) ->
      let tm1 = elab_check ctx tm1 IntType in
      let tm2 = elab_check ctx tm2 IntType in
      PrimApp (prim, [tm1; tm2]), IntType

  | Op1 ((`Neg) as prim, tm) ->
      let tm = elab_check ctx tm IntType in
      PrimApp (prim, [tm]), IntType

(** Elaborate a function literal into a core term, given an expected type. *)
and elab_check_fun_lit (ctx : context) (params : param list) (body : tm) (ty : Core.ty) : Core.tm =
  match params, Core.force ty with
  | [], ty ->
      elab_check ctx body ty
  | (name, None) :: params, FunType (param_ty, body_ty) ->
      let body = elab_check_fun_lit ((name.data, param_ty) :: ctx) params body body_ty in
      FunLit (name.data, param_ty, body)
  | (name, Some param_ty) :: params, FunType (param_ty', body_ty) ->
      let param_ty_loc = param_ty.loc in
      let param_ty = elab_ty param_ty in
      unify param_ty_loc param_ty param_ty';
      let body = elab_check_fun_lit ((name.data, param_ty) :: ctx) params body body_ty in
      FunLit (name.data, param_ty, body)
  | (name, _) :: _, MetaVar _ ->
      let tm', ty' = elab_infer_fun_lit ctx params None body in
      unify name.loc ty ty';
      tm'
  | (name, _) :: _, _ ->
      error name.loc "unexpected parameter"

(** Elaborate a function literal, inferring its type. *)
and elab_infer_fun_lit (ctx : context) (params : param list) (body_ty : ty option) (body : tm) : Core.tm * Core.ty =
  match params, body_ty with
  | [], Some body_ty ->
      let body_ty = elab_ty body_ty in
      elab_check ctx body body_ty, body_ty
  | [], None ->
      elab_infer ctx body
  | (name, param_ty) :: params, body_ty ->
      let param_ty = match param_ty with
        | None -> fresh_meta name.loc `FunParam
        | Some ty -> elab_ty ty
      in
      let body, body_ty = elab_infer_fun_lit ((name.data, param_ty) :: ctx) params body_ty body in
      FunLit (name.data, param_ty, body), FunType (param_ty, body_ty)
