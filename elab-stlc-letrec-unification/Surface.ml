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
  | LetRec of binder * param list * ty option * tm * tm
  | Ann of tm * ty
  | BoolLit of bool
  | IfThenElse of tm * tm * tm
  | IntLit of int
  | FunLit of param list * tm
  | FunApp of tm * tm
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
let lookup (context : context) (name : string) : (Core.index * Core.ty) option =
  let rec go index context =
    match context with
    | (name', ty) :: _ when name = name' -> Some (index, ty)
    | (_, _) :: context -> go (index + 1) context
    | [] -> None
  in
  go 0 context


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

(** {2 Type checking} *)

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

(** In this algorithm type checking is mainly unidirectional, relying on the
    [infer] funtion, but a {!check} function is provided for convenience.
*)

(** Elaborate a surface term into a core term, given an expected type. *)
let rec elab_check (context : context) (tm : tm) (ty : Core.ty) : Core.tm =
  let tm', ty' = elab_infer context tm in
  unify tm.loc ty ty';
  tm'

(** Elaborate a surface term into a core term, inferring its type. *)
and elab_infer (context : context) (tm : tm) : Core.tm * Core.ty =
  match tm.data with
  | Name name -> begin
      match lookup context name with
      | Some (index, ty) -> Var index, ty
      | None -> error tm.loc (Format.asprintf "unbound name `%s`" name)
  end
  | Let (def_name, params, def_ty, def, body) ->
      let def, def_ty = elab_infer_fun_lit context params def_ty def in
      let body, body_ty = elab_infer ((def_name.data, def_ty) :: context) body in
      Let (def_name.data, def_ty, def, body), body_ty

  | LetRec (def_name, params, def_ty, def, body) ->
      (* Creates a fresh function type for the definition. *)
      let rec fresh_fun_ty context loc params body_ty =
        match params, body_ty with
        | [], Some body_ty -> elab_ty body_ty
        | [], None -> fresh_meta loc `FunBody
        | (name, _) :: params, body_ty ->
            let param_ty = fresh_meta name.loc `FunParam in
            FunType (param_ty, fresh_fun_ty ((name.data, param_ty) :: context) loc params body_ty)
      in

      (* Elaborate the definition to a fixed-point combinator *)
      let def_ty = fresh_fun_ty context def_name.loc params def_ty in
      let def_body =
        match elab_check_fun_lit ((def_name.data, def_ty) :: context) params def def_ty with
        | FunLit _ as def_body -> def_body
        | _ -> error tm.loc "expected function literal in recursive let binding"
      in
      let def = Core.Fix (def_name.data, def_ty, def_body) in

      (* Elaborate the body of the let just like in the non-recursive case. *)
      let body, body_ty = elab_infer ((def_name.data, def_ty) :: context) body in

      Let (def_name.data, def_ty, def, body), body_ty

  | Ann (tm, ty) ->
      let ty = elab_ty ty in
      elab_check context tm ty, ty
  | BoolLit b -> BoolLit b, BoolType
  | IfThenElse (head, tm0, tm1) ->
      let head = elab_check context head BoolType in
      let ty = fresh_meta tm.loc `IfBranches in
      let tm0 = elab_check context tm0 ty in
      let tm1 = elab_check context tm1 ty in
      BoolElim (head, tm0, tm1), ty
  | IntLit i -> IntLit i, IntType
  | FunLit (params, body) ->
      elab_infer_fun_lit context params None body
  | FunApp (head, arg) ->
      let head_loc = head.loc in
      let head, head_ty = elab_infer context head in
      let param_ty, body_ty =
        match head_ty with
        | FunType (param_ty, body_ty) -> param_ty, body_ty
        | head_ty ->
            let param_ty = fresh_meta head_loc `FunParam in
            let body_ty = fresh_meta head_loc `FunBody in
            unify head_loc head_ty (FunType (param_ty, body_ty));
            param_ty, body_ty
      in
      let arg = elab_check context arg param_ty in
      FunApp (head, arg), body_ty
  | Op2 ((`Eq) as prim, tm0, tm1) ->
      let tm0 = elab_check context tm0 IntType in
      let tm1 = elab_check context tm1 IntType in
      PrimApp (prim, [tm0; tm1]), BoolType
  | Op2 ((`Add | `Sub | `Mul) as prim, tm0, tm1) ->
      let tm0 = elab_check context tm0 IntType in
      let tm1 = elab_check context tm1 IntType in
      PrimApp (prim, [tm0; tm1]), IntType
  | Op1 ((`Neg) as prim, tm) ->
      let tm = elab_check context tm IntType in
      PrimApp (prim, [tm]), IntType

(** Elaborate a function literal into a core term, given an expected type. *)
and elab_check_fun_lit (context : context) (params : param list) (body : tm) (ty : Core.ty) : Core.tm =
  match params, ty with
  (* No more parameters, so elaborate the body of the function literal. *)
  | [], ty ->
      elab_check context body ty

  (* There’s no explicit parameter annotation, so pull it from the expected
     function type. *)
  | (name, None) :: params, FunType (param_ty, body_ty) ->
      let body = elab_check_fun_lit ((name.data, param_ty) :: context) params body body_ty in
      FunLit (name.data, param_ty, body)

  (* There’s an explicit parameter annotation, so make sure it matches the
     expected function type. *)
  | (name, Some param_ty) :: params, FunType (param_ty', body_ty) ->
      let param_ty_loc = param_ty.loc in
      let param_ty = elab_ty param_ty in
      unify param_ty_loc param_ty param_ty';
      let body = elab_check_fun_lit ((name.data, param_ty) :: context) params body body_ty in
      FunLit (name.data, param_ty, body)

  (* We weren’t expecting a function type, so this parameter was unexpected *)
  | (name, _) :: _, _ ->
      error name.loc "unexpected parameter"

(** Elaborate a function literal, inferring its type. *)
and elab_infer_fun_lit (context : context) (params : param list) (body_ty : ty option) (body : tm) : Core.tm * Core.ty =
  match params, body_ty with
  | [], Some body_ty ->
      let body_ty = elab_ty body_ty in
      elab_check context body body_ty, body_ty
  | [], None ->
      elab_infer context body
  | (name, param_ty) :: params, body_ty ->
      let param_ty = match param_ty with
        | None -> fresh_meta name.loc `FunParam
        | Some ty -> elab_ty ty
      in
      let body, body_ty = elab_infer_fun_lit ((name.data, param_ty) :: context) params body_ty body in
      FunLit (name.data, param_ty, body), FunType (param_ty, body_ty)
