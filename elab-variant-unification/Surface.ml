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

(** Labels that distinguish variant cases, record fields, etc. *)
type label = string located

(** Names that bind definitions or parameters *)
type binder = string located

(** Types in the surface language *)
type ty =
  ty_data located

and ty_data =
  | Name of string
  | FunType of ty * ty
  | VariantType of (label * ty) list
  | Placeholder

type pattern =
  pattern_data located

and pattern_data =
  (* | Name of string *)
  (* | Placeholder *)
  (* | IntLit of int *)
  | VariantLit of label * binder

(** Terms in the surface language *)
type tm =
  tm_data located

and tm_data =
  | Name of string
  | Let of binder * param list * ty option * tm * tm
  | Ann of tm * ty
  | VariantLit of label * tm
  | Match of tm * (pattern * tm) list
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
  | `VariantLit
  | `MatchClauses
  | `PatternBinder
  | `IfBranches
  | `Placeholder
]

(** A global list of the metavariables inserted during elaboration. This is used
    to generate a list of unsolved metavariables at the end of elaboration. *)
let metas : (loc * meta_info * Core.meta_state ref) list ref = ref []

(** Generate a fresh metavariable, recording it in the list of metavariables *)
let fresh_meta (loc: loc) (info : meta_info) (constr : Core.constr) : Core.ty =
  let state = Core.fresh_meta constr in
  metas := (loc, info, state) :: !metas;
  MetaVar state

(** Return a list of unsolved metavariables *)
let unsolved_metas () : (loc * meta_info) list =
  let rec go acc metas =
    match metas with
    | [] -> acc
    | (loc, info, m) :: metas -> begin
        match !m with
        | Core.Unsolved (_, Any) -> go ((loc, info) :: acc) metas
        | Core.Unsolved (_, Variant cases) ->
            (* Default to a concrete variant type *)
            m := Solved (VariantType cases);
            go acc metas
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


(** {2 Bidirectional type checking} *)

(** The algorithm is structured {i bidirectionally}, divided into mutually
    recursive {i checking} and {i inference} modes. By supplying type
    annotations as early as possible using the checking mode, we can improve
    the locality of type errors. It also allows us to extend the type system
    advanced features like dependent types, higher rank types, and subtyping
    while maintaining decidability. *)

(** Elaborate a type, checking that it is well-formed. *)
let rec elab_ty (ty : ty) : Core.ty =
  match ty.data with
  | Name "Bool" -> BoolType
  | Name "Int" -> IntType
  | Name name ->
      error ty.loc (Format.asprintf "unbound type `%s`" name)
  | FunType (ty1, ty2) ->
      FunType (elab_ty ty1, elab_ty ty2)
  | VariantType cases ->
      VariantType
        (List.fold_left
          (fun acc (label, ty) ->
            if Core.LabelMap.mem label.data acc then
              error label.loc (Format.asprintf "duplicate label `%s`" label.data)
            else
              Core.LabelMap.add label.data (elab_ty ty) acc)
          Core.LabelMap.empty
          cases)
  | Placeholder ->
      fresh_meta ty.loc `Placeholder Any

(** Elaborate a surface term into a core term, given an expected type. *)
let rec elab_check (context : context) (tm : tm) (ty : Core.ty) : Core.tm =
  match tm.data, Core.force ty with
  | Let (def_name, params, def_body_ty, def_body, body), ty ->
      let def, def_ty = elab_infer_fun_lit context params def_body_ty def_body in
      let body = elab_check ((def_name.data, def_ty) :: context) body ty in
      Let (def_name.data, def_ty, def, body)

  | VariantLit (label, tm), VariantType cases -> begin
      match Core.LabelMap.find_opt label.data cases with
      | Some elem_ty -> VariantLit (label.data, elab_check context tm elem_ty, ty)
      | None ->
          error label.loc
            (Format.asprintf "unexpected variant `%s` in type `%a`"
              label.data
              Core.pp_ty (Core.zonk_ty ty))
  end

  | IfThenElse (head, tm0, tm1), ty ->
      let head = elab_check context head BoolType in
      let tm0 = elab_check context tm0 ty in
      let tm1 = elab_check context tm1 ty in
      BoolElim (head, tm0, tm1)

  | FunLit (params, body), ty ->
      elab_check_fun_lit context params body ty

  (* Fall back to type inference *)
  | _ ->
      let tm', ty' = elab_infer context tm in
      (* NOTE: We could add some subtyping coercions here *)
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

  | Let (def_name, params, def_body_ty, def_body, body) ->
      let def, def_ty = elab_infer_fun_lit context params def_body_ty def_body in
      let body, body_ty = elab_infer ((def_name.data, def_ty) :: context) body in
      Let (def_name.data, def_ty, def, body), body_ty

  | Ann (tm, ty) ->
      let ty = elab_ty ty in
      elab_check context tm ty, ty

  | VariantLit (label, elem_tm) ->
      let elem_tm, elem_ty = elab_infer context elem_tm in
      let cases = Core.LabelMap.singleton label.data elem_ty in
      let ty = fresh_meta tm.loc `VariantLit (Variant cases) in
      VariantLit (label.data, elem_tm, ty), ty

  | Match (head, clauses) -> begin
      let head_loc = head.loc in
      let head, head_ty = elab_infer context head in
      let body_ty = fresh_meta tm.loc `MatchClauses Any in
      (* TDOD: Proper match compilation *)
      match Core.force head_ty with
      | VariantType ty_cases ->
          (* iterate through clauses, accumulating term cases *)
          let tm_cases =
            List.fold_left
              (fun tm_cases ({ data = VariantLit (label, name); _}, body_tm : pattern * _) ->
                if Core.LabelMap.mem label.data tm_cases then
                  (* TODO: should be a warning *)
                  error label.loc (Format.asprintf "redundant variant pattern `%s`" label.data)
                else
                  match Core.LabelMap.find_opt label.data ty_cases with
                  | Some case_ty ->
                      let body_tm = elab_check ((name.data, case_ty) :: context) body_tm body_ty in
                      Core.LabelMap.add label.data (name.data, body_tm) tm_cases
                  | None ->
                      error label.loc (Format.asprintf "unexpected variant pattern `%s`" label.data))
              Core.LabelMap.empty
              clauses
          in
          (* check that labels in term cases match type cases *)
          let missing_cases =
            Core.LabelMap.to_seq ty_cases
            |> Seq.filter (fun (label, _) -> not (Core.LabelMap.mem label tm_cases))
            |> List.of_seq
          in
          if List.is_empty missing_cases then
            (* return term cases *)
            VariantElim (head, tm_cases), body_ty
          else
            error tm.loc
              (Format.asprintf "non-exhaustive match, missing %a"
                (Format.pp_print_list
                  ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
                  (fun fmt (label, _) -> Format.fprintf fmt "`%s`" label))
                missing_cases)

      | head_ty ->
          (* Build up the labelled types and and term cases from the clauses *)
          let tm_cases, ty_cases =
            List.fold_left
              (fun (tm_cases, ty_cases) ({ data = VariantLit (label, name); _}, body_tm : pattern * _) ->
                if Core.LabelMap.mem label.data tm_cases then
                  (* TODO: should be a warning? *)
                  error label.loc (Format.asprintf "redundant variant pattern `%s`" label.data)
                else
                  let case_ty = fresh_meta name.loc `PatternBinder Any in
                  let body_tm = elab_check ((name.data, case_ty) :: context) body_tm body_ty in
                  Core.LabelMap.add label.data (name.data, body_tm) tm_cases,
                  Core.LabelMap.add label.data case_ty ty_cases)
              (Core.LabelMap.empty, Core.LabelMap.empty)
              clauses
          in
          (* Unify head type with variant type *)
          unify head_loc head_ty (VariantType ty_cases);
          (* return term cases *)
          VariantElim (head, tm_cases), body_ty
  end

  | BoolLit b ->
      BoolLit b, BoolType

  | IfThenElse (head, tm0, tm1) ->
      let head = elab_check context head BoolType in
      let ty = fresh_meta tm.loc `IfBranches Any in
      let tm0 = elab_check context tm0 ty in
      let tm1 = elab_check context tm1 ty in
      BoolElim (head, tm0, tm1), ty

  | IntLit i ->
      IntLit i, IntType

  | FunLit (params, body) ->
      elab_infer_fun_lit context params None body

  | FunApp (head, arg) ->
      let head_loc = head.loc in
      let head, head_ty = elab_infer context head in
      let param_ty, body_ty =
        match head_ty with
        | FunType (param_ty, body_ty) -> param_ty, body_ty
        | head_ty ->
            let param_ty = fresh_meta head_loc `FunParam Any in
            let body_ty = fresh_meta head_loc `FunBody Any in
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
  match params, Core.force ty with
  | [], ty ->
      elab_check context body ty
  | (name, None) :: params, FunType (param_ty, body_ty) ->
      let body = elab_check_fun_lit ((name.data, param_ty) :: context) params body body_ty in
      FunLit (name.data, param_ty, body)
  | (name, Some param_ty) :: params, FunType (param_ty', body_ty) ->
      let param_ty_loc = param_ty.loc in
      let param_ty = elab_ty param_ty in
      unify param_ty_loc param_ty param_ty';
      let body = elab_check_fun_lit ((name.data, param_ty) :: context) params body body_ty in
      FunLit (name.data, param_ty, body)
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
        | None -> fresh_meta name.loc `FunParam Any
        | Some ty -> elab_ty ty
      in
      let body, body_ty = elab_infer_fun_lit ((name.data, param_ty) :: context) params body_ty body in
      FunLit (name.data, param_ty, body), FunType (param_ty, body_ty)
