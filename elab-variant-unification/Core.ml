(** {0 Core language} *)

(** {1 Names} *)

(** Labels are significant to the equality of terms. They are typically used
    to distinguish elements in variants, records, etc. *)
type label = string

(** An unordered row of elements distinguished by label. *)
module LabelMap = Map.Make (String)

(** These names are used as hints for pretty printing binders and variables,
    but don’t impact the equality of terms. *)
type name = string


(** {1 Nameless binding structure} *)

(** The binding structure of terms is represented in the core language by
    using numbers that represent the distance to a binder, instead of by the
    names attached to those binders. *)

(** {i De Bruijn index} that represents a variable occurance by the number of
    binders between the occurance and the binder it refers to. *)
type index = int

(** {i De Bruijn level} that represents a variable occurance by the number of
    binders from the top of the environment to the binder that the ocurrance
    refers to. These do not change their meaning as new bindings are added to
    the environment. *)
type level = int

(** Converts a {!level} to an {!index} that is bound in an environment of the
    supplied size. Assumes that [ size > level ]. *)
let level_to_index size level =
  size - level - 1

(** An environment of bindings that can be looked up directly using a
    {!index}, or by inverting a {!level} using {!level_to_index}. *)
type 'a env = 'a list


(** {1 Syntax} *)

(** Metavariable identifier *)
type meta_id = int

(** Type syntax *)
type ty =
  | MetaVar of meta_state ref
  | FunType of ty * ty
  | VariantType of ty LabelMap.t
  | IntType
  | BoolType

(** The state of a metavariable, updated during unification *)
and meta_state =
  | Solved of ty
  | Unsolved of meta_id * constr

(** Constraints on unsolved metavariables *)
and constr =
  | Any
  (** Unifies with any type *)

  | Variant of ty LabelMap.t
  (** Unifies with variant types that contain {i at least} all of the cases
      recorded in the map. *)

(** Primitive operations *)
type prim = [
  | `Eq   (** [Int -> Int -> Bool] *)
  | `Add  (** [Int -> Int -> Int] *)
  | `Sub  (** [Int -> Int -> Int] *)
  | `Mul  (** [Int -> Int -> Int] *)
  | `Neg  (** [Int -> Int] *)
]

(** Term syntax *)
type tm =
  | Var of index
  | Let of name * ty * tm * tm
  | FunLit of name * ty * tm
  | FunApp of tm * tm
  | VariantLit of label * tm * ty
  | VariantElim of tm * (name * tm) LabelMap.t
  | IntLit of int
  | BoolLit of bool
  | BoolElim of tm * tm * tm
  | PrimApp of prim * tm list


module Semantics = struct

  (** {1 Values} *)

  type vtm =
    | Neu of ntm
    | FunLit of name * ty * (vtm -> vtm)
    | VariantLit of label * vtm * ty
    | BoolLit of bool
    | IntLit of int

  and ntm =
    | Var of level
    | FunApp of ntm * vtm
    | VariantElim of ntm * (name * (vtm -> vtm)) LabelMap.t
    | BoolElim of ntm * vtm Lazy.t * vtm Lazy.t
    | PrimApp of prim * vtm list


  (** {1 Eliminators} *)

  let fun_app head arg =
    match head with
    | Neu ntm -> Neu (FunApp (ntm, arg))
    | FunLit (_, _, body) -> body arg
    | _ -> invalid_arg "expected function"

  let variant_elim head cases =
    match head with
    | Neu ntm -> Neu (VariantElim (ntm, cases))
    | VariantLit (label, vtm, _) ->
        let _, body = LabelMap.find label cases in
        body vtm
    | _ -> invalid_arg "expected variant"

  let bool_elim head vtm0 vtm1 =
    match head with
    | Neu ntm -> Neu (BoolElim (ntm, vtm0, vtm1))
    | BoolLit true -> Lazy.force vtm0
    | BoolLit false -> Lazy.force vtm1
    | _ -> invalid_arg "expected boolean"

  let prim_app prim args =
    match prim, args with
    | `Eq, [IntLit t1; IntLit t2] -> BoolLit (t1 = t2)
    | `Add, [IntLit t1; IntLit t2] -> IntLit (t1 + t2)
    | `Sub, [IntLit t1; IntLit t2] -> IntLit (t1 - t2)
    | `Mul, [IntLit t1; IntLit t2] -> IntLit (t1 * t2)
    | `Neg, [IntLit t1] -> IntLit (-t1)
    | prim, args -> Neu (PrimApp (prim, args))


  (** {1 Evaluation} *)

  (** Evaluate a term from the syntax into its semantic interpretation *)
  let rec eval (env : vtm env) (tm : tm) : vtm =
    match tm with
    | Var index -> List.nth env index
    | Let (_, _, def, body) ->
        let def = eval env def in
        eval (def :: env) body
    | FunLit (name, param_ty, body) ->
        FunLit (name, param_ty, fun arg -> eval (arg :: env) body)
    | FunApp (head, arg) ->
        let head = eval env head in
        let arg = eval env arg in
        fun_app head arg
    | VariantLit (label, tm, ty) ->
        VariantLit (label, eval env tm, ty)
    | VariantElim (head, cases) ->
        let head = eval env head in
        let cases =
          cases |> LabelMap.map (fun (name, body) ->
            name, fun arg -> eval (arg :: env) body)
        in
        variant_elim head cases
    | IntLit i -> IntLit i
    | BoolLit b -> BoolLit b
    | BoolElim (head, tm0, tm1) ->
        let head = eval env head in
        let vtm0 = Lazy.from_fun (fun () -> eval env tm0) in
        let vtm1 = Lazy.from_fun (fun () -> eval env tm1) in
        bool_elim head vtm0 vtm1
    | PrimApp (prim, args) ->
        prim_app prim (List.map (eval env) args)


  (** {1 Quotation} *)

  (** Convert terms from the semantic domain back into syntax. *)
  let rec quote (size : int) (vtm : vtm) : tm =
    match vtm with
    | Neu ntm -> quote_neu size ntm
    | FunLit (name, param_ty, body) ->
        let body = quote (size + 1) (body (Neu (Var size))) in
        FunLit (name, param_ty, body)
    | VariantLit (label, vtm, ty) ->
        VariantLit (label, quote size vtm, ty)
    | IntLit i -> IntLit i
    | BoolLit b -> BoolLit b

  and quote_neu (size : int) (ntm : ntm) : tm =
    match ntm with
    | Var level ->
        Var (level_to_index size level)
    | FunApp (head, arg) ->
        FunApp (quote_neu size head, quote size arg)
    | VariantElim (head, cases) ->
        let head = quote_neu size head in
        let cases =
          cases |> LabelMap.map (fun (name, body) ->
            name, quote (size + 1) (body (Neu (Var size))))
        in
        VariantElim (head, cases)
    | BoolElim (head, vtm0, vtm1) ->
        let tm0 = quote size (Lazy.force vtm0) in
        let tm1 = quote size (Lazy.force vtm1) in
        BoolElim (quote_neu size head, tm0, tm1)
    | PrimApp (prim, args) ->
        PrimApp (prim, List.map (quote size) args)

  (** {1 Normalisation} *)

  (** By evaluating a term then quoting the result, we can produce a term that
      is reduced as much as possible in the current environment. *)
  let normalise (env : vtm list) (tm : tm) : tm =
    quote (List.length env) (eval env tm)

end


(** {1 Functions related to metavariables} *)

(** Create a fresh, unsolved metavariable *)
let fresh_meta : constr -> meta_state ref =
  let next_id = ref 0 in
  fun constr ->
    let id = !next_id in
    incr next_id;
    ref (Unsolved (id, constr))

(** Force any solved metavariables on the outermost part of a type. Chains of
    metavariables will be collapsed to make forcing faster in the future. This
    is sometimes referred to as {i path compression}. *)
let rec force (ty : ty) : ty =
  match ty with
  | MetaVar m as ty -> begin
      match !m with
      | Solved ty ->
          let ty = force ty in
          m := Solved ty;
          ty
      | Unsolved _ -> ty
  end
  | ty -> ty


(** {1 Unification} *)

exception InfiniteType of meta_id
exception MismatchedTypes of ty * ty

(** Occurs check. This guards against self-referential unification problems
    that would result in infinite loops during unification. *)
let rec occurs (id : meta_id) (ty : ty) : unit =
  match force ty with
  | MetaVar m -> begin
      match !m with
      | Unsolved (id', _) when id = id' ->
          raise (InfiniteType id)
      | Unsolved (_, Variant cases) ->
          cases |> LabelMap.iter (fun _ -> occurs id)
      | Unsolved (_, Any) | Solved _ -> ()
  end
  | FunType (param_ty, body_ty) ->
      occurs id param_ty;
      occurs id body_ty;
  | VariantType cases ->
      cases |> LabelMap.iter (fun _ ty -> occurs id ty)
  | IntType -> ()
  | BoolType -> ()

(** Check if two types are the same, updating unsolved metavaribles in one
    type with known information from the other type if possible. *)
let rec unify (ty0 : ty) (ty1 : ty) : unit =
  match force ty0, force ty1 with
  | ty0, ty1 when ty0 = ty1 -> ()
  | MetaVar m, ty | ty, MetaVar m -> unify_meta m ty
  | FunType (param_ty0, body_ty0), FunType (param_ty1, body_ty1) ->
      unify param_ty0 param_ty1;
      unify body_ty0 body_ty1;
  | VariantType cases0, VariantType cases1 ->
      if LabelMap.equal (fun ty0 ty1 -> unify ty0 ty1; true) cases0 cases1 then () else
        raise (MismatchedTypes (ty0, ty1))
  | IntType, IntType -> ()
  | BoolType, BoolType -> ()
  | ty1, ty2 ->
      raise (MismatchedTypes (ty1, ty2))

(** Unify a metavariable with a forced type *)
and unify_meta (m : meta_state ref) (ty : ty) : unit =
  match !m with
  (* The metavariable has no constraints, so we can set it to point to the type
     we are unifying against. *)
  | Unsolved (id, Any) ->
      occurs id ty;
      m := Solved ty
  (* The metavariable is constrained to be a variant type, so check that the
     the type we are unifying against also unifies with a variant type *)
  | Unsolved (id, (Variant cases as c)) ->
      occurs id ty;
      begin
        match ty with
        | MetaVar m' -> begin
            match !m' with
            | Unsolved (_, c') -> m' := Unsolved (id, unify_constrs c c')
            | Solved _ -> invalid_arg "expected a forced type"
        end
        | VariantType exact_cases ->
            let cases = unify_cases exact_cases cases in
            (* The number of cases in the unified cases must not exceed the
                cases given in explicit type annotations. *)
            if LabelMap.cardinal exact_cases < LabelMap.cardinal cases then
              raise (MismatchedTypes (MetaVar m, ty)) (* TODO: variant-specific mismatch error *)
        | _ -> ()
      end;
      m := Solved ty;
  (* The metavariable has a known type, so fall back to regular unification. *)
  | Solved mty ->
      unify ty mty

(** Unify two constraints *)
and unify_constrs (c1 : constr) (c2 : constr) : constr =
  match c1, c2 with
  | Any, Any -> Any
  | Variant cases, Any | Any, Variant cases -> Variant cases
  | Variant cases1, Variant cases2 -> Variant (unify_cases cases1 cases2)

(** Unify two lists of cases *)
and unify_cases (cases1 : ty LabelMap.t) (cases2 : ty LabelMap.t) : ty LabelMap.t =
  LabelMap.merge
    (fun _ ty1 ty2 ->
        match ty1, ty2 with
        | Some ty1, Some ty2 -> unify ty1 ty2; Some ty1
        | Some ty, None | None, Some ty -> Some ty
        | None, None  -> None)
    cases1
    cases2


(** {1 Zonking} *)

(** These functions flatten solved metavariables in types. This is imporatant
    for pretty printing types, as we want to be able to ‘see through’
    metavariables to properly associate function types. *)

let rec zonk_ty (ty : ty) : ty =
  match force ty with
  | MetaVar m -> begin
      match !m with
      | Solved ty -> zonk_ty ty
      | Unsolved _ -> MetaVar m
  end
  | FunType (param_ty, body_ty) ->
      FunType (zonk_ty param_ty, zonk_ty body_ty)
  | VariantType cases ->
      VariantType (cases |> LabelMap.map (fun ty -> zonk_ty ty))
  | IntType -> IntType
  | BoolType -> BoolType

let rec zonk_tm (tm : tm) : tm =
  match tm with
  | Var index -> Var index
  | Let (name, def_ty, def, body) ->
      Let (name, zonk_ty def_ty, zonk_tm def, zonk_tm body)
  | FunLit (name, param_ty, body) ->
      FunLit (name, zonk_ty param_ty, zonk_tm body)
  | FunApp (head, arg) ->
      FunApp (zonk_tm head, zonk_tm arg)
  | VariantLit (label, tm, ty) -> VariantLit (label, zonk_tm tm, zonk_ty ty)
  | VariantElim (head, cases) ->
      let head = zonk_tm head in
      let cases = cases |> LabelMap.map (fun (binder, body) -> binder, zonk_tm body) in
      VariantElim (head, cases)
  | IntLit i -> IntLit i
  | BoolLit b -> BoolLit b
  | BoolElim (head, tm0, tm1) ->
      BoolElim (zonk_tm head, zonk_tm tm0, zonk_tm tm1)
  | PrimApp (prim, args) ->
      PrimApp (prim, List.map zonk_tm args)


(** {1 Pretty printing} *)

let rec pp_ty (fmt : Format.formatter) (ty : ty) : unit =
  match ty with
  | FunType (param_ty, body_ty) ->
      Format.fprintf fmt "%a -> %a"
        pp_atomic_ty param_ty
        pp_ty body_ty
  | ty ->
      pp_atomic_ty fmt ty
and pp_atomic_ty fmt ty =
  match ty with
  | MetaVar m -> begin
      match !m with
      | Solved ty -> pp_atomic_ty fmt ty
      | Unsolved (id, Any) -> Format.fprintf fmt "?%i" id
      | Unsolved (id, Variant cases) ->
          Format.fprintf fmt "@[<2>@[?{%i@ ~@]@ @[%a@]}@]"
            id pp_ty (VariantType cases)
  end
  | VariantType cases when LabelMap.is_empty cases ->
      Format.fprintf fmt "[|]"
  | VariantType cases ->
      Format.fprintf fmt "[%a]"
        (Format.pp_print_seq
          ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ |@ ")
          (fun fmt (label, ty) ->
            Format.fprintf fmt "@[<2>@[%s@ :@]@ @[%a@]@]" label pp_ty ty))
        (LabelMap.to_seq cases)
  | IntType -> Format.fprintf fmt "Int"
  | BoolType -> Format.fprintf fmt "Bool"
  | ty -> Format.fprintf fmt "@[(%a)@]" pp_ty ty

let pp_name_ann fmt (name, ty) =
  Format.fprintf fmt "@[<2>@[%s :@]@ %a@]" name pp_ty ty

let pp_param fmt (name, ty) =
  Format.fprintf fmt "@[<2>(@[%s :@]@ %a)@]" name pp_ty ty

let rec pp_tm (names : name env) (fmt : Format.formatter) (tm : tm) : unit =
  match tm with
  | Let _ as tm ->
      let rec go names fmt tm =
        match tm with
        | Let (name, def_ty, def, body) ->
            Format.fprintf fmt "@[<2>@[let %a@ :=@]@ @[%a;@]@]@ %a"
              pp_name_ann (name, def_ty)
              (pp_tm names) def
              (go (name :: names)) body
        | tm -> Format.fprintf fmt "@[%a@]" (pp_tm names) tm
      in
      Format.fprintf fmt "@[<hv>%a@]" (go names) tm
  | VariantElim (head, cases) ->
      Format.fprintf fmt "@[<hv>@[match@ @[%a@]@ with@]@ %aend@]"
        (pp_eq_tm names) head
        (Format.pp_print_seq
          ~pp_sep:(fun fmt () -> Format.fprintf fmt "")
          (fun fmt (label, (name, body)) ->
            Format.fprintf fmt "@[@[<2>@[|@ [%s@ :=@]@ @[%s@]]@]@ =>@ @[%a@]@]@ "
              label
              name
              (pp_tm (name :: names)) body))
        (LabelMap.to_seq cases)
  | FunLit (name, param_ty, body) ->
      Format.fprintf fmt "@[<2>@[fun@ %a@ =>@]@ %a@]"
        pp_param (name, param_ty)
        (pp_tm (name :: names)) body
  | VariantLit (label, tm, ty) ->
      Format.fprintf fmt "@[<2>@[@[<2>@[[%s@ :=@]@ @[%a@]]@]@ :@]@ @[%a@]@]"
        label
        (pp_tm names) tm
        pp_ty ty
  | tm -> pp_if_tm names fmt tm
and pp_if_tm names fmt tm =
  match tm with
  | BoolElim (head, tm0, tm1) ->
      Format.fprintf fmt "@[if@ %a@ then@]@ %a@ else@ %a"
        (pp_eq_tm names) head
        (pp_eq_tm names) tm0
        (pp_if_tm names) tm1
  | tm ->
      pp_eq_tm names fmt tm
and pp_eq_tm names fmt tm =
  match tm with
  | PrimApp (`Eq, [arg1; arg2]) ->
      Format.fprintf fmt "@[%a@ =@ %a@]"
        (pp_add_tm names) arg1
        (pp_eq_tm names) arg2
  | tm ->
      pp_add_tm names fmt tm
and pp_add_tm names fmt tm =
  match tm with
  | PrimApp (`Add, [arg1; arg2]) ->
      Format.fprintf fmt "@[%a@ +@ %a@]"
        (pp_mul_tm names) arg1
        (pp_add_tm names) arg2
  | PrimApp (`Sub, [arg1; arg2]) ->
      Format.fprintf fmt "@[%a@ -@ %a@]"
        (pp_mul_tm names) arg1
        (pp_add_tm names) arg2
  | tm ->
      pp_mul_tm names fmt tm
and pp_mul_tm names fmt tm =
  match tm with
  | PrimApp (`Mul, [arg1; arg2]) ->
      Format.fprintf fmt "@[%a@ *@ %a@]"
        (pp_app_tm names) arg1
        (pp_mul_tm names) arg2
  | tm ->
      pp_app_tm names fmt tm
and pp_app_tm names fmt tm =
  match tm with
  | FunApp (head, arg) ->
      Format.fprintf fmt "@[%a@ %a@]"
        (pp_app_tm names) head
        (pp_atomic_tm names) arg
  | PrimApp (`Neg, [arg]) ->
      Format.fprintf fmt "@[-%a@]"
        (pp_atomic_tm names) arg
  | tm ->
      pp_atomic_tm names fmt tm
and pp_atomic_tm names fmt tm =
  match tm with
  | Var index -> Format.fprintf fmt "%s" (List.nth names index)
  | IntLit i -> Format.fprintf fmt "%i" i
  | BoolLit true -> Format.fprintf fmt "true"
  | BoolLit false -> Format.fprintf fmt "false"
  (* FIXME: Will loop forever on invalid primitive applications *)
  | tm -> Format.fprintf fmt "@[(%a)@]" (pp_tm names) tm
