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
  (** Unifies with variant types that contain {i at least} all of the entries
      recorded in the map. *)

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
  | PrimApp of Prim.t * tm list


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
    | PrimApp of Prim.t * vtm list


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

  let bool_elim head vtm1 vtm2 =
    match head with
    | Neu ntm -> Neu (BoolElim (ntm, vtm1, vtm2))
    | BoolLit true -> Lazy.force vtm1
    | BoolLit false -> Lazy.force vtm2
    | _ -> invalid_arg "expected boolean"

  let prim_app (prim : Prim.t) : vtm list -> vtm =
    let guard f args =
      try f args with
      | Match_failure _ -> Neu (PrimApp (prim, args))
    in
    match prim with
    | BoolEq -> guard @@ fun[@warning "-partial-match"] [BoolLit t1; BoolLit t2] -> BoolLit (t1 = t2)
    | IntEq -> guard @@ fun[@warning "-partial-match"] [IntLit t1; IntLit t2] -> BoolLit (t1 = t2)
    | IntAdd -> guard @@ fun[@warning "-partial-match"] [IntLit t1; IntLit t2] -> IntLit (t1 + t2)
    | IntSub -> guard @@ fun[@warning "-partial-match"] [IntLit t1; IntLit t2] -> IntLit (t1 - t2)
    | IntMul -> guard @@ fun[@warning "-partial-match"] [IntLit t1; IntLit t2] -> IntLit (t1 * t2)
    | IntNeg -> guard @@ fun[@warning "-partial-match"] [IntLit t1] -> IntLit (-t1)


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
        let vtm1 = Lazy.from_fun (fun () -> eval env tm0) in
        let vtm2 = Lazy.from_fun (fun () -> eval env tm1) in
        bool_elim head vtm1 vtm2
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
    | BoolElim (head, vtm1, vtm2) ->
        let tm0 = quote size (Lazy.force vtm1) in
        let tm1 = quote size (Lazy.force vtm2) in
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

(** Extract the identifier and constraint from a forced metavariable. *)
let expect_forced (m : meta_state ref) : meta_id * constr =
  match !m with
  | Unsolved (id, c) -> id, c
  | Solved _ -> invalid_arg "unforced meta"


(** {1 Unification} *)

exception InfiniteType of meta_id
exception MismatchedTypes of ty * ty

(** Occurs check. This guards against self-referential unification problems
    that would result in infinite loops during unification. *)
let rec occurs (id : meta_id) (ty : ty) : unit =
  match force ty with
  | MetaVar m -> occurs_meta id m
  | FunType (param_ty, body_ty) ->
      occurs id param_ty;
      occurs id body_ty
  | VariantType row -> occurs_row id row
  | IntType -> ()
  | BoolType -> ()

and occurs_meta (id : meta_id) (m : meta_state ref) : unit =
  match !m with
  | Unsolved (id', _) when id = id' -> raise (InfiniteType id)
  | Unsolved (_, Variant row) -> occurs_row id row
  | Unsolved (_, Any) | Solved _ -> ()

and occurs_row (id : meta_id) (row : ty LabelMap.t) : unit =
  row |> LabelMap.iter (fun _ -> occurs id)

(** Check if two types are the same, updating unsolved metavaribles in one
    type with known information from the other type if possible. *)
let rec unify (ty1 : ty) (ty2 : ty) : unit =
  match force ty1, force ty2 with
  | MetaVar m1, MetaVar m2 when m1 = m2 -> ()
  | MetaVar m, ty | ty, MetaVar m ->
      unify_meta m ty
  | FunType (param_ty1, body_ty1), FunType (param_ty2, body_ty2) ->
      unify param_ty1 param_ty2;
      unify body_ty1 body_ty2
  | VariantType row1, VariantType row2 ->
      if not (LabelMap.equal (fun ty1 ty2 -> unify ty1 ty2; true) row1 row2) then
        raise (MismatchedTypes (ty1, ty2))
  | IntType, IntType -> ()
  | BoolType, BoolType -> ()
  | ty1, ty2 ->
      raise (MismatchedTypes (ty1, ty2))

(** Unify a forced metavariable with a forced type *)
and unify_meta (m : meta_state ref) (ty : ty) : unit =
  match expect_forced m, ty with
  (* Unify with any type *)
  | (id, Any), ty ->
      occurs id ty;
      m := Solved ty
  (* Unify metavariable constraints *)
  | (id, c), MetaVar m' ->
      occurs_meta id m';
      let _, c' = expect_forced m' in
      m' := Unsolved (id, unify_constrs c c');
      m := Solved ty
  (* Unify a variant constraint against a concrete variant type *)
  | (id, Variant row), VariantType exact_row ->
      occurs_row id exact_row;
      (* Unify the entries in the contraint row against the entries in the
         concrete type row, failing if any are missing. *)
      row |> LabelMap.iter begin fun label row_ty ->
        match LabelMap.find_opt label exact_row with
        | Some exact_row_ty -> unify row_ty exact_row_ty
        | None -> raise (MismatchedTypes (MetaVar m, ty))
      end;
      m := Solved ty
  (* The type does not match the constraint, so raise an error *)
  | (_, Variant _), ty ->
      raise (MismatchedTypes (MetaVar m, ty))

(** Unify two constraints, returning the combined constraint if successful. *)
and unify_constrs (c1 : constr) (c2 : constr) : constr =
  let unify_rows =
    LabelMap.merge @@ fun _ ty1 ty2 ->
      match ty1, ty2 with
      | Some ty1, Some ty2 -> unify ty1 ty2; Some ty1
      | Some ty, None | None, Some ty -> Some ty
      | None, None  -> None
  in
  match c1, c2 with
  | Any, Any -> Any
  | Variant row, Any | Any, Variant row -> Variant row
  | Variant row1, Variant row2 -> Variant (unify_rows row1 row2)


(** {1 Zonking} *)

(** These functions flatten solved metavariables in types. This is imporatant
    for pretty printing types, as we want to be able to ‘see through’
    metavariables to properly associate function types. *)

(* Deeply force a type, leaving only unsolved metavariables remaining *)
let rec zonk_ty (ty : ty) : ty =
  match force ty with
  | MetaVar m -> MetaVar m
  | FunType (param_ty, body_ty) ->
      FunType (zonk_ty param_ty, zonk_ty body_ty)
  | VariantType row ->
      VariantType (row |> LabelMap.map (fun ty -> zonk_ty ty))
  | IntType -> IntType
  | BoolType -> BoolType

(** Flatten all metavariables in a term *)
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
  | MetaVar m -> pp_meta fmt m
  | VariantType cases when LabelMap.is_empty cases ->
      Format.fprintf fmt "[|]"
  | VariantType row ->
      Format.fprintf fmt "[%a]"
        (Format.pp_print_seq
          ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ |@ ")
          (fun fmt (label, ty) ->
            Format.fprintf fmt "@[<2>@[%s@ :@]@ @[%a@]@]" label pp_ty ty))
        (LabelMap.to_seq row)
  | IntType -> Format.fprintf fmt "Int"
  | BoolType -> Format.fprintf fmt "Bool"
  | ty -> Format.fprintf fmt "@[(%a)@]" pp_ty ty
and pp_meta fmt m =
  match !m with
  | Solved ty -> pp_atomic_ty fmt ty
  | Unsolved (id, Any) -> Format.fprintf fmt "?%i" id
  | Unsolved (id, Variant cases) ->
      Format.fprintf fmt "@[<2>@[?{%i@ ~@]@ @[%a@]}@]"
        id pp_ty (VariantType cases)

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
        (pp_app_tm names) head
        (Format.pp_print_seq
          ~pp_sep:(fun fmt () -> Format.fprintf fmt "")
          (fun fmt (label, (name, body)) ->
            Format.fprintf fmt "@[@[<2>@[|@ [%s@ :=@]@ @[%s@]]@]@ =>@ @[%a@]@]@ "
              label
              name
              (pp_tm (name :: names)) body))
        (LabelMap.to_seq cases)
  | FunLit (name, param_ty, body) ->
      let rec go names fmt tm =
        match tm with
        | FunLit (name, param_ty, body) ->
            Format.fprintf fmt "@ @[fun@ %a@ =>@]%a"
              pp_param (name, param_ty)
              (go (name :: names)) body
        | tm -> Format.fprintf fmt "@]@ @[%a@]@]" (pp_tm names) tm
      in
      Format.fprintf fmt "@[<hv 2>@[<hv>@[fun@ %a@ =>@]%a"
        pp_param (name, param_ty)
        (go (name :: names)) body
  | VariantLit (label, tm, ty) ->
      Format.fprintf fmt "@[<2>@[@[<2>@[[%s@ :=@]@ @[%a@]]@]@ :@]@ @[%a@]@]"
        label
        (pp_tm names) tm
        pp_ty ty
  | BoolElim (head, tm0, tm1) ->
      Format.fprintf fmt "@[<hv>@[if@ %a@ then@]@;<1 2>@[%a@]@ else@;<1 2>@[%a@]@]"
        (pp_app_tm names) head
        (pp_app_tm names) tm0
        (pp_tm names) tm1
  | tm ->
      pp_app_tm names fmt tm
and pp_app_tm names fmt tm =
  match tm with
  | FunApp (head, arg) ->
      Format.fprintf fmt "@[%a@ %a@]"
        (pp_app_tm names) head
        (pp_atomic_tm names) arg
  | PrimApp (prim, args) ->
      let pp_sep fmt () = Format.fprintf fmt "@ " in
      Format.fprintf fmt "@[#%s@ -%a@]"
        (Prim.name prim)
        (Format.pp_print_list ~pp_sep (pp_atomic_tm names)) args
  | tm ->
      pp_atomic_tm names fmt tm
and pp_atomic_tm names fmt tm =
  match tm with
  | Var index -> Format.fprintf fmt "%s" (List.nth names index)
  | IntLit i -> Format.fprintf fmt "%i" i
  | BoolLit true -> Format.fprintf fmt "true"
  | BoolLit false -> Format.fprintf fmt "false"
  | tm -> Format.fprintf fmt "@[(%a)@]" (pp_tm names) tm
