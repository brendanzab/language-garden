(** {0 Core language} *)

(** {1 Names} *)

(** Labels are significant to the equality of terms. They are typically used
    to distinguish elements in variants, records, etc. *)
type label = string

(** An unordered row of elements distinguished by label. *)
module Label_map = Map.Make (String)

(** These names are used as hints for pretty printing binders and variables,
    but don’t impact the equality of terms. *)
type name = string option


(** {1 Nameless binding structure} *)

(** The binding structure of terms is represented in the core language by
    using numbers that represent the distance to a binder, instead of by the
    names attached to those binders. *)

(** {i De Bruijn index} that represents a variable occurrence by the number of
    binders between the occurrence and the binder it refers to. *)
type index = int

(** {i De Bruijn level} that represents a variable occurrence by the number of
    binders from the top of the environment to the binder that the occurrence
    refers to. These do not change their meaning as new bindings are added to
    the environment. *)
type level = int

(** [level_to_index size level] converts [level] to an {!index} that is bound in
    an environment of the supplied [size], where [size] represents the next
    fresh {!level} to be bound in the environment.

    Assumes that [size > level].
*)
let level_to_index (size : level) (level : level) =
  size - level - 1

(** An environment of bindings that can be looked up directly using a
    {!index}, or by converting to a {!level} using {!level_to_index}. *)
type 'a env = 'a list


(** {1 Syntax} *)

(** Metavariable identifier *)
type meta_id = int

(** Type syntax *)
type ty =
  | Meta_var of meta_state ref
  | Fun_type of ty * ty
  | Variant_type of ty Label_map.t
  | Int_type
  | Bool_type

(** The state of a metavariable, updated during unification *)
and meta_state =
  | Solved of ty
  | Unsolved of meta_id * constr

(** Constraints on unsolved metavariables *)
and constr =
  | Any
  (** Unifies with any type *)

  | Variant of ty Label_map.t
  (** Unifies with variant types that contain {i at least} all of the entries
      recorded in the map. *)

(** Term syntax *)
type tm =
  | Var of index
  | Let of name * ty * tm * tm
  | Fun_lit of name * ty * tm
  | Fun_app of tm * tm
  | Variant_lit of label * tm * ty
  | Variant_elim of tm * (name * tm) Label_map.t
  | Int_lit of int
  | Bool_lit of bool
  | Bool_elim of tm * tm * tm
  | Prim_app of Prim.t * tm list


module Semantics = struct

  (** {1 Values} *)

  (** Terms in weak head normal form (i.e. values) *)
  type vtm =
    | Neu of ntm
    | Fun_lit of name * ty * (vtm -> vtm)
    | Variant_lit of label * vtm * ty
    | Bool_lit of bool
    | Int_lit of int

  (** Neutral values that could not be reduced to a normal form as a result of
      being stuck on something else that would not reduce further.

      For simple (non-dependent) type systems these are not actually required,
      however they allow us to {!quote} terms back to syntax, which is useful
      for pretty printing under binders.
  *)
  and ntm =
    | Var of level              (* A fresh variable (used when evaluating under a binder) *)
    | Fun_app of ntm * vtm
    | Variant_elim of ntm * (name * (vtm -> vtm)) Label_map.t
    | Bool_elim of ntm * vtm Lazy.t * vtm Lazy.t
    | Prim_app of Prim.t * vtm list


  (** {1 Eliminators} *)

  let fun_app head arg =
    match head with
    | Neu ntm -> Neu (Fun_app (ntm, arg))
    | Fun_lit (_, _, body) -> body arg
    | _ -> invalid_arg "expected function"

  let variant_elim head cases =
    match head with
    | Neu ntm -> Neu (Variant_elim (ntm, cases))
    | Variant_lit (label, vtm, _) ->
        let _, body = Label_map.find label cases in
        body vtm
    | _ -> invalid_arg "expected variant"

  let bool_elim head vtm1 vtm2 =
    match head with
    | Neu ntm -> Neu (Bool_elim (ntm, vtm1, vtm2))
    | Bool_lit true -> Lazy.force vtm1
    | Bool_lit false -> Lazy.force vtm2
    | _ -> invalid_arg "expected boolean"

  let prim_app (prim : Prim.t) : vtm list -> vtm =
    let guard f args =
      try f args with
      | Match_failure _ -> Neu (Prim_app (prim, args))
    in
    match prim with
    | Bool_eq -> guard @@ fun[@warning "-partial-match"] [Bool_lit t1; Bool_lit t2] -> Bool_lit (t1 = t2)
    | Int_eq -> guard @@ fun[@warning "-partial-match"] [Int_lit t1; Int_lit t2] -> Bool_lit (t1 = t2)
    | Int_add -> guard @@ fun[@warning "-partial-match"] [Int_lit t1; Int_lit t2] -> Int_lit (t1 + t2)
    | Int_sub -> guard @@ fun[@warning "-partial-match"] [Int_lit t1; Int_lit t2] -> Int_lit (t1 - t2)
    | Int_mul -> guard @@ fun[@warning "-partial-match"] [Int_lit t1; Int_lit t2] -> Int_lit (t1 * t2)
    | Int_neg -> guard @@ fun[@warning "-partial-match"] [Int_lit t1] -> Int_lit (-t1)


  (** {1 Evaluation} *)

  (** Evaluate a term from the syntax into its semantic interpretation *)
  let rec eval (env : vtm env) (tm : tm) : vtm =
    match tm with
    | Var index -> List.nth env index
    | Let (_, _, def, body) ->
        let def = eval env def in
        eval (def :: env) body
    | Fun_lit (name, param_ty, body) ->
        Fun_lit (name, param_ty, fun arg -> eval (arg :: env) body)
    | Fun_app (head, arg) ->
        let head = eval env head in
        let arg = eval env arg in
        fun_app head arg
    | Variant_lit (label, tm, ty) ->
        Variant_lit (label, eval env tm, ty)
    | Variant_elim (head, cases) ->
        let head = eval env head in
        let cases =
          cases |> Label_map.map (fun (name, body) ->
            name, fun arg -> eval (arg :: env) body)
        in
        variant_elim head cases
    | Int_lit i -> Int_lit i
    | Bool_lit b -> Bool_lit b
    | Bool_elim (head, tm0, tm1) ->
        let head = eval env head in
        let vtm1 = lazy (eval env tm0) in
        let vtm2 = lazy (eval env tm1) in
        bool_elim head vtm1 vtm2
    | Prim_app (prim, args) ->
        prim_app prim (List.map (eval env) args)


  (** {1 Quotation} *)

  (** Convert terms from the semantic domain back into syntax. *)
  let rec quote (size : level) (vtm : vtm) : tm =
    match vtm with
    | Neu ntm -> quote_neu size ntm
    | Fun_lit (name, param_ty, body) ->
        let body = quote (size + 1) (body (Neu (Var size))) in
        Fun_lit (name, param_ty, body)
    | Variant_lit (label, vtm, ty) ->
        Variant_lit (label, quote size vtm, ty)
    | Int_lit i -> Int_lit i
    | Bool_lit b -> Bool_lit b

  and quote_neu (size : level) (ntm : ntm) : tm =
    match ntm with
    | Var level ->
        Var (level_to_index size level)
    | Fun_app (head, arg) ->
        Fun_app (quote_neu size head, quote size arg)
    | Variant_elim (head, cases) ->
        let head = quote_neu size head in
        let cases =
          cases |> Label_map.map (fun (name, body) ->
            name, quote (size + 1) (body (Neu (Var size))))
        in
        Variant_elim (head, cases)
    | Bool_elim (head, vtm1, vtm2) ->
        let tm0 = quote size (Lazy.force vtm1) in
        let tm1 = quote size (Lazy.force vtm2) in
        Bool_elim (quote_neu size head, tm0, tm1)
    | Prim_app (prim, args) ->
        Prim_app (prim, List.map (quote size) args)


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
  | Meta_var m as ty ->
      begin match !m with
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

exception Infinite_type of meta_id
exception Mismatched_types of ty * ty

(** Occurs check. This guards against self-referential unification problems
    that would result in infinite loops during unification. *)
let rec occurs (id : meta_id) (ty : ty) : unit =
  match force ty with
  | Meta_var m -> occurs_meta id m
  | Fun_type (param_ty, body_ty) ->
      occurs id param_ty;
      occurs id body_ty
  | Variant_type row -> occurs_row id row
  | Int_type -> ()
  | Bool_type -> ()

and occurs_meta (id : meta_id) (m : meta_state ref) : unit =
  match !m with
  | Unsolved (id', _) when id = id' -> raise (Infinite_type id)
  | Unsolved (_, Variant row) -> occurs_row id row
  | Unsolved (_, Any) | Solved _ -> ()

and occurs_row (id : meta_id) (row : ty Label_map.t) : unit =
  row |> Label_map.iter (fun _ -> occurs id)

(** Check if two types are the same, updating unsolved metavariables in one
    type with known information from the other type if possible. *)
let rec unify (ty1 : ty) (ty2 : ty) : unit =
  match force ty1, force ty2 with
  | Meta_var m1, Meta_var m2 when m1 == m2 -> ()
  | Meta_var m, ty | ty, Meta_var m ->
      unify_meta m ty
  | Fun_type (param_ty1, body_ty1), Fun_type (param_ty2, body_ty2) ->
      unify param_ty1 param_ty2;
      unify body_ty1 body_ty2
  | Variant_type row1, Variant_type row2 ->
      if not (Label_map.equal (fun ty1 ty2 -> unify ty1 ty2; true) row1 row2) then
        raise (Mismatched_types (ty1, ty2))
  | Int_type, Int_type -> ()
  | Bool_type, Bool_type -> ()
  | ty1, ty2 ->
      raise (Mismatched_types (ty1, ty2))

(** Unify a forced metavariable with a forced type *)
and unify_meta (m : meta_state ref) (ty : ty) : unit =
  match expect_forced m, ty with
  (* Unify with any type *)
  | (id, Any), ty ->
      occurs id ty;
      m := Solved ty
  (* Unify metavariable constraints *)
  | (id, c), Meta_var m' ->
      occurs_meta id m';
      let _, c' = expect_forced m' in
      m' := Unsolved (id, unify_constrs c c');
      m := Solved ty
  (* Unify a variant constraint against a concrete variant type *)
  | (id, Variant row), Variant_type exact_row ->
      occurs_row id exact_row;
      (* Unify the entries in the contraint row against the entries in the
         concrete type row, failing if any are missing. *)
      row |> Label_map.iter begin fun label row_ty ->
        match Label_map.find_opt label exact_row with
        | Some exact_row_ty -> unify row_ty exact_row_ty
        | None -> raise (Mismatched_types (Meta_var m, ty))
      end;
      m := Solved ty
  (* The type does not match the constraint, so raise an error *)
  | (_, Variant _), ty ->
      raise (Mismatched_types (Meta_var m, ty))

(** Unify two constraints, returning the combined constraint if successful. *)
and unify_constrs (c1 : constr) (c2 : constr) : constr =
  let unify_rows =
    Label_map.merge @@ fun _ ty1 ty2 ->
      match ty1, ty2 with
      | Some ty1, Some ty2 -> unify ty1 ty2; Some ty1
      | Some ty, None | None, Some ty -> Some ty
      | None, None  -> None
  in
  match c1, c2 with
  | Any, Any -> Any
  | Variant row, Any | Any, Variant row -> Variant row
  | Variant row1, Variant row2 -> Variant (unify_rows row1 row2)


(** {1 Pretty printing} *)

let rec pp_ty (ppf : Format.formatter) (ty : ty) : unit =
  match force ty with
  | Fun_type (param_ty, body_ty) ->
      Format.fprintf ppf "%a -> %a"
        pp_atomic_ty param_ty
        pp_ty body_ty
  | ty ->
      pp_atomic_ty ppf ty
and pp_atomic_ty ppf ty =
  match ty with
  | Meta_var m -> pp_meta ppf m
  | Variant_type cases when Label_map.is_empty cases ->
      Format.fprintf ppf "[|]"
  | Variant_type row ->
      Format.fprintf ppf "[%a]"
        (Format.pp_print_seq
          ~pp_sep:(fun ppf () -> Format.fprintf ppf "@ |@ ")
          (fun ppf (label, ty) ->
            Format.fprintf ppf "@[<2>@[%s@ :@]@ @[%a@]@]" label pp_ty ty))
        (Label_map.to_seq row)
  | Int_type -> Format.fprintf ppf "Int"
  | Bool_type -> Format.fprintf ppf "Bool"
  | ty -> Format.fprintf ppf "@[(%a)@]" pp_ty ty
and pp_meta ppf m =
  match !m with
  | Solved ty -> pp_atomic_ty ppf ty
  | Unsolved (id, Any) -> Format.fprintf ppf "?%i" id
  | Unsolved (id, Variant cases) ->
      Format.fprintf ppf "@[<2>@[?{%i@ ~@]@ @[%a@]}@]"
        id pp_ty (Variant_type cases)

let pp_name ppf name =
  match name with
  | Some name -> Format.pp_print_string ppf name
  | None -> Format.pp_print_string ppf "_"

let pp_name_ann ppf (name, ty) =
  Format.fprintf ppf "@[<2>@[%a :@]@ %a@]" pp_name name pp_ty ty

let pp_param ppf (name, ty) =
  Format.fprintf ppf "@[<2>(@[%a :@]@ %a)@]" pp_name name pp_ty ty

let rec pp_tm (names : name env) (ppf : Format.formatter) (tm : tm) : unit =
  match tm with
  | Let _ as tm ->
      let rec go names ppf tm =
        match tm with
        | Let (name, def_ty, def, body) ->
            Format.fprintf ppf "@[<2>@[let %a@ :=@]@ @[%a;@]@]@ %a"
              pp_name_ann (name, def_ty)
              (pp_tm names) def
              (go (name :: names)) body
        | tm -> Format.fprintf ppf "@[%a@]" (pp_tm names) tm
      in
      Format.fprintf ppf "@[<v>%a@]" (go names) tm
  | Variant_elim (head, cases) ->
      Format.fprintf ppf "@[<hv>@[match@ @[%a@]@ with@]@ %aend@]"
        (pp_app_tm names) head
        (Format.pp_print_seq
          ~pp_sep:(fun ppf () -> Format.fprintf ppf "")
          (fun ppf (label, (name, body)) ->
            Format.fprintf ppf "@[@[<2>@[|@ [%s@ :=@]@ @[%a@]]@]@ =>@ @[%a@]@]@ "
              label
              pp_name name
              (pp_tm (name :: names)) body))
        (Label_map.to_seq cases)
  | Fun_lit (name, param_ty, body) ->
      let rec go names ppf tm =
        match tm with
        | Fun_lit (name, param_ty, body) ->
            Format.fprintf ppf "@ @[fun@ %a@ =>@]%a"
              pp_param (name, param_ty)
              (go (name :: names)) body
        | tm -> Format.fprintf ppf "@]@ @[%a@]@]" (pp_tm names) tm
      in
      Format.fprintf ppf "@[<hv 2>@[<hv>@[fun@ %a@ =>@]%a"
        pp_param (name, param_ty)
        (go (name :: names)) body
  | Variant_lit (label, tm, ty) ->
      Format.fprintf ppf "@[<2>@[@[<2>@[[%s@ :=@]@ @[%a@]]@]@ :@]@ @[%a@]@]"
        label
        (pp_tm names) tm
        pp_ty ty
  | Bool_elim (head, tm0, tm1) ->
      Format.fprintf ppf "@[<hv>@[if@ %a@ then@]@;<1 2>@[%a@]@ else@;<1 2>@[%a@]@]"
        (pp_app_tm names) head
        (pp_app_tm names) tm0
        (pp_tm names) tm1
  | tm ->
      pp_app_tm names ppf tm
and pp_app_tm names ppf tm =
  match tm with
  | Fun_app (head, arg) ->
      Format.fprintf ppf "@[%a@ %a@]"
        (pp_app_tm names) head
        (pp_atomic_tm names) arg
  | Prim_app (prim, args) ->
      let pp_sep ppf () = Format.fprintf ppf "@ " in
      Format.fprintf ppf "@[#%s@ %a@]"
        (Prim.name prim)
        (Format.pp_print_list ~pp_sep (pp_atomic_tm names)) args
  | tm ->
      pp_atomic_tm names ppf tm
and pp_atomic_tm names ppf tm =
  match tm with
  | Var index -> Format.fprintf ppf "%a" pp_name (List.nth names index)
  | Int_lit i -> Format.fprintf ppf "%i" i
  | Bool_lit true -> Format.fprintf ppf "true"
  | Bool_lit false -> Format.fprintf ppf "false"
  | tm -> Format.fprintf ppf "@[(%a)@]" (pp_tm names) tm
