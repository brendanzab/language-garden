(** {0 Core language} *)

(** {1 Names} *)

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
  | Meta_var of meta_state ref
  | Fun_type of ty * ty
  | Tuple_type of ty list
  | Int_type
  | Bool_type

(** The state of a metavariable, updated during unification *)
and meta_state =
  | Solved of ty
  | Unsolved of meta_id

(** Term syntax *)
type tm =
  | Var of index
  | Let of name * ty * tm * tm
  | Fix of name * ty * tm
  | Fun_lit of name * ty * tm
  | Fun_app of tm * tm
  | Tuple_lit of tm list
  | Tuple_proj of tm * int
  | Int_lit of int
  | Bool_lit of bool
  | Bool_elim of tm * tm * tm
  | Prim_app of Prim.t * tm list


module Semantics = struct

  (** Evaluation options *)
  type eval_opts = {
    unfold_fix : bool;
  }

  (** Default options for evaluation *)
  let default_opts = {
    unfold_fix = true;
  }

  (** {1 Values} *)

  (** Terms in weak head normal form (i.e. values) *)
  type vtm =
    | Neu of ntm
    | Fun_lit of name * ty * (eval_opts -> vtm -> vtm)
    | Tuple_lit of vtm list
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
    | Fix of name * ty * (eval_opts -> vtm -> vtm)
    | Fun_app of ntm * vtm
    | Tuple_proj of ntm * int
    | Bool_elim of ntm * vtm Lazy.t * vtm Lazy.t
    | Prim_app of Prim.t * vtm list


  (** {1 Eliminators} *)

  let rec fun_app opts head arg =
    match head with
    | Neu (Fix (_, _, body)) when opts.unfold_fix ->
        fun_app opts (body opts head) arg
    | Neu ntm -> Neu (Fun_app (ntm, arg))
    | Fun_lit (_, _, body) -> body opts arg
    | _ -> invalid_arg "expected function"

  let rec tuple_proj opts head elem_index =
    match head with
    | Neu (Fix (_, _, body)) when opts.unfold_fix ->
        tuple_proj opts (body opts head) elem_index
    | Neu ntm -> Neu (Tuple_proj (ntm, elem_index))
    | Tuple_lit elems -> List.nth elems elem_index
    | _ -> invalid_arg "expected tuple"

  let bool_elim head vtm0 vtm1 =
    match head with
    | Neu ntm -> Neu (Bool_elim (ntm, vtm0, vtm1))
    | Bool_lit true -> Lazy.force vtm0
    | Bool_lit false -> Lazy.force vtm1
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
  let rec eval ?(opts = default_opts) (env : vtm env) (tm : tm) : vtm =
    match tm with
    | Var index -> List.nth env index
    | Let (_, _, def, body) ->
        let def = eval ~opts env def in
        eval ~opts (def :: env) body
    | Fix (name, self_ty, body) ->
        let body' opts self = eval ~opts (self :: env) body in
        Neu (Fix (name, self_ty, body'))
    | Fun_lit (name, param_ty, body) ->
        let body opts arg = eval ~opts (arg :: env) body in
        Fun_lit (name, param_ty, body)
    | Fun_app (head, arg) ->
        let head = eval ~opts env head in
        let arg = eval ~opts env arg in
        fun_app opts head arg
    | Tuple_lit elems ->
        Tuple_lit (List.map (eval ~opts env) elems)
    | Tuple_proj (head, elem_index) ->
        let head = eval ~opts env head in
        tuple_proj opts head elem_index
    | Int_lit i -> Int_lit i
    | Bool_lit b -> Bool_lit b
    | Bool_elim (head, tm0, tm1) ->
        let head = eval ~opts env head in
        let vtm0 = lazy (eval ~opts env tm0) in
        let vtm1 = lazy (eval ~opts env tm1) in
        bool_elim head vtm0 vtm1
    | Prim_app (prim, args) ->
        prim_app prim (List.map (eval ~opts env) args)


  (** {1 Quotation} *)

  (** Convert terms from the semantic domain back into syntax. *)
  let rec quote ~opts (size : int) (vtm : vtm) : tm =
    match vtm with
    | Neu ntm -> quote_neu ~opts size ntm
    | Fun_lit (name, param_ty, body) ->
        let body = quote ~opts (size + 1) (body opts (Neu (Var size))) in
        Fun_lit (name, param_ty, body)
    | Tuple_lit elems ->
        Tuple_lit (List.map (quote ~opts size) elems)
    | Bool_lit b -> Bool_lit b
    | Int_lit i -> Int_lit i

  and quote_neu ~opts (size : int) (ntm : ntm) : tm =
    match ntm with
    | Var level -> Var (level_to_index size level)
    | Fix (name, self_ty, body) ->
        let body = quote ~opts (size + 1) (body opts (Neu (Var size))) in
        Fix (name, self_ty, body)
    | Fun_app (head, arg) ->
        Fun_app (quote_neu ~opts size head, quote ~opts size arg)
    | Tuple_proj (head, elem_index) ->
        Tuple_proj (quote_neu ~opts size head, elem_index)
    | Bool_elim (head, vtm0, vtm1) ->
        let tm0 = quote ~opts size (Lazy.force vtm0) in
        let tm1 = quote ~opts size (Lazy.force vtm1) in
        Bool_elim (quote_neu ~opts size head, tm0, tm1)
    | Prim_app (prim, args) ->
        Prim_app (prim, List.map (quote ~opts size) args)


  (** {1 Normalisation} *)

  (** By evaluating a term then quoting the result, we can produce a term that
      is reduced as much as possible in the current environment. *)
  let normalise ?(opts = default_opts) (env : vtm list) (tm : tm) : tm =
    quote (List.length env) (eval ~opts env tm) ~opts:{
      opts with unfold_fix = false;
      (* Limit recursive unfoldings. This prevents infinite loops when quoting
         partially-applied fixed-points. *)
    } [@warning "-useless-record-with"]

end


(** {1 Functions related to metavariables} *)

(** Create a fresh, unsolved metavariable *)
let fresh_meta : unit -> meta_state ref =
  let next_id = ref 0 in
  fun () ->
    let id = !next_id in
    incr next_id;
    ref (Unsolved id)

(** Force any solved metavariables on the outermost part of a type. Chains of
    metavariables will be collapsed to make forcing faster in the future. This
    is sometimes referred to as {i path compression}. *)
let rec force (ty : ty) : ty =
  match ty with
  | Meta_var m as ty -> begin
      match !m with
      | Solved ty ->
          let ty = force ty in
          m := Solved ty;
          ty
      | Unsolved _ -> ty
  end
  | ty -> ty

(** Extract the identifier from a forced metavariable. *)
let expect_forced (m : meta_state ref) : meta_id =
  match !m with
  | Unsolved id -> id
  | Solved _ -> invalid_arg "unforced meta"


(** {1 Unification} *)

exception Infinite_type of meta_id
exception Mismatched_types of ty * ty

(** Occurs check. This guards against self-referential unification problems
    that would result in infinite loops during unification. *)
let rec occurs (id : meta_id) (ty : ty) : unit =
  match force ty with
  | Meta_var m ->
      if expect_forced m = id then
        raise (Infinite_type id)
  | Fun_type (param_ty, body_ty) ->
      occurs id param_ty;
      occurs id body_ty
  | Tuple_type elem_tys ->
      List.iter (occurs id) elem_tys
  | Int_type -> ()
  | Bool_type -> ()

(** Check if two types are the same, updating unsolved metavariables in one
    type with known information from the other type if possible. *)
let rec unify (ty0 : ty) (ty1 : ty) : unit =
  match force ty0, force ty1 with
  | Meta_var m1, Meta_var m2 when m1 = m2 -> ()
  | Meta_var m, ty | ty, Meta_var m ->
      occurs (expect_forced m) ty;
      m := Solved ty
  | Fun_type (param_ty0, body_ty0), Fun_type (param_ty1, body_ty1) ->
      unify param_ty0 param_ty1;
      unify body_ty0 body_ty1;
  | Tuple_type elem_tys0, Tuple_type elem_tys1  -> begin
      try List.iter2 unify elem_tys0 elem_tys1 with
      | Invalid_argument _ ->
          raise (Mismatched_types (ty0, ty1))
  end
  | Int_type, Int_type -> ()
  | Bool_type, Bool_type -> ()
  | ty1, ty2 ->
      raise (Mismatched_types (ty1, ty2))


(** {1 Pretty printing} *)

let rec fresh (ns : name env) (n : name) : name =
  match n with
  | Some n when List.mem (Some n) ns -> fresh ns (Some (n ^ "'"))
  | Some _ | None -> n

let rec pp_ty (fmt : Format.formatter) (ty : ty) : unit =
  match force ty with
  | Fun_type (param_ty, body_ty) ->
      Format.fprintf fmt "%a -> %a"
        pp_atomic_ty param_ty
        pp_ty body_ty
  | ty ->
      pp_atomic_ty fmt ty
and pp_atomic_ty fmt ty =
  match ty with
  | Meta_var m -> pp_meta fmt m
  | Tuple_type [elem_ty] ->
      Format.fprintf fmt "(%a,)"
        pp_ty elem_ty
  | Tuple_type elem_tys ->
      Format.fprintf fmt "(%a)"
        (Format.pp_print_list
          ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
          pp_ty)
        elem_tys
  | Int_type -> Format.fprintf fmt "Int"
  | Bool_type -> Format.fprintf fmt "Bool"
  | ty -> Format.fprintf fmt "@[(%a)@]" pp_ty ty
and pp_meta fmt m =
  match !m with
  | Solved ty -> pp_atomic_ty fmt ty
  | Unsolved id -> Format.fprintf fmt "?%i" id

let pp_name fmt name =
  match name with
  | Some name -> Format.pp_print_string fmt name
  | None -> Format.pp_print_string fmt "_"

let pp_name_ann fmt (name, ty) =
  Format.fprintf fmt "@[<2>@[%a :@]@ %a@]" pp_name name pp_ty ty

let pp_param fmt (name, ty) =
  Format.fprintf fmt "@[<2>(@[%a :@]@ %a)@]" pp_name name pp_ty ty

let rec pp_tm (names : name env) (fmt : Format.formatter) (tm : tm) : unit =
  match tm with
  | Let _ as tm ->
      let rec go names fmt tm =
        match tm with
        | Let (name, def_ty, def, body) ->
            let name = fresh names name in
            Format.fprintf fmt "@[<2>@[let %a@ :=@]@ @[%a;@]@]@ %a"
              pp_name_ann (name, def_ty)
              (pp_tm names) def
              (go (name :: names)) body
        | tm -> Format.fprintf fmt "@[%a@]" (pp_tm names) tm
      in
      Format.fprintf fmt "@[<v>%a@]" (go names) tm
  | Fix (name, self_ty, body) ->
      let name = fresh names name in
      Format.fprintf fmt "@[<2>@[#fix@ %a@ =>@]@ %a@]"
        pp_param (name, self_ty)
        (pp_tm (name :: names)) body
  | Fun_lit (name, param_ty, body) ->
      let rec go names fmt tm =
        match tm with
        | Fun_lit (name, param_ty, body) ->
            let name = fresh names name in
            Format.fprintf fmt "@ @[fun@ %a@ =>@]%a"
              pp_param (name, param_ty)
              (go (name :: names)) body
        | tm -> Format.fprintf fmt "@]@ @[%a@]@]" (pp_tm names) tm
      in
      let name = fresh names name in
      Format.fprintf fmt "@[<hv 2>@[<hv>@[fun@ %a@ =>@]%a"
        pp_param (name, param_ty)
        (go (name :: names)) body
  | Bool_elim (head, tm0, tm1) ->
      Format.fprintf fmt "@[<hv>@[if@ %a@ then@]@;<1 2>@[%a@]@ else@;<1 2>@[%a@]@]"
        (pp_app_tm names) head
        (pp_app_tm names) tm0
        (pp_tm names) tm1
  | tm ->
      pp_app_tm names fmt tm
and pp_app_tm names fmt tm =
  match tm with
  | Fun_app (head, arg) ->
      Format.fprintf fmt "@[%a@ %a@]"
        (pp_app_tm names) head
        (pp_proj_tm names) arg
  | Prim_app (prim, args) ->
      let pp_sep fmt () = Format.fprintf fmt "@ " in
      Format.fprintf fmt "@[#%s@ %a@]"
        (Prim.name prim)
        (Format.pp_print_list ~pp_sep (pp_atomic_tm names)) args
  | tm ->
      pp_proj_tm names fmt tm
and pp_proj_tm names fmt tm =
  match tm with
  | Tuple_proj (head, elem_index) ->
      Format.fprintf fmt "%a.%i"
        (pp_proj_tm names) head
        elem_index
  | tm ->
      pp_atomic_tm names fmt tm
and pp_atomic_tm names fmt tm =
  match tm with
  | Var index -> Format.fprintf fmt "%a" pp_name (List.nth names index)
  | Tuple_lit [elem] ->
      Format.fprintf fmt "(%a,)"
        (pp_tm names) elem
  | Tuple_lit elems ->
      Format.fprintf fmt "(%a)"
        (Format.pp_print_list
          ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
          (pp_tm names))
        elems
  | Int_lit i -> Format.fprintf fmt "%i" i
  | Bool_lit true -> Format.fprintf fmt "true"
  | Bool_lit false -> Format.fprintf fmt "false"
  | tm -> Format.fprintf fmt "@[(%a)@]" (pp_tm names) tm
