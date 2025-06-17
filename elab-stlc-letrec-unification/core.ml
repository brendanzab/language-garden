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

(** Identifier used for pretty printing metavariables. *)
type meta_id = int

(** Type syntax *)
type ty =
  | Meta_var of meta
  | Fun_type of ty * ty
  | Tuple_type of ty list
  | Int_type
  | Bool_type

(** The current state of a metavariable *)
and meta_state =
  | Solved of ty
  | Unsolved of meta_id

(** Mutable representation of metavariables. These are updated in-place during
    unification and when types are forced. Alternatively we could have
    chosen to store these in a separate metacontext, like in the
    elaboration-zoo. *)
and meta = meta_state ref

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
    | Bool_elim of ntm * (unit -> vtm) * (unit -> vtm)
    | Prim_app of Prim.t * vtm list


  (** {1 Eliminators} *)

  let rec fun_app (opts : eval_opts) (head : vtm) (arg : vtm) : vtm =
    match head with
    | Neu (Fix (_, _, body)) when opts.unfold_fix ->
        fun_app opts (body opts head) arg
    | Neu ntm -> Neu (Fun_app (ntm, arg))
    | Fun_lit (_, _, body) -> body opts arg
    | _ -> invalid_arg "expected function"

  let rec tuple_proj (opts : eval_opts) (head : vtm) (elem_index : int) =
    match head with
    | Neu (Fix (_, _, body)) when opts.unfold_fix ->
        tuple_proj opts (body opts head) elem_index
    | Neu ntm -> Neu (Tuple_proj (ntm, elem_index))
    | Tuple_lit elems -> List.nth elems elem_index
    | _ -> invalid_arg "expected tuple"

  let bool_elim (head : vtm) (vtm1 : unit -> vtm) (vtm2 : unit -> vtm) : vtm =
    match head with
    | Neu ntm -> Neu (Bool_elim (ntm, vtm1, vtm2))
    | Bool_lit true -> vtm1 ()
    | Bool_lit false -> vtm2 ()
    | _ -> invalid_arg "expected boolean"

  let prim_app (prim : Prim.t) : vtm list -> vtm =
    let guard f args =
      try f args with
      | Match_failure _ -> Neu (Prim_app (prim, args))
    in
    match prim with
    | Bool_eq -> guard @@ fun[@warning "-partial-match"] [Bool_lit t1; Bool_lit t2] -> Bool_lit (Bool.equal t1 t2)
    | Int_eq -> guard @@ fun[@warning "-partial-match"] [Int_lit t1; Int_lit t2] -> Bool_lit (Int.equal t1 t2)
    | Int_add -> guard @@ fun[@warning "-partial-match"] [Int_lit t1; Int_lit t2] -> Int_lit (Int.add t1 t2)
    | Int_sub -> guard @@ fun[@warning "-partial-match"] [Int_lit t1; Int_lit t2] -> Int_lit (Int.sub t1 t2)
    | Int_mul -> guard @@ fun[@warning "-partial-match"] [Int_lit t1; Int_lit t2] -> Int_lit (Int.mul t1 t2)
    | Int_neg -> guard @@ fun[@warning "-partial-match"] [Int_lit t1] -> Int_lit (Int.neg t1)


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
    | Bool_elim (head, tm1, tm2) ->
        let head = eval ~opts env head in
        let vtm1 () = eval ~opts env tm1 in
        let vtm2 () = eval ~opts env tm2 in
        bool_elim head vtm1 vtm2
    | Prim_app (prim, args) ->
        prim_app prim (List.map (eval ~opts env) args)


  (** {1 Quotation} *)

  (** Convert terms from the semantic domain back into syntax. *)
  let rec quote ~opts (size : level) (vtm : vtm) : tm =
    match vtm with
    | Neu ntm -> quote_neu ~opts size ntm
    | Fun_lit (name, param_ty, body) ->
        let body = quote ~opts (size + 1) (body opts (Neu (Var size))) in
        Fun_lit (name, param_ty, body)
    | Tuple_lit elems ->
        Tuple_lit (List.map (quote ~opts size) elems)
    | Bool_lit b -> Bool_lit b
    | Int_lit i -> Int_lit i

  and quote_neu ~opts (size : level) (ntm : ntm) : tm =
    match ntm with
    | Var level -> Var (level_to_index size level)
    | Fix (name, self_ty, body) ->
        let body = quote ~opts (size + 1) (body opts (Neu (Var size))) in
        Fix (name, self_ty, body)
    | Fun_app (head, arg) ->
        Fun_app (quote_neu ~opts size head, quote ~opts size arg)
    | Tuple_proj (head, elem_index) ->
        Tuple_proj (quote_neu ~opts size head, elem_index)
    | Bool_elim (head, vtm1, vtm2) ->
        let tm1 = quote ~opts size (vtm1 ()) in
        let tm2 = quote ~opts size (vtm2 ()) in
        Bool_elim (quote_neu ~opts size head, tm1, tm2)
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
let fresh_meta : unit -> meta =
  let next_id = ref 0 in
  fun () ->
    let id = !next_id in
    incr next_id;
    ref (Unsolved id)

(** Force any solved metavariables on the outermost part of a type. Chains of
    metavariables will be collapsed to make forcing faster in the future. This
    is sometimes referred to as {i path compression}. *)
let rec force_ty (ty : ty) : ty =
  match ty with
  | Meta_var ({ contents = Solved ty } as m) ->
      let ty = force_ty ty in
      m := Solved ty;
      ty
  | ty -> ty


(** {1 Unification} *)

exception Infinite_type of meta
exception Mismatched_types of ty * ty

(** Occurs check. This guards against self-referential unification problems
    that would result in infinite loops during unification. *)
let rec occurs (m : meta) (ty : ty) : unit =
  match force_ty ty with
  | Meta_var m' ->
      if m == m' then
        raise (Infinite_type m)
  | Fun_type (param_ty, body_ty) ->
      occurs m param_ty;
      occurs m body_ty
  | Tuple_type elem_tys ->
      List.iter (occurs m) elem_tys
  | Int_type -> ()
  | Bool_type -> ()

(** Check if two types are the same, updating unsolved metavariables in one
    type with known information from the other type if possible. *)
let rec unify_tys (ty1 : ty) (ty2 : ty) : unit =
  match force_ty ty1, force_ty ty2 with
  | Meta_var m1, Meta_var m2 when m1 == m2 -> ()
  | Meta_var m, ty | ty, Meta_var m ->
      occurs m ty;
      m := Solved ty
  | Fun_type (param_ty1, body_ty1), Fun_type (param_ty2, body_ty2) ->
      unify_tys param_ty1 param_ty2;
      unify_tys body_ty1 body_ty2;
  | Tuple_type elem_tys1, Tuple_type elem_tys2  -> begin
      try List.iter2 unify_tys elem_tys1 elem_tys2 with
      | Invalid_argument _ ->
          raise (Mismatched_types (ty1, ty2))
  end
  | Int_type, Int_type -> ()
  | Bool_type, Bool_type -> ()
  | ty1, ty2 ->
      raise (Mismatched_types (ty1, ty2))


(** {1 Pretty printing} *)

let pp_ty : ty -> Format.formatter -> unit =
  let rec pp_ty ty ppf =
    match ty with
    | Meta_var m -> pp_meta pp_ty m ppf
    | Fun_type (param_ty, body_ty) ->
        Format.fprintf ppf "%t -> %t"
          (pp_atomic_ty param_ty)
          (pp_ty body_ty)
    | ty ->
        pp_atomic_ty ty ppf
  and pp_atomic_ty ty ppf =
    match ty with
    | Meta_var m -> pp_meta pp_atomic_ty m ppf
    | Tuple_type [elem_ty] ->
        Format.fprintf ppf "(%t,)"
          (pp_ty elem_ty)
    | Tuple_type elem_tys ->
        Format.fprintf ppf "(%a)"
          (Format.pp_print_list
            ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ ")
            (Fun.flip pp_ty))
          elem_tys
    | Int_type -> Format.fprintf ppf "Int"
    | Bool_type -> Format.fprintf ppf "Bool"
    | Fun_type _ as ty -> Format.fprintf ppf "@[(%t)@]" (pp_ty ty)
  and pp_meta pp_ty m ppf =
    match !m with
    | Solved ty -> pp_ty ty ppf
    | Unsolved id -> Format.fprintf ppf "?%i" id
  in
  pp_ty

let rec fresh (names : name env) (name : name) : name =
  match name with
  | Some name when List.mem (Some name) names -> fresh names (Some (name ^ "'"))
  | Some _ | None -> name

let pp_name (name : name) (ppf : Format.formatter) : unit =
  match name with
  | Some name -> Format.pp_print_string ppf name
  | None -> Format.pp_print_string ppf "_"

let pp_name_ann (name : name) (ty : ty) (ppf : Format.formatter) : unit =
  Format.fprintf ppf "@[<2>@[%t :@]@ %t@]" (pp_name name) (pp_ty ty)

let pp_param (name : name) (ty : ty) (ppf : Format.formatter) : unit =
  Format.fprintf ppf "@[<2>(@[%t :@]@ %t)@]" (pp_name name) (pp_ty ty)

let pp_tm : name env -> tm -> Format.formatter -> unit =
  let rec pp_tm names tm ppf =
    match tm with
    | Let _ as tm ->
        let rec go names tm ppf =
          match tm with
          | Let (name, def_ty, def, body) ->
              let name = fresh names name in
              Format.fprintf ppf "@[<2>@[let %t@ :=@]@ @[%t;@]@]@ %t"
                (pp_name_ann name def_ty)
                (pp_tm names def)
                (go (name :: names) body)
          | tm -> Format.fprintf ppf "@[%t@]" (pp_tm names tm)
        in
        Format.fprintf ppf "@[<v>%t@]" (go names tm)
    | Fix (name, self_ty, body) ->
        let name = fresh names name in
        Format.fprintf ppf "@[<2>@[#fix@ %t@ =>@]@ %t@]"
          (pp_param name self_ty)
          (pp_tm (name :: names) body)
    | Fun_lit (name, param_ty, body) ->
        let rec go names tm ppf =
          match tm with
          | Fun_lit (name, param_ty, body) ->
              let name = fresh names name in
              Format.fprintf ppf "@ @[fun@ %t@ =>@]%t"
                (pp_param name param_ty)
                (go (name :: names) body)
          | tm -> Format.fprintf ppf "@]@ @[%t@]@]" (pp_tm names tm)
        in
        let name = fresh names name in
        Format.fprintf ppf "@[<hv 2>@[<hv>@[fun@ %t@ =>@]%t"
          (pp_param name param_ty)
          (go (name :: names) body)
    | Bool_elim (head, tm1, tm2) ->
        Format.fprintf ppf "@[<hv>@[if@ %t@ then@]@;<1 2>@[%t@]@ else@;<1 2>@[%t@]@]"
          (pp_app_tm names head)
          (pp_app_tm names tm1)
          (pp_tm names tm2)
    | tm ->
        pp_app_tm names tm ppf
  and pp_app_tm names tm ppf =
    match tm with
    | Fun_app (head, arg) ->
        Format.fprintf ppf "@[%t@ %t@]"
          (pp_app_tm names head)
          (pp_proj_tm names arg)
    | Prim_app (prim, args) ->
        let pp_sep ppf () = Format.fprintf ppf "@ " in
        Format.fprintf ppf "@[#%s@ %t@]"
          (Prim.name prim)
          (fun ppf ->
            Format.pp_print_list ~pp_sep (Fun.flip (pp_proj_tm names)) ppf args)
    | tm ->
        pp_proj_tm names tm ppf
  and pp_proj_tm names tm ppf =
    match tm with
    | Tuple_proj (head, elem_index) ->
        Format.fprintf ppf "%t.%i"
          (pp_proj_tm names head)
          elem_index
    | tm ->
        pp_atomic_tm names tm ppf
  and pp_atomic_tm names tm ppf =
    match tm with
    | Var index -> Format.fprintf ppf "%t" (pp_name (List.nth names index))
    | Tuple_lit [elem] ->
        Format.fprintf ppf "(%t,)"
          (pp_tm names elem)
    | Tuple_lit elems ->
        Format.fprintf ppf "(%a)"
          (Format.pp_print_list
            ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ ")
            (Fun.flip (pp_tm names)))
          elems
    | Int_lit i -> Format.fprintf ppf "%i" i
    | Bool_lit true -> Format.fprintf ppf "true"
    | Bool_lit false -> Format.fprintf ppf "false"
    | Let _ | Fix _ | Fun_lit _ | Fun_app _ | Tuple_proj _
    | Bool_elim _ | Prim_app _ as tm ->
        Format.fprintf ppf "@[(%t)@]" (pp_tm names tm)
  in
  pp_tm
