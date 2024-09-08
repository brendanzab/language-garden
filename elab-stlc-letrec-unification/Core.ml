(** {0 Core language} *)

(** {1 Names} *)

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
  | TupleType of ty list
  | IntType
  | BoolType

(** The state of a metavariable, updated during unification *)
and meta_state =
  | Solved of ty
  | Unsolved of meta_id

(** Term syntax *)
type tm =
  | Var of index
  | Let of name * ty * tm * tm
  | Fix of name * ty * tm
  | FunLit of name * ty * tm
  | FunApp of tm * tm
  | TupleLit of tm list
  | TupleProj of tm * int
  | IntLit of int
  | BoolLit of bool
  | BoolElim of tm * tm * tm
  | PrimApp of Prim.t * tm list


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

  type vtm =
    | Neu of ntm
    | FunLit of name * ty * (eval_opts -> vtm -> vtm)
    | TupleLit of vtm list
    | BoolLit of bool
    | IntLit of int

  and ntm =
    | Var of level
    | Fix of name * ty * (eval_opts -> vtm -> vtm)
    | FunApp of ntm * vtm
    | TupleProj of ntm * int
    | BoolElim of ntm * vtm Lazy.t * vtm Lazy.t
    | PrimApp of Prim.t * vtm list


  (** {1 Eliminators} *)

  let rec fun_app opts head arg =
    match head with
    | Neu (Fix (_, _, body)) when opts.unfold_fix ->
        fun_app opts (body opts head) arg
    | Neu ntm -> Neu (FunApp (ntm, arg))
    | FunLit (_, _, body) -> body opts arg
    | _ -> invalid_arg "expected function"

  let rec tuple_proj opts head elem_index =
    match head with
    | Neu (Fix (_, _, body)) when opts.unfold_fix ->
        tuple_proj opts (body opts head) elem_index
    | Neu ntm -> Neu (TupleProj (ntm, elem_index))
    | TupleLit elems -> List.nth elems elem_index
    | _ -> invalid_arg "expected tuple"

  let bool_elim head vtm0 vtm1 =
    match head with
    | Neu ntm -> Neu (BoolElim (ntm, vtm0, vtm1))
    | BoolLit true -> Lazy.force vtm0
    | BoolLit false -> Lazy.force vtm1
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
  let rec eval ?(opts = default_opts) (env : vtm env) (tm : tm) : vtm =
    match tm with
    | Var index -> List.nth env index
    | Let (_, _, def, body) ->
        let def = eval ~opts env def in
        eval ~opts (def :: env) body
    | Fix (name, self_ty, body) ->
        let body' opts self = eval ~opts (self :: env) body in
        Neu (Fix (name, self_ty, body'))
    | FunLit (name, param_ty, body) ->
        let body opts arg = eval ~opts (arg :: env) body in
        FunLit (name, param_ty, body)
    | FunApp (head, arg) ->
        let head = eval ~opts env head in
        let arg = eval ~opts env arg in
        fun_app opts head arg
    | TupleLit elems ->
        TupleLit (List.map (eval ~opts env) elems)
    | TupleProj (head, elem_index) ->
        let head = eval ~opts env head in
        tuple_proj opts head elem_index
    | IntLit i -> IntLit i
    | BoolLit b -> BoolLit b
    | BoolElim (head, tm0, tm1) ->
        let head = eval ~opts env head in
        let vtm0 = Lazy.from_fun (fun () -> eval ~opts env tm0) in
        let vtm1 = Lazy.from_fun (fun () -> eval ~opts env tm1) in
        bool_elim head vtm0 vtm1
    | PrimApp (prim, args) ->
        prim_app prim (List.map (eval ~opts env) args)


  (** {1 Quotation} *)

  (** Convert terms from the semantic domain back into syntax. *)
  let rec quote ~opts (size : int) (vtm : vtm) : tm =
    match vtm with
    | Neu ntm -> quote_neu ~opts size ntm
    | FunLit (name, param_ty, body) ->
        let body = quote ~opts (size + 1) (body opts (Neu (Var size))) in
        FunLit (name, param_ty, body)
    | TupleLit elems ->
        TupleLit (List.map (quote ~opts size) elems)
    | BoolLit b -> BoolLit b
    | IntLit i -> IntLit i

  and quote_neu ~opts (size : int) (ntm : ntm) : tm =
    match ntm with
    | Var level -> Var (level_to_index size level)
    | Fix (name, self_ty, body) ->
        let body = quote ~opts (size + 1) (body opts (Neu (Var size))) in
        Fix (name, self_ty, body)
    | FunApp (head, arg) ->
        FunApp (quote_neu ~opts size head, quote ~opts size arg)
    | TupleProj (head, elem_index) ->
        TupleProj (quote_neu ~opts size head, elem_index)
    | BoolElim (head, vtm0, vtm1) ->
        let tm0 = quote ~opts size (Lazy.force vtm0) in
        let tm1 = quote ~opts size (Lazy.force vtm1) in
        BoolElim (quote_neu ~opts size head, tm0, tm1)
    | PrimApp (prim, args) ->
        PrimApp (prim, List.map (quote ~opts size) args)


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
  | MetaVar m as ty -> begin
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

exception InfiniteType of meta_id
exception MismatchedTypes of ty * ty

(** Occurs check. This guards against self-referential unification problems
    that would result in infinite loops during unification. *)
let rec occurs (id : meta_id) (ty : ty) : unit =
  match force ty with
  | MetaVar m ->
      if expect_forced m = id then
        raise (InfiniteType id)
  | FunType (param_ty, body_ty) ->
      occurs id param_ty;
      occurs id body_ty
  | TupleType elem_tys ->
      List.iter (occurs id) elem_tys
  | IntType -> ()
  | BoolType -> ()

(** Check if two types are the same, updating unsolved metavaribles in one
    type with known information from the other type if possible. *)
let rec unify (ty0 : ty) (ty1 : ty) : unit =
  match force ty0, force ty1 with
  | MetaVar m1, MetaVar m2 when m1 = m2 -> ()
  | MetaVar m, ty | ty, MetaVar m ->
      occurs (expect_forced m) ty;
      m := Solved ty
  | FunType (param_ty0, body_ty0), FunType (param_ty1, body_ty1) ->
      unify param_ty0 param_ty1;
      unify body_ty0 body_ty1;
  | TupleType elem_tys0, TupleType elem_tys1  -> begin
      try List.iter2 unify elem_tys0 elem_tys1 with
      | Invalid_argument _ ->
          raise (MismatchedTypes (ty0, ty1))
  end
  | IntType, IntType -> ()
  | BoolType, BoolType -> ()
  | ty1, ty2 ->
      raise (MismatchedTypes (ty1, ty2))


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
  | TupleType elem_tys ->
      TupleType (List.map zonk_ty elem_tys)
  | IntType -> IntType
  | BoolType -> BoolType

(** Flatten all metavariables in a term *)
let rec zonk_tm (tm : tm) : tm =
  match tm with
  | Var index -> Var index
  | Let (name, def_ty, def, body) ->
      Let (name, zonk_ty def_ty, zonk_tm def, zonk_tm body)
  | Fix (name, self_ty, body) ->
      Fix (name, zonk_ty self_ty, zonk_tm body)
  | FunLit (name, param_ty, body) ->
      FunLit (name, zonk_ty param_ty, zonk_tm body)
  | FunApp (head, arg) ->
      FunApp (zonk_tm head, zonk_tm arg)
  | TupleLit elems ->
      TupleLit (List.map zonk_tm elems)
  | TupleProj (head, elem_index) ->
      TupleProj (zonk_tm head, elem_index)
  | IntLit i -> IntLit i
  | BoolLit b -> BoolLit b
  | BoolElim (head, tm0, tm1) ->
      BoolElim (zonk_tm head, zonk_tm tm0, zonk_tm tm1)
  | PrimApp (prim, args) ->
      PrimApp (prim, List.map zonk_tm args)


(** {1 Pretty printing} *)

let rec fresh (ns : string env) (n : string) : string =
  match List.mem n ns with
  | true -> fresh ns (n ^ "'")
  | false -> n

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
  | TupleType [elem_ty] ->
      Format.fprintf fmt "(%a,)"
        pp_ty elem_ty
  | TupleType elem_tys ->
      Format.fprintf fmt "(%a)"
        (Format.pp_print_list
          ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
          pp_ty)
        elem_tys
  | IntType -> Format.fprintf fmt "Int"
  | BoolType -> Format.fprintf fmt "Bool"
  | ty -> Format.fprintf fmt "@[(%a)@]" pp_ty ty
and pp_meta fmt m =
  match !m with
  | Solved ty -> pp_atomic_ty fmt ty
  | Unsolved id -> Format.fprintf fmt "?%i" id

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
            let name = fresh names name in
            Format.fprintf fmt "@[<2>@[let %a@ :=@]@ @[%a;@]@]@ %a"
              pp_name_ann (name, def_ty)
              (pp_tm names) def
              (go (name :: names)) body
        | tm -> Format.fprintf fmt "@[%a@]" (pp_tm names) tm
      in
      Format.fprintf fmt "@[<hv>%a@]" (go names) tm
  | Fix (name, self_ty, body) ->
      let name = fresh names name in
      Format.fprintf fmt "@[<2>@[#fix@ %a@ =>@]@ %a@]"
        pp_param (name, self_ty)
        (pp_tm (name :: names)) body
  | FunLit (name, param_ty, body) ->
      let rec go names fmt tm =
        match tm with
        | FunLit (name, param_ty, body) ->
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
        (pp_proj_tm names) arg
  | PrimApp (prim, args) ->
      let pp_sep fmt () = Format.fprintf fmt "@ " in
      Format.fprintf fmt "@[#%s@ -%a@]"
        (Prim.name prim)
        (Format.pp_print_list ~pp_sep (pp_atomic_tm names)) args
  | tm ->
      pp_proj_tm names fmt tm
and pp_proj_tm names fmt tm =
  match tm with
  | TupleProj (head, elem_index) ->
      Format.fprintf fmt "%a.%i"
        (pp_proj_tm names) head
        elem_index
  | tm ->
      pp_atomic_tm names fmt tm
and pp_atomic_tm names fmt tm =
  match tm with
  | Var index -> Format.fprintf fmt "%s" (List.nth names index)
  | TupleLit [elem] ->
      Format.fprintf fmt "(%a,)"
        (pp_tm names) elem
  | TupleLit elems ->
      Format.fprintf fmt "(%a)"
        (Format.pp_print_list
          ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
          (pp_tm names))
        elems
  | IntLit i -> Format.fprintf fmt "%i" i
  | BoolLit true -> Format.fprintf fmt "true"
  | BoolLit false -> Format.fprintf fmt "false"
  (* FIXME: Will loop forever on invalid primitive applications *)
  | tm -> Format.fprintf fmt "@[(%a)@]" (pp_tm names) tm
