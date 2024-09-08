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
  | FunLit of name * ty * tm
  | FunApp of tm * tm
  | IntLit of int
  | BoolLit of bool
  | BoolElim of tm * tm * tm
  | PrimApp of Prim.t * tm list


module Semantics = struct

  (** {1 Values} *)

  type vtm =
    | Neu of ntm
    | IntLit of int
    | BoolLit of bool
    | FunLit of name * ty * (vtm -> vtm)

  and ntm =
    | Var of level
    | BoolElim of ntm * vtm Lazy.t * vtm Lazy.t
    | FunApp of ntm * vtm
    | PrimApp of Prim.t * vtm list


  (** {1 Eliminators} *)

  let fun_app head arg =
    match head with
    | Neu ntm -> Neu (FunApp (ntm, arg))
    | FunLit (_, _, body) -> body arg
    | _ -> invalid_arg "expected function"

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
    | IntLit i -> IntLit i
    | BoolLit b -> BoolLit b
    | BoolElim (head, tm1, tm2) ->
        let head = eval env head in
        let vtm1 = Lazy.from_fun (fun () -> eval env tm1) in
        let vtm2 = Lazy.from_fun (fun () -> eval env tm2) in
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
    | IntLit i -> IntLit i
    | BoolLit b -> BoolLit b

  and quote_neu (size : int) (ntm : ntm) : tm =
    match ntm with
    | Var level ->
        Var (level_to_index size level)
    | FunApp (head, arg) ->
        FunApp (quote_neu size head, quote size arg)
    | BoolElim (head, vtm1, vtm2) ->
        let tm1 = quote size (Lazy.force vtm1) in
        let tm2 = quote size (Lazy.force vtm2) in
        BoolElim (quote_neu size head, tm1, tm2)
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
  | IntType -> ()
  | BoolType -> ()

(** Check if two types are the same, updating unsolved metavaribles in one
    type with known information from the other type if possible. *)
let rec unify (ty1 : ty) (ty2 : ty) : unit =
  match force ty1, force ty2 with
  | MetaVar m1, MetaVar m2 when m1 = m2 -> ()
  | MetaVar m, ty | ty, MetaVar m ->
      occurs (expect_forced m) ty;
      m := Solved ty
  | FunType (param_ty1, body_ty1), FunType (param_ty2, body_ty2) ->
      unify param_ty1 param_ty2;
      unify body_ty1 body_ty2
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
  | IntLit i -> IntLit i
  | BoolLit b -> BoolLit b
  | BoolElim (head, tm1, tm2) ->
      BoolElim (zonk_tm head, zonk_tm tm1, zonk_tm tm2)
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
            Format.fprintf fmt "@[<2>@[let %a@ :=@]@ @[%a;@]@]@ %a"
              pp_name_ann (name, def_ty)
              (pp_tm names) def
              (go (name :: names)) body
        | tm -> Format.fprintf fmt "@[%a@]" (pp_tm names) tm
      in
      Format.fprintf fmt "@[<hv>%a@]" (go names) tm
  | FunLit (name, param_ty, body) ->
      Format.fprintf fmt "@[<2>@[fun@ %a@ =>@]@ @[%a@]@]"
        pp_param (name, param_ty)
        (pp_tm (name :: names)) body
  | tm -> pp_if_tm names fmt tm
and pp_if_tm names fmt tm =
  match tm with
  | BoolElim (head, tm0, tm1) ->
      Format.fprintf fmt "@[if@ %a@ then@]@ %a@ else@ %a"
        (pp_app_tm names) head
        (pp_app_tm names) tm0
        (pp_if_tm names) tm1
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
  (* FIXME: Will loop forever on invalid primitive applications *)
  | tm -> Format.fprintf fmt "@[(%a)@]" (pp_tm names) tm
