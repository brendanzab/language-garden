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

(** Type syntax *)
type ty =
  | Var of index                    (* Local type variables *)
  | TyFunType of name * ty          (* Type of a type function (i.e. a forall) *)
  | FunType of ty * ty              (* Type of function types *)
  | IntType
  | BoolType

(** Term syntax *)
type tm =
  | Var of index                    (* Local term variables *)
  | Let of name * ty * tm * tm
  | TyFunLit of name * tm           (* Type function literal (i.e. a big-lambda abstraction) *)
  | TyFunApp of tm * ty             (* Type function application *)
  | FunLit of name * ty * tm        (* Function literal (i.e. a lambda abstraction) *)
  | FunApp of tm * tm               (* Function application *)
  | IntLit of int
  | BoolLit of bool
  | BoolElim of tm * tm * tm
  | PrimApp of Prim.t * tm list


module Semantics = struct

  (** {1 Values} *)

  (** Types in weak head normal form (i.e. type values) *)
  type vty =
    | Neu of nty
    | TyFunType of name * (vty -> vty)
    | FunType of vty * vty
    | IntType
    | BoolType

  (** Neutral type values.

      We don’t have eliminators in types in System-F, so type neutrals only
      consist of variables. This would be extended with type function
      applications when moving to System-Fω.
  *)
  and nty =
    | Var of level              (* A fresh variable (used when evaluating under a binder) *)

  (** Terms in weak head normal form (i.e. values) *)
  type vtm =
    | Neu of ntm
    | TyFunLit of name * (vty -> vtm)
    | FunLit of name * vty * (vtm -> vtm)
    | IntLit of int
    | BoolLit of bool

  (** Neutral values that could not be reduced to a normal form as a result of
      being stuck on something else that would not reduce further.

      For simple (non-dependent) type systems these are not actually required,
      however they allow us to {!quote} terms back to syntax, which is useful
      for pretty printing under binders.
  *)
  and ntm =
    | Var of level              (* A fresh variable (used when evaluating under a binder) *)
    | TyFunApp of ntm * vty
    | FunApp of ntm * vtm
    | BoolElim of ntm * vtm Lazy.t * vtm Lazy.t
    | PrimApp of Prim.t * vtm list


  (** {1 Eliminators} *)

  let ty_fun_app (head : vtm) (arg : vty) : vtm =
    match head with
    | Neu ntm -> Neu (TyFunApp (ntm, arg))
    | TyFunLit (_, body) -> body arg
    | _ -> invalid_arg "expected type function"

  let fun_app (head : vtm) (arg : vtm) : vtm =
    match head with
    | Neu ntm -> Neu (FunApp (ntm, arg))
    | FunLit (_, _, body) -> body arg
    | _ -> invalid_arg "expected function"

  let bool_elim (head : vtm) (vtm0 : vtm Lazy.t) (vtm1 : vtm Lazy.t) : vtm =
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

  (** Evaluate a type from the syntax into its semantic interpretation *)
  let rec eval_ty (ty_env : vty env) (ty : ty) : vty =
    match ty with
    | Var index -> List.nth ty_env index
    | TyFunType (name, body_ty) ->
        TyFunType (name, fun arg -> eval_ty (arg :: ty_env) body_ty)
    | FunType (param_ty, body_ty) ->
        let param_vty = eval_ty ty_env param_ty in
        let body_vty = eval_ty ty_env body_ty in
        FunType (param_vty, body_vty)
    | IntType -> IntType
    | BoolType -> BoolType

  (** Evaluate a term from the syntax into its semantic interpretation *)
  let rec eval_tm (ty_env : vty env) (tm_env : vtm env) (tm : tm) : vtm =
    match tm with
    | Var index -> List.nth tm_env index
    | Let (_, _, def, body) ->
        let def = eval_tm ty_env tm_env def in
        eval_tm ty_env (def :: tm_env) body
    | TyFunLit (name, body) ->
        TyFunLit (name, fun arg -> eval_tm (arg :: ty_env) tm_env body)
    | TyFunApp (head, arg) ->
        let head = eval_tm ty_env tm_env head in
        let arg = eval_ty ty_env arg in
        ty_fun_app head arg
    | FunLit (name, param_ty, body) ->
        let param_vty = eval_ty ty_env param_ty in
        FunLit (name, param_vty, fun arg -> eval_tm ty_env (arg :: tm_env) body)
    | FunApp (head, arg) ->
        let head = eval_tm ty_env tm_env head in
        let arg = eval_tm ty_env tm_env arg in
        fun_app head arg
    | IntLit i -> IntLit i
    | BoolLit b -> BoolLit b
    | BoolElim (head, tm0, tm1) ->
        let head = eval_tm ty_env tm_env head in
        let vtm0 = Lazy.from_fun (fun () -> eval_tm ty_env tm_env tm0) in
        let vtm1 = Lazy.from_fun (fun () -> eval_tm ty_env tm_env tm1) in
        bool_elim head vtm0 vtm1
    | PrimApp (prim, args) ->
        prim_app prim (List.map (eval_tm ty_env tm_env) args)


  (** {1 Quotation} *)

  (** Convert types from the semantic domain back into syntax. *)
  let rec quote_vty  (ty_size : int) (vty : vty) : ty =
    match vty with
    | Neu (Var level) -> Var (level_to_index ty_size level)
    | TyFunType (name, body_vty) ->
        let body = quote_vty (ty_size + 1) (body_vty (Neu (Var ty_size))) in
        TyFunType (name, body)
    | FunType (param_vty, body_vty) ->
        let param_ty = quote_vty ty_size param_vty in
        let body_ty = quote_vty ty_size body_vty in
        FunType (param_ty, body_ty)
    | IntType -> IntType
    | BoolType -> BoolType

  (** Convert terms from the semantic domain back into syntax. *)
  let rec quote_vtm (ty_size : int) (tm_size : int) (vtm : vtm) : tm =
    match vtm with
    | Neu ntm -> quote_ntm ty_size tm_size ntm
    | TyFunLit (name, body) ->
        let body = quote_vtm (ty_size + 1) tm_size (body (Neu (Var ty_size))) in
        TyFunLit (name, body)
    | FunLit (name, param_vty, body) ->
        let param_ty = quote_vty ty_size param_vty in
        let body = quote_vtm ty_size (tm_size + 1) (body (Neu (Var tm_size))) in
        FunLit (name, param_ty, body)
    | IntLit i -> IntLit i
    | BoolLit b -> BoolLit b

  and quote_ntm (ty_size : int) (tm_size : int) (ntm : ntm) : tm =
    match ntm with
    | Var level ->
        Var (level_to_index tm_size level)
    | TyFunApp (head, arg) ->
        TyFunApp (quote_ntm ty_size tm_size head, quote_vty ty_size arg)
    | FunApp (head, arg) ->
        FunApp (quote_ntm ty_size tm_size head, quote_vtm ty_size tm_size arg)
    | BoolElim (head, vtm0, vtm1) ->
        let tm0 = quote_vtm ty_size tm_size (Lazy.force vtm0) in
        let tm1 = quote_vtm ty_size tm_size (Lazy.force vtm1) in
        BoolElim (quote_ntm ty_size tm_size head, tm0, tm1)
    | PrimApp (prim, args) ->
        PrimApp (prim, List.map (quote_vtm ty_size tm_size) args)


  (** {1 Normalisation} *)

  (** By evaluating a term then quoting the result, we can produce a term that
      is reduced as much as possible in the current environment. *)
  let normalise_tm (ty_env : vty env) (tm_env : vtm list) (tm : tm) : tm =
    quote_vtm (List.length ty_env) (List.length tm_env) (eval_tm ty_env tm_env tm)


  (** {1 Conversion Checking} *)

  let rec is_convertible (ty_size : int) (vty1 : vty) (vty2 : vty) : bool =
    match vty1, vty2 with
    | Neu (Var level1), Neu (Var level2) -> level1 = level2
    | TyFunType (_, body_ty1), TyFunType (_, body_ty2) ->
        let x : vty = Neu (Var ty_size) in
        is_convertible (ty_size + 1) (body_ty1 x) (body_ty2 x)
    | FunType (param_ty1, body_ty1), FunType (param_ty2, body_ty2) ->
        is_convertible ty_size param_ty1 param_ty2
          && is_convertible ty_size body_ty1 body_ty2
    | IntType, IntType -> true
    | BoolType, BoolType -> true
    | _, _ -> false

end


(** {1 Pretty printing} *)

let rec pp_ty  (ty_names : name env) (fmt : Format.formatter) (ty : ty) : unit =
  match ty with
  | TyFunType (name, body_ty) ->
      Format.fprintf fmt "[%s] -> %a"
        name
        (pp_ty (name :: ty_names)) body_ty
  | FunType (param_ty, body_ty) ->
      Format.fprintf fmt "%a -> %a"
        (pp_atomic_ty ty_names) param_ty
        (pp_ty ty_names) body_ty
  | ty ->
      pp_atomic_ty ty_names fmt ty
and pp_atomic_ty ty_names fmt ty =
  match ty with
  | Var index -> Format.fprintf fmt "%s" (List.nth ty_names index)
  | IntType -> Format.fprintf fmt "Int"
  | BoolType -> Format.fprintf fmt "Bool"
  | ty -> Format.fprintf fmt "@[(%a)@]" (pp_ty ty_names) ty

let pp_name_ann ty_names fmt (name, ty) =
  Format.fprintf fmt "@[<2>@[%s :@]@ %a@]" name (pp_ty ty_names) ty

let pp_param ty_names fmt (name, ty) =
  Format.fprintf fmt "@[<2>(@[%s :@]@ %a)@]" name (pp_ty ty_names) ty

let rec pp_tm (ty_names : name env) (tm_names : name env) (fmt : Format.formatter) (tm : tm) : unit =
  let rec go_params ty_names tm_names fmt tm =
    match tm with
    | TyFunLit (name, body) ->
        Format.fprintf fmt "@ @[fun@ [%s]@ =>@]%a"
          name
          (go_params (name :: ty_names) tm_names) body
    | FunLit (name, param_ty, body) ->
        Format.fprintf fmt "@ @[fun@ %a@ =>@]%a"
          (pp_param ty_names) (name, param_ty)
          (go_params ty_names (name :: tm_names)) body
    | tm -> Format.fprintf fmt "@]@ @[%a@]@]" (pp_tm ty_names tm_names) tm
  in
  match tm with
  | Let _ as tm ->
      let rec go tm_names fmt tm =
        match tm with
        | Let (name, def_ty, def, body) ->
            Format.fprintf fmt "@[<2>@[let %a@ :=@]@ @[%a;@]@]@ %a"
              (pp_name_ann ty_names) (name, def_ty)
              (pp_tm ty_names tm_names) def
              (go (name :: tm_names)) body
        | tm -> Format.fprintf fmt "@[%a@]" (pp_tm ty_names tm_names) tm
      in
      Format.fprintf fmt "@[<v>%a@]" (go tm_names) tm
  | TyFunLit (name, body) ->
      Format.fprintf fmt "@[<hv 2>@[<hv>@[fun@ [%s]@ =>@]%a"
        name
        (go_params (name :: ty_names) tm_names) body
  | FunLit (name, param_ty, body) ->
      Format.fprintf fmt "@[<hv 2>@[<hv>@[fun@ %a@ =>@]%a"
        (pp_param ty_names) (name, param_ty)
        (go_params ty_names (name :: tm_names)) body
  | BoolElim (head, tm0, tm1) ->
      Format.fprintf fmt "@[<hv>@[if@ %a@ then@]@;<1 2>@[%a@]@ else@;<1 2>@[%a@]@]"
        (pp_app_tm ty_names tm_names) head
        (pp_app_tm ty_names tm_names) tm0
        (pp_tm ty_names tm_names) tm1
  | tm ->
      pp_app_tm ty_names tm_names fmt tm
and pp_app_tm ty_names tm_names fmt tm =
  match tm with
  | TyFunApp (head, arg) ->
      Format.fprintf fmt "@[%a@ [%a]@]"
        (pp_app_tm ty_names tm_names) head
        (pp_ty ty_names) arg
  | FunApp (head, arg) ->
      Format.fprintf fmt "@[%a@ %a@]"
        (pp_app_tm ty_names tm_names) head
        (pp_atomic_tm ty_names tm_names) arg
  | PrimApp (prim, args) ->
      let pp_sep fmt () = Format.fprintf fmt "@ " in
      Format.fprintf fmt "@[#%s@ -%a@]"
        (Prim.name prim)
        (Format.pp_print_list ~pp_sep (pp_atomic_tm ty_names tm_names)) args
  | tm ->
      pp_atomic_tm ty_names tm_names fmt tm
and pp_atomic_tm ty_names tm_names fmt tm =
  match tm with
  | Var index -> Format.fprintf fmt "%s" (List.nth tm_names index)
  | IntLit i -> Format.fprintf fmt "%i" i
  | BoolLit true -> Format.fprintf fmt "true"
  | BoolLit false -> Format.fprintf fmt "false"
  | tm -> Format.fprintf fmt "@[(%a)@]" (pp_tm ty_names tm_names) tm
