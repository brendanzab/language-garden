(** {0 Core language} *)

(** These names are used as hints for pretty printing binders and variables,
    but don’t impact the equality of terms. *)
type name = string

(** {i De Bruijn index} that represents a variable occurrence by the number of
    binders between the occurrence and the binder it refers to. *)
type index = int

(** An environment of bindings that can be looked up directly using a
    {!index}, or by converting to a {!level} using {!level_to_index}. *)
type 'a env = 'a list


(** {1 Syntax} *)

(** Type syntax *)
type ty =
  | Fun_type of ty * ty
  | Int_type
  | Bool_type
  | Unknown_type            (* A type hole, used when recovering from errors *)

(** Term syntax *)
type tm =
  | Var of index
  | Prim of Prim.t
  | Let of name * ty * tm * tm
  | Fun_lit of name * ty * tm
  | Fun_app of tm * tm
  | Int_lit of int
  | Bool_lit of bool
  | Bool_elim of tm * tm * tm
  | Reported_error

(** Apply a term to a list of arguments *)
let fun_app (head : tm) (args : tm list) : tm =
  List.fold_left (fun head arg -> Fun_app (head, arg)) head args


module Semantics = struct

  (** {1 Values} *)

  (** Evaluated terms *)
  type vtm =
    | Prim_app of Prim.t * vtm list
    | Fun_lit of clos
    | Int_lit of int
    | Bool_lit of bool
    | Reported_error

  (** Closures that can be instantiated with a value. The environment provides
      a value for each variable in the term, except for the variable that the
      closure will be instantiated with during evaluation.*)
  and clos = vtm env * tm


  (** {1 Evaluation} *)

  (** Evaluate a term from the syntax into its semantic interpretation *)
  let rec eval (env : vtm env) (tm : tm) : vtm =
    match tm with
    | Var index -> List.nth env index
    | Prim prim -> prim_app prim []
    | Let (_, _, def, body) ->
        let def = eval env def in
        eval (def :: env) body
    | Fun_lit (_, _, body) ->
        Fun_lit (env, body)
    | Fun_app (head, arg) ->
        begin match eval env head with
        | Fun_lit clos -> clos_app clos (eval env arg)
        | Prim_app (prim, args) -> prim_app prim ((eval env arg) :: args)
        | _ -> invalid_arg "expected function"
        end
    | Int_lit i -> Int_lit i
    | Bool_lit b -> Bool_lit b
    | Bool_elim (head, tm1, tm2) ->
        begin match eval env head with
        | Bool_lit true -> eval env tm1
        | Bool_lit false -> eval env tm2
        | _ -> invalid_arg "expected boolean"
        end
    | Reported_error -> Reported_error

  and clos_app (env, body : clos) (arg : vtm) : vtm =
    eval (arg :: env) body

  and prim_app (prim : Prim.t) (args : vtm list) : vtm =
    match prim, args with
    | Bool_eq, [Bool_lit t2; Bool_lit t1] -> Bool_lit (Bool.equal t1 t2)
    | Int_eq, [Int_lit t2; Int_lit t1] -> Bool_lit (Int.equal t1 t2)
    | Int_add, [Int_lit t2; Int_lit t1] -> Int_lit (Int.add t1 t2)
    | Int_sub, [Int_lit t2; Int_lit t1] -> Int_lit (Int.sub t1 t2)
    | Int_mul, [Int_lit t2; Int_lit t1] -> Int_lit (Int.mul t1 t2)
    | Int_neg, [Int_lit t1] -> Int_lit (Int.neg t1)
    | prim, args -> Prim_app (prim, args)


  (** {1 Pretty printing} *)

  let pp_vtm : vtm -> Format.formatter -> unit =
    let rec pp_vtm vtm ppf =
      match vtm with
      | Prim_app (prim, ((_ :: _) as args)) ->
          let pp_sep ppf () = Format.fprintf ppf "@ " in
          Format.fprintf ppf "@[<2>#%s %t@]"
            (Prim.name prim)
            (Fun.flip (Format.pp_print_list (Fun.flip pp_atomic_vtm) ~pp_sep) args)
      | vtm ->
          pp_atomic_vtm vtm ppf
    and pp_atomic_vtm vtm ppf =
      match vtm with
      | Prim_app (prim, []) -> Format.fprintf ppf "#%s" (Prim.name prim)
      | Fun_lit _ -> Format.fprintf ppf "<function>"
      | Bool_lit true -> Format.fprintf ppf "true"
      | Bool_lit false -> Format.fprintf ppf "false"
      | Int_lit i -> Format.fprintf ppf "%i" i
      | Reported_error -> Format.fprintf ppf "#reported-error"
      | Prim_app (_, (_ :: _)) as vtm ->
          Format.fprintf ppf "@[(%t)@]" (pp_vtm vtm)
    in
    pp_vtm

end


(** {1 Comparing types} *)

(** Check if two types are compatible with each other. *)
let rec equate_tys (ty1 : ty) (ty2 : ty) : bool =
  match ty1, ty2 with
  | Unknown_type, _ | _, Unknown_type -> true
  | Fun_type (param_ty1, body_ty1), Fun_type (param_ty2, body_ty2) ->
      equate_tys param_ty1 param_ty2
      && equate_tys body_ty1 body_ty2
  | Int_type, Int_type -> true
  | Bool_type, Bool_type -> true
  | _, _ -> false

(** The “meet” of two types picks the most specific of two compatible types,
    which is important for propagating as much type information as we can when
    inferring the type of conditionals. This was inspired by section 2.1.6 of
    “Total Type Error Localization and Recovery with Holes” by Zhao et. al. *)
let rec meet_tys (ty1 : ty) (ty2 : ty) : ty option =
  let ( let* ) = Option.bind in
  match ty1, ty2 with
  | Unknown_type, ty | ty, Unknown_type -> Some ty
  | Fun_type (param_ty1, body_ty1), Fun_type (param_ty2, body_ty2) ->
      let* param_ty = meet_tys param_ty1 param_ty2 in
      let* body_ty = meet_tys body_ty1 body_ty2 in
      Some (Fun_type (param_ty, body_ty))
  | Int_type, Int_type -> Some Int_type
  | Bool_type, Bool_type -> Some Bool_type
  | _, _ -> None


(** {1 Pretty printing} *)

let pp_ty : ty -> Format.formatter -> unit =
  let rec pp_ty ty ppf =
    match ty with
    | Fun_type (param_ty, body_ty) ->
        Format.fprintf ppf "%t -> %t"
          (pp_atomic_ty param_ty)
          (pp_ty body_ty)
    | ty ->
        pp_atomic_ty ty ppf
  and pp_atomic_ty ty ppf =
    match ty with
    | Int_type -> Format.fprintf ppf "Int"
    | Bool_type -> Format.fprintf ppf "Bool"
    | Unknown_type -> Format.fprintf ppf "?"
    | Fun_type _ as ty -> Format.fprintf ppf "@[(%t)@]" (pp_ty ty)
  in
  pp_ty

let pp_name_ann (name : name) (ty : ty) (ppf : Format.formatter) : unit =
  Format.fprintf ppf "@[<2>@[%s :@]@ %t@]" name (pp_ty ty)

let pp_param (name : name) (ty : ty) (ppf : Format.formatter) : unit =
  Format.fprintf ppf "@[<2>(@[%s :@]@ %t)@]" name (pp_ty ty)

let pp_tm : name env -> tm -> Format.formatter -> unit =
  let rec pp_tm names tm ppf =
    match tm with
    | Let _ as tm ->
        let rec go names tm ppf =
          match tm with
          | Let (name, def_ty, def, body) ->
              Format.fprintf ppf "@[<2>@[let %t@ :=@]@ @[%t;@]@]@ %t"
                (pp_name_ann name def_ty)
                (pp_tm names def)
                (go (name :: names) body)
          | tm -> Format.fprintf ppf "@[%t@]" (pp_tm names tm)
        in
        Format.fprintf ppf "@[<v>%t@]" (go names tm)
    | Fun_lit (name, param_ty, body) ->
        let rec go names tm ppf =
          match tm with
          | Fun_lit (name, param_ty, body) ->
              Format.fprintf ppf "@ @[fun@ %t@ =>@]%t"
                (pp_param name param_ty)
                (go (name :: names) body)
          | tm -> Format.fprintf ppf "@]@ @[%t@]@]" (pp_tm names tm)
        in
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
    let rec go tm ppf =
      match tm with
      | Fun_app (head, arg) ->
          Format.fprintf ppf "%t@ %t"
            (go head)
            (pp_atomic_tm names arg)
      | tm ->
          pp_atomic_tm names tm ppf
    in
    match tm with
    | Fun_app _ as tm ->
        Format.fprintf ppf "@[<hv 2>%t@]" (go tm)
    | tm ->
        pp_atomic_tm names tm ppf
  and pp_atomic_tm names tm ppf =
    match tm with
    | Var index -> Format.fprintf ppf "%s" (List.nth names index)
    | Prim prim -> Format.fprintf ppf "#%s" (Prim.name prim)
    | Int_lit i -> Format.fprintf ppf "%i" i
    | Bool_lit true -> Format.fprintf ppf "true"
    | Bool_lit false -> Format.fprintf ppf "false"
    | Reported_error -> Format.fprintf ppf "#reported-error"
    | Let _ | Fun_lit _ | Fun_app _ | Bool_elim _ as tm ->
        Format.fprintf ppf "@[(%t)@]" (pp_tm names tm)
  in
  pp_tm
