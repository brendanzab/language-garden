(** {0 Core language} *)

(** {1 Types} *)

type ty =
  | IntType
  | FunType of ty * ty
  | MetaVar of meta_state ref

and meta_state =
  | Solved of ty
  | Unsolved of int


(** {1 Terms} *)

(** Primitive operations *)
type prim = [
  | `Add  (** [Int -> Int -> Int] *)
  | `Sub  (** [Int -> Int -> Int] *)
  | `Mul  (** [Int -> Int -> Int] *)
  | `Neg  (** [Int -> Int] *)
]

type tm =
  | Var of int
  | Let of string * ty * tm * tm
  | IntLit of int
  | FunLit of string * ty * tm
  | FunApp of tm * tm
  | PrimApp of prim * tm list


module Semantics = struct

  (** {1 Values} *)

  type vtm =
    | Var of int
    | IntLit of int
    | FunLit of string * ty * (vtm -> vtm)


  (** {1 Eliminators} *)

  let prim_app prim args =
    match prim, args with
    | `Neg, [IntLit t1] -> IntLit (-t1)
    | `Add, [IntLit t1; IntLit t2] -> IntLit (t1 + t2)
    | `Sub, [IntLit t1; IntLit t2] -> IntLit (t1 - t2)
    | `Mul, [IntLit t1; IntLit t2] -> IntLit (t1 * t2)
    | _, _ -> invalid_arg "invalid prim application"

  let fun_app head arg =
    match head with
    | FunLit (_, _, body) -> body arg
    | _ -> invalid_arg "expected function"


  (** {1 Evaluation} *)

  let rec eval (env : vtm list) : tm -> vtm =
    function
    | Var index -> List.nth env index
    | Let (_, _, def, body) ->
        let def = eval env def in
        eval (def :: env) body
    | IntLit i -> IntLit i
    | PrimApp (prim, args) ->
        prim_app prim (List.map (eval env) args)
    | FunLit (name, param_ty, body) ->
        FunLit (name, param_ty, fun arg -> eval (arg :: env) body)
    | FunApp (head, arg) ->
        let head = eval env head in
        let arg = eval env arg in
        fun_app head arg


  (** {1 Quotation} *)

  let rec quote (size : int) : vtm -> tm =
    function
    | Var level -> Var (size - level - 1)
    | IntLit i -> IntLit i
    | FunLit (name, param_ty, body) ->
        let body = quote (size + 1) (body (Var size)) in
        FunLit (name, param_ty, body)


  (** {1 Normalisation} *)

  let normalise (env : vtm list) (tm : tm) : tm =
    quote (List.length env) (eval env tm)

end


(** {1 Functions related to metavariables} *)

(** Create a fresh, unsolved metavariable *)
let fresh_meta =
  let next_id = ref 0 in
  fun () ->
    let id = !next_id in
    incr next_id;
    MetaVar (ref (Unsolved id))

(** Force any solved metavariables on the outermost part of a type. Chains of
    metavariables will be collapsed to make forcing faster in the future. *)
let rec force : ty -> ty =
  function
  | MetaVar m as ty ->
      begin match !m with
      | Solved ty ->
          let ty = force ty in
          m := Solved ty;
          ty
      | Unsolved _ -> ty
      end
  | ty -> ty


(** {1 Unification} *)

exception InfiniteType of int
exception MismatchedTypes of ty * ty

(** Occurs check. This guards against self-referential unification problems
    that would result in infinite loops during unification. *)
let rec occurs (id : int) (ty : ty) : unit =
  match force ty with
  | MetaVar m ->
      begin match !m with
      | Unsolved id' when id = id' ->
          raise (InfiniteType id)
      | Unsolved _ | Solved _-> ()
      end
  | IntType -> ()
  | FunType (param_ty, body_ty) ->
      occurs id param_ty;
      occurs id body_ty

(** Check if two types are the same, updating unsolved metavaribles in one
    type with known information from the other type if possible. *)
let rec unify (ty0 : ty) (ty1 : ty) : unit =
  match force ty0, force ty1 with
  | ty0, ty1 when ty0 = ty1 -> ()
  | MetaVar m, ty | ty, MetaVar m -> unify_meta m ty
  | IntType, IntType -> ()
  | FunType (param_ty0, body_ty0), FunType (param_ty1, body_ty1) ->
      unify param_ty0 param_ty1;
      unify body_ty0 body_ty1;
  | ty1, ty2 ->
      raise (MismatchedTypes (ty1, ty2))

(** Unify a metavariable with a type *)
and unify_meta (m : meta_state ref) (ty : ty) : unit =
  match !m with
  | Unsolved id ->
      occurs id ty;
      m := Solved ty;
  | Solved mty ->
      unify ty mty


(** {1 Pretty printing} *)

let rec pp_ty fmt ty =
  (* Forcing here is important because we want to handle precedences correctly
     through metavariable solutions *)
  match force ty with
  | FunType (param_ty, body_ty) ->
      Format.fprintf fmt "%a -> %a"
        pp_atomic_ty param_ty
        pp_ty body_ty
  | ty ->
      pp_atomic_ty fmt ty
and pp_atomic_ty fmt =
  function
  | IntType -> Format.fprintf fmt "Int"
  | MetaVar m ->
      begin match !m with
      | Solved ty -> pp_atomic_ty fmt ty
      | Unsolved id -> Format.fprintf fmt "?%i" id
      end
  | ty -> Format.fprintf fmt "@[(%a)@]" pp_ty ty

let pp_name_ann fmt (name, ty) =
  Format.fprintf fmt "@[<2>@[%s :@]@ %a@]" name pp_ty ty

let pp_param fmt (name, ty) =
  Format.fprintf fmt "@[<2>(@[%s :@]@ %a)@]" name pp_ty ty

let rec pp_tm names fmt =
  function
  | Let _ as tm ->
      let rec go names fmt = function
        | Let (name, def_ty, def, body) ->
            Format.fprintf fmt "@[<2>@[let %a@ :=@]@ @[%a;@]@]@ %a"
              pp_name_ann (name, def_ty)
              (pp_tm names) def
              (go (name :: names)) body
        | tm -> Format.fprintf fmt "@[%a@]" (pp_tm names) tm
      in
      go names fmt tm
  | FunLit (name, param_ty, body) ->
      Format.fprintf fmt "@[@[fun@ %a@ =>@]@ %a@]"
        pp_param (name, param_ty)
        (pp_tm (name :: names)) body
  | tm ->
      pp_add_tm names fmt tm
and pp_add_tm names fmt =
  function
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
and pp_mul_tm names fmt =
  function
  | PrimApp (`Mul, [arg1; arg2]) ->
      Format.fprintf fmt "@[%a@ *@ %a@]"
        (pp_app_tm names) arg1
        (pp_mul_tm names) arg2
  | tm ->
      pp_app_tm names fmt tm
and pp_app_tm names fmt =
  function
  | FunApp (head, arg) ->
      Format.fprintf fmt "@[%a@ %a@]"
        (pp_app_tm names) head
        (pp_atomic_tm names) arg
  | PrimApp (`Neg, [arg]) ->
      Format.fprintf fmt "@[-%a@]"
        (pp_atomic_tm names) arg
  | tm ->
      pp_atomic_tm names fmt tm
and pp_atomic_tm names fmt =
  function
  | Var index ->
      Format.fprintf fmt "%s" (List.nth names index)
  | IntLit i -> Format.fprintf fmt "%i" i
  (* FIXME: Will loop forever on invalid primitive applications *)
  | tm -> Format.fprintf fmt "@[(%a)@]" (pp_tm names) tm
