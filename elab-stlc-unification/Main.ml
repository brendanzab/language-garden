(** An elaborator for a simply typed lambda calculus with mutable metavars.

    This implementation is based on Arad Arbel’s gist:
    https://gist.github.com/aradarbel10/837aa65d2f06ac6710c6fbe479909b4c
*)

module Core = struct

  (** {1 Types} *)

  type ty =
    | IntType
    | FunType of ty * ty
    | MetaVar of meta_state ref

  and meta_state =
    | Solved of ty
    | Unsolved of int

  (** Create a fresh, unsolved metavariable *)
  let fresh_meta =
    let next_id = ref 0 in
    fun () ->
      let id = !next_id in
      incr next_id;
      MetaVar (ref (Unsolved id))

  (** Force any solved metas on the outermost part of a type *)
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


  (** {1 Pretty printing} *)

  let rec pp_ty fmt =
    function
    | FunType (param_ty, body_ty) ->
        Format.fprintf fmt "%a -> %a"
          pp_atomic_ty param_ty
          pp_ty body_ty
    | ty ->
        pp_atomic_ty fmt ty
  and pp_atomic_ty fmt =
    function
    | IntType -> Format.fprintf fmt "Int"
    | MetaVar m -> pp_meta fmt m
    | ty -> Format.fprintf fmt "@[(%a)@]" pp_ty ty

  and pp_meta fmt m =
    pp_meta_state fmt !m

  and pp_meta_state fmt =
    function
    | Solved ty -> pp_ty fmt ty
    | Unsolved id -> Format.fprintf fmt "?%i" id

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


  module Semantics = struct

    (** {1 Values} *)

    type vtm =
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

  end


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


  (** {1 Zonking} *)

  (** Flattens any solved metavariables in the type. This is imporatant for
      properly pretty printing types, as we want to be able to ‘see through’
      metavariables to properly associate function types. *)
  let rec zonk =
    function
    | IntType -> IntType
    | FunType (param_ty, body_ty) ->
        FunType (zonk param_ty, zonk body_ty)
    | MetaVar m as ty ->
        begin match !m with
        | Solved ty -> zonk ty
        | Unsolved _ -> ty
        end

end


module Surface = struct

  type tm =
    | Var of string
    | Let of string * tm * tm
    | IntLit of int
    | FunLit of string * tm
    | FunApp of tm * tm
    | Op2 of [`Add | `Sub | `Mul] * tm * tm
    | Op1 of [`Neg] * tm


  (** {1 Elaboration} *)

  exception Error of string

  let rec check context (tm : tm) (ty : Core.ty) : Core.tm =
    let tm, ty' = infer context tm in
    try Core.unify ty ty'; tm with
    | Core.InfiniteType _ ->
        raise (Error
          (Format.asprintf "@[<v 2> @[infinite type:@]@ @[expected: %a@]@ @[found: %a@]@]"
            Core.pp_ty (Core.zonk ty)
            Core.pp_ty (Core.zonk ty')))
    | Core.MismatchedTypes (_, _) ->
        raise (Error
          (Format.asprintf "@[<v 2> @[mismatched types:@]@ @[expected: %a@]@ @[found: %a@]@]"
            Core.pp_ty (Core.zonk ty)
            Core.pp_ty (Core.zonk ty')))

  and infer context (tm : tm) : Core.tm * Core.ty =
    match tm with
    | Var name ->
        let rec go index context : Core.tm * Core.ty =
          match context with
          | (name', ty) :: _ when name = name' -> Var index, ty
          | (_, _) :: context -> go (index + 1) context
          | [] -> raise (Error (Format.asprintf "the variable `%s` is not bound in the current scope" name))
        in
        go 0 context
    | Let (def_name, def, body) ->
        let def, def_ty = infer context def in
        let body, body_ty = infer ((def_name, def_ty) :: context) body in
        Let (def_name, def_ty, def, body), body_ty
    | IntLit i -> IntLit i, IntType
    | FunLit (param_name, body) ->
        let param_ty = Core.fresh_meta () in
        let body, body_ty = infer ((param_name, param_ty) :: context) body in
        FunLit (param_name, param_ty, body), FunType (param_ty, body_ty)
    | FunApp (head, arg) ->
        let arg, arg_ty = infer context arg in
        let body_ty = Core.fresh_meta () in
        let head = check context head (FunType (arg_ty, body_ty)) in
        FunApp (head, arg), body_ty
    | Op2 ((`Add | `Sub | `Mul) as prim, tm0, tm1) ->
        let tm0 = check context tm0 IntType in
        let tm1 = check context tm1 IntType in
        PrimApp (prim, [tm0; tm1]), IntType
    | Op1 ((`Neg) as prim, tm) ->
        let tm = check context tm IntType in
        PrimApp (prim, [tm]), IntType

  (* TODO: check unsolved metas - perhaps store in a matacontext? *)

end
