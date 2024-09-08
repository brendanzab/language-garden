(** {0 Simply typed lambda calculus (alpha renamed)}

    This is a version of {!Lang.Fun} that uses unique names to represent
    variable binding structure, as opposed to de Bruijn indices.
*)

(** {1 Variables} *)

module Var = Fresh.Make ()

module VarMap = Map.Make (Var)
module VarSet = Set.Make (Var)


(** {1 Syntax} *)

type ty = Lang_Fun.ty =
  | BoolType
  | IntType
  | FunType of ty * ty

type tm =
  | Var of Var.t
  | Let of Var.t * ty * tm * tm
  | BoolLit of bool
  | IntLit of int
  | PrimApp of Prim.t * tm list
  | FunLit of Var.t * ty * tm
  | FunApp of tm * tm


(** Return the part of an environment that is used in a term *)
let rec fvs : tm -> VarSet.t =
  function
  | Var var -> VarSet.singleton var
  | Let (def_var, _, def, body) -> VarSet.union (fvs def) (VarSet.remove def_var (fvs body))
  | BoolLit _ -> VarSet.empty
  | IntLit _ -> VarSet.empty
  | PrimApp (_, args) -> List.fold_left VarSet.union VarSet.empty (List.map fvs args)
  | FunLit (param_var, _, body) -> VarSet.remove param_var (fvs body)
  | FunApp (head, arg) -> VarSet.union (fvs head) (fvs arg)


(** {1 Pretty printing} *)

let pp_ty = Lang_Fun.pp_ty

let pp_var (fmt : Format.formatter) (var : Var.t) =
  Format.fprintf fmt "%s%i"
    (Var.name var)
    (Var.to_int var)

let pp_name_ann (fmt : Format.formatter) (var, ty) =
  Format.fprintf fmt "@[<2>@[%a :@]@ %a@]"
    pp_var var
    pp_ty ty

let pp_param (fmt : Format.formatter) (var, ty) =
  Format.fprintf fmt "@[<2>(@[%a :@]@ %a)@]"
    pp_var var
    pp_ty ty

let rec pp_tm (fmt : Format.formatter) (tm : tm) =
  match tm with
  | Let _ as tm ->
      let rec go fmt tm =
        match tm with
        | Let (def_var, def_ty, def, body) ->
            Format.fprintf fmt "@[<hv 2>@[let %a@ :=@]@ @[%a;@]@]@ %a"
              pp_name_ann (def_var, def_ty)
              pp_tm def
              go body
        | tm -> Format.fprintf fmt "@[%a@]" pp_tm tm
      in
      go fmt tm
  | FunLit (name, param_ty, body) ->
      let rec go fmt tm =
        match tm with
        | FunLit (name, param_ty, body) ->
            Format.fprintf fmt "@ @[fun@ %a@ =>@]%a"
              pp_param (name, param_ty)
              go body
        | tm -> Format.fprintf fmt "@]@ @[%a@]@]" pp_tm tm
      in
      Format.fprintf fmt "@[<hv 2>@[<hv>@[fun@ %a@ =>@]%a"
        pp_param (name, param_ty)
        go body
  | tm ->
      pp_app_tm fmt tm
and pp_app_tm (fmt : Format.formatter) (tm : tm) =
  match tm with
  | PrimApp (head, args) ->
      Format.fprintf fmt "@[#%s@ %a@]"
        (Prim.to_string head)
        (Format.pp_print_list pp_atomic_tm ~pp_sep:Format.pp_print_space) args
  | FunApp (head, arg) ->
      Format.fprintf fmt "@[%a@ %a@]"
        pp_app_tm head
        pp_atomic_tm arg
  | tm ->
      pp_atomic_tm fmt tm
and pp_atomic_tm (fmt : Format.formatter) (tm : tm) =
  match tm with
  | Var var -> pp_var fmt var
  | BoolLit true -> Format.fprintf fmt "true"
  | BoolLit false -> Format.fprintf fmt "false"
  | IntLit i -> Format.fprintf fmt "%i" i
  | tm -> Format.fprintf fmt "@[(%a)@]" pp_tm tm


module Semantics = struct

  (** {1 Values} *)

  type vtm =
    | BoolLit of bool
    | IntLit of int
    | FunLit of Var.t * ty * clos

  and clos = {
    env : vtm VarMap.t;
    body : tm;
  }


  (** {1 Evaluation} *)

  let rec eval env : tm -> vtm =
    function
    | Var var -> VarMap.find var env
    | Let (def_var, _, def, body) ->
        let def = eval env def in
        eval (VarMap.add def_var def env) body
    | BoolLit b -> BoolLit b
    | IntLit i -> IntLit i
    | PrimApp (prim, args) ->
        prim_app prim (List.map (eval env) args)
    | FunLit (param_var, param_ty, body) ->
        (* We do a naive form of dynamic closure conversion here, just capturing
          the entire environment along with the code of the body. We’ll do a
          more thorough job in the compiler though. *)
        FunLit (param_var, param_ty, { env; body })
    | FunApp (head, arg) ->
        let head = eval env head in
        let arg = eval env arg in
        fun_app head arg


  (** {1 Eliminators} *)

  and prim_app prim args =
    match prim, args with
    | `Neg, [IntLit t1] -> IntLit (-t1)
    | `Add, [IntLit t1; IntLit t2] -> IntLit (t1 + t2)
    | `Sub, [IntLit t1; IntLit t2] -> IntLit (t1 - t2)
    | `Mul, [IntLit t1; IntLit t2] -> IntLit (t1 * t2)
    | _, _ -> invalid_arg "invalid prim application"

  and fun_app head arg =
    match head with
    | FunLit (param_var, _, { env; body }) ->
        eval (VarMap.add param_var arg env) body
    | _ -> invalid_arg "expected function"

end


module Validation = struct

  let rec check context tm expected_ty =
    match tm, expected_ty with
    | tm, expected_ty ->
        let ty = synth context tm in
        if ty = expected_ty then () else
          invalid_arg
            (Format.asprintf "@[<v 2>@[mismatched types:@]@ @[expected: %a@]@ @[found: %a@]@]"
              pp_ty expected_ty
              pp_ty ty)

  and synth context tm =
    match tm with
    | Var var ->
        begin match VarMap.find_opt var context with
        | Some ty -> ty
        | None -> invalid_arg "unbound variable"
        end
    | Let (def_var, _, def, body) ->
        let def_ty = synth context def in
        synth (VarMap.add def_var def_ty context) body
    | BoolLit _ -> BoolType
    | IntLit _ -> IntType
    | PrimApp (`Neg, [t]) ->
        check context t IntType;
        IntType
    | PrimApp ((`Add | `Sub | `Mul), [t1; t2]) ->
        check context t1 IntType;
        check context t2 IntType;
        IntType
    | PrimApp _ ->
        invalid_arg "invalid prim application"
    | FunLit (param_var, param_ty, body) ->
        let body_ty = synth (VarMap.add param_var param_ty context) body in
        FunType (param_ty, body_ty)
    | FunApp (head, arg) ->
        begin match synth context head with
        | FunType (param_ty, body_ty) ->
            check context arg param_ty;
            body_ty
        | ty ->
            invalid_arg
              (Format.asprintf "expected function but found term of type %a" pp_ty ty)
        end

end
