(** {0 Closure converted functional language (alpha renamed)}

    This is a version of {!Lang.Clos} that uses unique names to represent
    variable binding structure, as opposed to de Bruijn indices.
*)

(** {1 Variables} *)

module Var = Fresh.Make ()

module VarMap = Map.Make (Var)
module VarSet = Set.Make (Var)


(** {1 Syntax} *)

type ty =
  | BoolType                        (** [ Bool ] *)
  | IntType                         (** [ Int ] *)
  | CodeType of ty * ty * ty        (** [ Code(t_env, t1, t2) ] *)
  | TupleType of ty list            (** [ (t1, ... tn) ] *)
  | ClosType of ty * ty             (** [ t1 -> t2 ] *)

type tm =
  | Var of Var.t
  | Let of Var.t * ty * tm * tm
  | BoolLit of bool
  | IntLit of int
  | PrimApp of Prim.t * tm list
  | CodeLit of (Var.t * ty) * (Var.t * ty) * tm
  | TupleLit of tm list
  | TupleProj of tm * int
  | ClosLit of tm * tm
  | ClosApp of tm * tm


(** {1 Pretty printing} *)

let pp_comma_sep (fmt : Format.formatter) () =
  Format.fprintf fmt ",@ "

let pp_tuple_elems (type a) (pp_elem : Format.formatter -> a -> unit) (fmt : Format.formatter) (elems : a list) =
  match elems with
  | [elem] -> Format.fprintf fmt "%a," pp_elem elem
  | elems ->
      Format.fprintf fmt "%a"
        (Format.pp_print_list pp_elem ~pp_sep:pp_comma_sep) elems

let rec pp_ty (fmt : Format.formatter) (ty : ty) =
  match ty with
  | ClosType (param_ty, body_ty) ->
      Format.fprintf fmt "%a -> %a"
        pp_atomic_ty param_ty
        pp_ty body_ty
  | ty ->
      pp_atomic_ty fmt ty
and pp_atomic_ty (fmt : Format.formatter) (ty : ty) =
  match ty with
  | BoolType -> Format.fprintf fmt "Bool"
  | IntType -> Format.fprintf fmt "Int"
  | CodeType (env_ty, param_ty, body_ty) ->
      Format.fprintf fmt "@[Code(%a,@ %a,@ %a)@]"
        pp_ty env_ty
        pp_ty param_ty
        pp_ty body_ty
  | TupleType tys ->
      Format.fprintf fmt "@[(%a)@]" (pp_tuple_elems pp_ty) tys
  | ty ->
      Format.fprintf fmt "@[(%a)@]" pp_ty ty

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
      let rec go (fmt : Format.formatter) (tm : tm) =
        match tm with
        | Let (def_var, def_ty, def, body) ->
            Format.fprintf fmt "@[<2>@[let %a@ :=@]@ @[%a;@]@]@ %a"
              pp_name_ann (def_var, def_ty)
              pp_tm def
              go body
        | tm -> Format.fprintf fmt "@[%a@]" pp_tm tm
      in
      go fmt tm
  | CodeLit ((env_var, env_ty), (param_var, param_ty), body) ->
      Format.fprintf fmt "@[@[fun@ %a@ %a@ =>@]@ %a@]"
        pp_param (env_var, env_ty)
        pp_param (param_var, param_ty)
        pp_tm body
  | tm ->
      pp_add_tm fmt tm
and pp_add_tm (fmt : Format.formatter) (tm : tm) =
  match tm with
  | PrimApp (`Add, [arg1; arg2]) ->
      Format.fprintf fmt "@[%a@ +@ %a@]"
        pp_mul_tm arg1
        pp_add_tm arg2
  | PrimApp (`Sub, [arg1; arg2]) ->
      Format.fprintf fmt "@[%a@ -@ %a@]"
        pp_mul_tm arg1
        pp_add_tm arg2
  | tm ->
      pp_mul_tm fmt tm
and pp_mul_tm (fmt : Format.formatter) (tm : tm) =
  match tm with
  | PrimApp (`Mul, [arg1; arg2]) ->
      Format.fprintf fmt "@[%a@ *@ %a@]"
        pp_app_tm arg1
        pp_mul_tm arg2
  | tm ->
      pp_app_tm fmt tm
and pp_app_tm (fmt : Format.formatter) (tm : tm) =
  match tm with
  | ClosApp (head, arg) ->
      Format.fprintf fmt "@[%a@ %a@]"
        pp_app_tm head
        pp_proj_tm arg
  | PrimApp (`Neg, [arg]) ->
      Format.fprintf fmt "@[-%a@]"
        pp_atomic_tm arg
  | tm ->
      pp_proj_tm fmt tm
and pp_proj_tm (fmt : Format.formatter) (tm : tm) =
  match tm with
  | TupleProj (head, label) ->
      Format.fprintf fmt "@[%a.%i@]"
        pp_proj_tm head
        label
  | tm ->
      pp_atomic_tm fmt tm
and pp_atomic_tm (fmt : Format.formatter) (tm : tm) =
  match tm with
  | Var var -> pp_var fmt var
  | BoolLit true -> Format.fprintf fmt "true"
  | BoolLit false -> Format.fprintf fmt "false"
  | IntLit i -> Format.fprintf fmt "%i" i
  | ClosLit (code, env) ->
      Format.fprintf fmt "@[<2>clos(%a,@ %a)@]"
        pp_tm code
        pp_tm env
  | TupleLit tms ->
      Format.fprintf fmt "@[(%a)@]" (pp_tuple_elems pp_tm) tms
  (* FIXME: Will loop forever on invalid primitive applications *)
  | tm -> Format.fprintf fmt "@[(%a)@]" pp_tm tm


module Semantics = struct

  (** {1 Values} *)

  type vtm =
    | BoolLit of bool
    | IntLit of int
    | CodeLit of (Var.t * ty) * (Var.t * ty) * tm
    | TupleLit of vtm list
    | ClosLit of vtm * vtm


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
    | CodeLit (env, param, body) ->
        CodeLit (env, param, body)
    | TupleLit tms ->
        TupleLit (List.map (eval env) tms)
    | TupleProj (head, label) ->
        let head = eval env head in
        tuple_proj head label
    | ClosLit (code, env') ->
        ClosLit (eval env code, eval env env')
    | ClosApp (head, arg) ->
        let head = eval env head in
        let arg = eval env arg in
        clos_app head arg


  (** {1 Eliminators} *)

  and prim_app prim args =
    match prim, args with
    | `Neg, [IntLit t1] -> IntLit (-t1)
    | `Add, [IntLit t1; IntLit t2] -> IntLit (t1 + t2)
    | `Sub, [IntLit t1; IntLit t2] -> IntLit (t1 - t2)
    | `Mul, [IntLit t1; IntLit t2] -> IntLit (t1 * t2)
    | _, _ -> invalid_arg "invalid prim application"

  and tuple_proj head label =
    match head with
    | TupleLit vtms -> List.nth vtms label
    | _ -> invalid_arg "expected tuple"

  and clos_app head arg =
    match head with
    | ClosLit (CodeLit ((env_var, _), (param_var, _), body), env) ->
        let env =
          VarMap.empty
          |> VarMap.add env_var env
          |> VarMap.add param_var arg
        in
        eval env body
    | _ -> invalid_arg "expected closure"

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
    | CodeLit ((env_var, env_ty), (param_var, param_ty), body) ->
        (* Code literals capture no variables from the surrounding context, so
          the body of the closure is synthesised in a new context that assumes
          only the parameter and the environment. *)
        let body_context =
          VarMap.empty
          |> VarMap.add env_var env_ty
          |> VarMap.add param_var param_ty
        in
        let body_ty = synth body_context body in
        CodeType (env_ty, param_ty, body_ty)
    | TupleLit tms ->
        TupleType (List.map (synth context) tms)
    | TupleProj (head, label) ->
        begin match synth context head with
        | TupleType tys ->
            begin match List.nth_opt tys label with
            | Some ty -> ty
            | None ->
                invalid_arg
                  (Format.asprintf "projected %i on a tuple with %i elements"
                    label (List.length tys))
            end
        | ty ->
            invalid_arg
              (Format.asprintf "expected tuple but found term of type %a" pp_ty ty)
        end
    | ClosLit (code, env) ->
        begin match synth context code with
        | CodeType (env_ty, param_ty, body_ty) ->
            check context env env_ty;
            ClosType (param_ty, body_ty)
        | ty ->
            invalid_arg
              (Format.asprintf "expected code but found term of type %a" pp_ty ty)
        end
    | ClosApp (head, arg) ->
        begin match synth context head with
        | ClosType (param_ty, body_ty) ->
            check context arg param_ty;
            body_ty
        | ty ->
            invalid_arg
              (Format.asprintf "expected closure but found term of type %a" pp_ty ty)
        end

end
