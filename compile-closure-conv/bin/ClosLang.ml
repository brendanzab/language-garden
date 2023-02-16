(** {0 A functional language with explicit closures} *)

(** This language makes an explicit distinction between the ‘code’ of closures,
    and the captured data they close over. *)


(** {1 Syntax} *)

type prim = [
  | `Neg
  | `Add
  | `Sub
  | `Mul
]

let string_of_prim : prim -> string =
  function
  | `Neg -> "#neg"
  | `Add -> "#add"
  | `Sub -> "#sub"
  | `Mul -> "#mul"


type ty =
  | BoolType                        (** [ Bool ] *)
  | IntType                         (** [ Int ] *)
  | CodeType of ty * ty * ty        (** [ Code(t_env, t1, t2) ] *)
  | TupleType of ty list            (** [ (t1, ... tn) ] *)
  | ClosType of ty * ty             (** [ t1 -> t2 ] *)

type tm =
  | Var of int
  | Let of string * ty * tm * tm
  | BoolLit of bool
  | IntLit of int
  | PrimApp of prim * tm list
  | CodeLit of ty * (string * ty) * tm    (** [ fun env x => e ] *)
  | TupleLit of tm list                   (** [ (e1, ..., e2) ] *)
  | TupleProj of tm * int                 (** [ e.n ] *)
  | ClosLit of tm * tm                    (** [ ⟪ e1, e2 ⟫  ] *)
  | ClosApp of tm * tm                    (** [ e1 e2 ] *)


(** {1 Pretty printing} *)

let rec pp_ty fmt =
  function
  | ClosType (param_ty, body_ty) ->
      Format.fprintf fmt "%a -> %a"
        pp_atomic_ty param_ty
        pp_ty body_ty
  | ty ->
      pp_atomic_ty fmt ty
and pp_atomic_ty fmt =
  function
  | BoolType -> Format.fprintf fmt "Bool"
  | IntType -> Format.fprintf fmt "Int"
  | CodeType (env_ty, param_ty, body_ty) ->
      Format.fprintf fmt "@[Code(%a,@ %a,@ %a)@]"
        pp_ty env_ty
        pp_ty param_ty
        pp_ty body_ty
  | TupleType tys ->
      Format.fprintf fmt "@[(%a)@]"
        (Format.pp_print_list pp_ty ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")) tys
  | ty ->
      Format.fprintf fmt "@[(%a)@]" pp_ty ty


let rec pp_tm names fmt = function
  | Let (name, def_ty, def, body) ->
      Format.fprintf fmt "@[<2>@[let@ %s@ :@ %a@ :=@]@ %a;@]@ %a"
        name
        pp_ty def_ty
        (pp_tm names) def
        (pp_tm (name :: names)) body
  | CodeLit (env_ty, (name, param_ty), body) ->
      Format.fprintf fmt "@[@[fun@ @[(%s@ :@ %a)@]@ @[(%s@ :@ %a)@]@ =>@]@ %a@]"
        "env"
        pp_ty env_ty
        name
        pp_ty param_ty
        (pp_tm [name; "env"]) body
  | tm ->
      pp_app_tm names fmt tm
and pp_app_tm names fmt = function
  | ClosApp (head, arg) ->
      Format.fprintf fmt "@[%a@ %a@]"
        (pp_app_tm names) head
        (pp_proj_tm names) arg
  | PrimApp (prim, args) ->
      Format.fprintf fmt "@[<2>%s@ %a@]"
        (string_of_prim prim)
        (Format.pp_print_list (pp_proj_tm names) ~pp_sep:Format.pp_print_space) args
  | tm ->
      pp_proj_tm names fmt tm
and pp_proj_tm names fmt = function
  | TupleProj (head, label) ->
      Format.fprintf fmt "@[%a.%i@]"
        (pp_proj_tm names) head
        label
  | tm ->
      pp_atomic_tm names fmt tm
and pp_atomic_tm names fmt = function
  | Var index ->
      Format.fprintf fmt "%s" (List.nth names index)
  | BoolLit true -> Format.fprintf fmt "true"
  | BoolLit false -> Format.fprintf fmt "false"
  | IntLit i -> Format.fprintf fmt "%i" i
  | ClosLit (code, env) ->
      Format.fprintf fmt "@[<2>⟪%a,@ %a⟫@]"
        (pp_tm names) code
        (pp_tm names) env
  | TupleLit tms ->
      Format.fprintf fmt "@[⟨%a⟩@]"
        (Format.pp_print_list (pp_tm names) ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")) tms
  | tm -> Format.fprintf fmt "@[(%a)@]" (pp_tm names) tm


module Semantics = struct

  type vtm =
    | BoolLit of bool
    | IntLit of int
    | CodeLit of ty * (string * ty) * tm   (** [ λ env. λ x. e ] *)
    | TupleLit of vtm list                 (** [ (v1, ..., v2) ] *)
    | ClosLit of vtm * vtm                 (** [ ⟪ v1, v2 ⟫  ] *)


  let rec eval env : tm -> vtm =
    function
    | Var x -> List.nth env x
    | Let (_, _, def, body) ->
        let def = eval env def in
        eval (def :: env) body
    | BoolLit b -> BoolLit b
    | IntLit i -> IntLit i
    | PrimApp (prim, args) ->
        begin match prim, List.map (eval env) args with
        | `Neg, [IntLit t1] -> IntLit (-t1)
        | `Add, [IntLit t1; IntLit t2] -> IntLit (t1 + t2)
        | `Sub, [IntLit t1; IntLit t2] -> IntLit (t1 - t2)
        | `Mul, [IntLit t1; IntLit t2] -> IntLit (t1 * t2)
        | _, _ -> invalid_arg "invalid prim application"
        end
    | CodeLit (env_ty, (name, param_ty), body) ->
        CodeLit (env_ty, (name, param_ty), body)
    | TupleLit tms ->
        TupleLit (List.map (eval env) tms)
    | TupleProj (head, label) ->
        begin match eval env head with
        | TupleLit vtms -> List.nth vtms label
        | _ -> invalid_arg "expected tuple"
        end
    | ClosLit (code, env') -> ClosLit (eval env code, eval env env')
    | ClosApp (head, arg) ->
        begin match eval env head with
        | ClosLit (CodeLit (_, _, body), env') ->
            let arg = eval env arg in
            eval [arg; env'] body
        | _ -> invalid_arg "expected closure"
        end

end


module Validation = struct

  let rec check context tm expected_ty =
    match tm, expected_ty with
    | tm, expected_ty ->
        let ty = synth context tm in
        if ty = expected_ty then () else
          invalid_arg "mismatched types"

  and synth context tm =
    match tm with
    | Var x ->
        begin match List.nth_opt context x with
        | Some ty -> ty
        | None -> invalid_arg "unbound variable"
        end
    | Let (_, _, def, body) ->
        let def_ty = synth context def in
        synth (def_ty :: context) body
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
    | CodeLit (env_ty, (_, param_ty), body) ->
        (* Because code literals capture no variables from the surrounding
           context, the body of the closure is synthesised in a new context
           that assumes only the parameter and the environment. *)
        let body_ty = synth [param_ty; env_ty] body in
        CodeType (env_ty, param_ty, body_ty)
    | TupleLit tms ->
        TupleType (List.map (synth context) tms)
    | TupleProj (head, label) ->
        begin match synth context head with
        | TupleType tys ->
            begin match List.nth_opt tys label with
            | Some ty -> ty
            | None -> invalid_arg "invalid tuple label"
            end
        | _ -> invalid_arg "not a tuple"
        end
    | ClosLit (code, env) ->
        begin match synth context code with
        | CodeType (env_ty, param_ty, body_ty) ->
            check context env env_ty;
            ClosType (param_ty, body_ty)
        | _ -> invalid_arg "expected code"
        end
    | ClosApp (head, arg) ->
        begin match synth context head with
        | ClosType (param_ty, body_ty) ->
            check context arg param_ty;
            body_ty
        | _ -> invalid_arg "expected closure"
        end

end
