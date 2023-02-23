(** {0 Closure converted functional language}

    Unlike {!FunLang}, this language makes an explicit distinction between the
    ‘code’ of closures, and the captured variables they close over.
*)


(** {1 Syntax} *)

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
  | PrimApp of Prim.t * tm list
  | CodeLit of ty * (string * ty) * tm    (** [ fun env x => e ] *)
  | TupleLit of tm list                   (** [ (e1, ..., en) ] *)
  | TupleProj of tm * int                 (** [ e.n ] *)
  | ClosLit of tm * tm                    (** [ clos(e1, e2) ] *)
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

let pp_name_ann fmt (name, ty) =
  Format.fprintf fmt "@[<2>@[%s :@]@ %a@]" name pp_ty ty

let pp_param fmt (name, ty) =
  Format.fprintf fmt "@[<2>(@[%s :@]@ %a)@]" name pp_ty ty

let rec pp_tm names fmt = function
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
  | CodeLit (env_ty, (name, param_ty), body) ->
      Format.fprintf fmt "@[@[fun@ %a@ %a@ =>@]@ %a@]"
        pp_param ("env", env_ty)
        pp_param (name, param_ty)
        (pp_tm [name; "env"]) body
  | tm ->
      pp_add_tm names fmt tm
and pp_add_tm names fmt = function
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
and pp_mul_tm names fmt = function
  | PrimApp (`Mul, [arg1; arg2]) ->
      Format.fprintf fmt "@[%a@ *@ %a@]"
        (pp_app_tm names) arg1
        (pp_mul_tm names) arg2
  | tm ->
      pp_app_tm names fmt tm
and pp_app_tm names fmt = function
  | ClosApp (head, arg) ->
      Format.fprintf fmt "@[%a@ %a@]"
        (pp_app_tm names) head
        (pp_proj_tm names) arg
  | PrimApp (`Neg, [arg]) ->
      Format.fprintf fmt "@[-%a@]"
        (pp_atomic_tm names) arg
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
      Format.fprintf fmt "@[<2>clos(%a,@ %a)@]"
        (pp_tm names) code
        (pp_tm names) env
  | TupleLit [tm] ->
      Format.fprintf fmt "@[(%a,)@]"
        (pp_tm names) tm
  | TupleLit tms ->
      Format.fprintf fmt "@[(%a)@]"
        (Format.pp_print_list (pp_tm names) ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")) tms
  (* FIXME: Will loop forever on invalid primitive applications *)
  | tm -> Format.fprintf fmt "@[(%a)@]" (pp_tm names) tm


module Semantics = struct

  (** {1 Values} *)

  type vtm =
    | BoolLit of bool
    | IntLit of int
    | CodeLit of ty * (string * ty) * tm   (** [ fun env x => e ] *)
    | TupleLit of vtm list                 (** [ (v1, ..., v2) ] *)
    | ClosLit of vtm * vtm                 (** [ clos(v1, v2)  ] *)


  (** {1 Evaluation} *)

  let rec eval env : tm -> vtm =
    function
    | Var index -> List.nth env index
    | Let (_, _, def, body) ->
        let def = eval env def in
        eval (def :: env) body
    | BoolLit b -> BoolLit b
    | IntLit i -> IntLit i
    | PrimApp (prim, args) ->
        prim_app prim (List.map (eval env) args)
    | CodeLit (env_ty, (name, param_ty), body) ->
        CodeLit (env_ty, (name, param_ty), body)
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
    | ClosLit (CodeLit (_, _, body), env) -> eval [arg; env] body
    | _ -> invalid_arg "expected closure"

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
    | Var index ->
        begin match List.nth_opt context index with
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
        (* Code literals capture no variables from the surrounding context, so
          the body of the closure is synthesised in a new context that assumes
          only the parameter and the environment. *)
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
