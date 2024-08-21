(** {0 Lambda lifted functional language} *)

(** {1 Variables} *)

(** Global variable identifiers *)
module GlobalVar = Fresh.Make ()

(** Local variable identifiers *)
module LocalVar = Fresh.Make ()

module LocalVarMap = Map.Make (LocalVar)


(** {1 Syntax} *)

type ty =
  | BoolType                        (** [ Bool ] *)
  | IntType                         (** [ Int ] *)
  | TupleType of ty list            (** [ (t1, ... tn) ] *)
  | ClosType of ty * ty             (** [ t1 -> t2 ] *)

type code_ty = {
  env_ty : ty;
  param_ty : ty;
  body_ty : ty;
}

type global_tys = (GlobalVar.t * code_ty) list

type tm =
  | LocalVar of LocalVar.t
  | Let of LocalVar.t * ty * tm * tm
  | BoolLit of bool
  | IntLit of int
  | PrimApp of Prim.t * tm list
  | CodeApp of GlobalVar.t * tm * tm
  | TupleLit of tm list
  | TupleProj of tm * int
  | ClosLit of GlobalVar.t * tm
  | ClosApp of tm * tm

and code = {
  env : LocalVar.t * ty;
  param : LocalVar.t * ty;
  body : tm;
}

and globals = (GlobalVar.t * code) list

and lifted_tm = {
  globals : globals;
  main : tm;
}


(** {1 Pretty printing} *)

let pp_comma_sep (fmt : Format.formatter) () =
  Format.fprintf fmt ",@ "

let pp_tuple_elems (type a) (pp_elem : Format.formatter -> a -> unit) (fmt : Format.formatter) (elems : a list) =
  match elems with
  | [elem] -> Format.fprintf fmt "%a," pp_elem elem
  | elems ->
      Format.fprintf fmt "%a"
        (Format.pp_print_list pp_elem ~pp_sep:pp_comma_sep) elems

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
  | TupleType tys ->
      Format.fprintf fmt "@[(%a)@]" (pp_tuple_elems pp_ty) tys
  | ty ->
      Format.fprintf fmt "@[(%a)@]" pp_ty ty

let pp_global_var fmt (var : GlobalVar.t) =
  Format.fprintf fmt "%s%i↑"
    (GlobalVar.name var)
    (GlobalVar.to_int var)

let pp_local_var fmt (var : LocalVar.t) =
  Format.fprintf fmt "%s%i"
    (LocalVar.name var)
    (LocalVar.to_int var)

let pp_name_ann fmt (var, ty) =
  Format.fprintf fmt "@[<2>@[%a :@]@ %a@]"
    pp_local_var var
    pp_ty ty

let pp_param fmt (var, ty) =
  Format.fprintf fmt "@[<2>(@[%a :@]@ %a)@]"
    pp_local_var var
    pp_ty ty

let rec pp_tm fmt = function
  | Let _ as tm ->
      let rec go fmt = function
        | Let (def_var, def_ty, def, body) ->
            Format.fprintf fmt "@[<2>@[let %a@ :=@]@ @[%a;@]@]@ %a"
              pp_name_ann (def_var, def_ty)
              pp_tm def
              go body
        | tm -> Format.fprintf fmt "@[%a@]" pp_tm tm
      in
      go fmt tm
  | tm ->
      pp_add_tm fmt tm
and pp_add_tm fmt = function
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
and pp_mul_tm fmt = function
  | PrimApp (`Mul, [arg1; arg2]) ->
      Format.fprintf fmt "@[%a@ *@ %a@]"
        pp_app_tm arg1
        pp_mul_tm arg2
  | tm ->
      pp_app_tm fmt tm
and pp_app_tm fmt = function
  | CodeApp (code, env, arg) ->
      Format.fprintf fmt "@[%a@ %a@ %a@]"
        pp_global_var code
        pp_proj_tm env
        pp_proj_tm arg
  | ClosApp (head, arg) ->
      Format.fprintf fmt "@[%a@ %a@]"
        pp_app_tm head
        pp_proj_tm arg
  | PrimApp (`Neg, [arg]) ->
      Format.fprintf fmt "@[-%a@]"
        pp_atomic_tm arg
  | tm ->
      pp_proj_tm fmt tm
and pp_proj_tm fmt = function
  | TupleProj (head, label) ->
      Format.fprintf fmt "@[%a.%i@]"
        pp_proj_tm head
        label
  | tm ->
      pp_atomic_tm fmt tm
and pp_atomic_tm fmt = function
  | LocalVar var -> pp_local_var fmt var
  | BoolLit true -> Format.fprintf fmt "true"
  | BoolLit false -> Format.fprintf fmt "false"
  | IntLit i -> Format.fprintf fmt "%i" i
  | ClosLit (code, env) ->
      Format.fprintf fmt "@[<2>clos(%a,@ %a)@]"
        pp_global_var code
        pp_tm env
  | TupleLit tms ->
      Format.fprintf fmt "@[(%a)@]" (pp_tuple_elems pp_tm) tms
  (* FIXME: Will loop forever on invalid primitive applications *)
  | tm -> Format.fprintf fmt "@[(%a)@]" pp_tm tm

let pp_lifted_tm fmt tm =
  let rec pp_globals fmt =
    function
    | [] -> Format.fprintf fmt ""
    | (var, code) :: globals ->
        Format.fprintf fmt "%a@[<2>@[def@ %a@ %a@ %a@ :=@]@ %a;@]@ "
          pp_globals globals
          pp_global_var var
          pp_param code.env
          pp_param code.param
          pp_tm code.body
  in
  Format.fprintf fmt "@[<v>%a%a@]"
    pp_globals tm.globals
    pp_tm tm.main


module Semantics = struct

  (** {1 Values} *)

  type vtm =
    | BoolLit of bool
    | IntLit of int
    | TupleLit of vtm list
    | ClosLit of GlobalVar.t * vtm


  (** {1 Evaluation} *)

  let rec eval globals locals : tm -> vtm =
    function
    | LocalVar var -> LocalVarMap.find var locals
    | Let (def_var, _, def, body) ->
        let def = eval globals locals def in
        eval globals (LocalVarMap.add def_var def locals) body
    | BoolLit b -> BoolLit b
    | IntLit i -> IntLit i
    | PrimApp (prim, args) ->
        prim_app prim (List.map (eval globals locals) args)
    | CodeApp (head, env, arg) ->
        let env = eval globals locals env in
        let arg = eval globals locals arg in
        code_app globals head env arg
    | TupleLit tms ->
        TupleLit (List.map (eval globals locals) tms)
    | TupleProj (head, label) ->
        let head = eval globals locals head in
        tuple_proj head label
    | ClosLit (code, env') ->
        ClosLit (code, eval globals locals env')
    | ClosApp (head, arg) ->
        let head = eval globals locals head in
        let arg = eval globals locals arg in
        clos_app globals head arg

  and eval_lifted (tm : lifted_tm) : vtm =
    eval tm.globals LocalVarMap.empty tm.main


  (** {1 Eliminators} *)

  and prim_app prim args =
    match prim, args with
    | `Neg, [IntLit t1] -> IntLit (-t1)
    | `Add, [IntLit t1; IntLit t2] -> IntLit (t1 + t2)
    | `Sub, [IntLit t1; IntLit t2] -> IntLit (t1 - t2)
    | `Mul, [IntLit t1; IntLit t2] -> IntLit (t1 * t2)
    | _, _ -> invalid_arg "invalid prim application"

  and code_app globals head env arg =
    let code = List.assoc head globals in
    let locals =
      LocalVarMap.empty
      |> LocalVarMap.add (fst code.env) env
      |> LocalVarMap.add (fst code.param) arg
    in
    eval globals locals code.body

  and tuple_proj head label =
    match head with
    | TupleLit vtms -> List.nth vtms label
    | _ -> invalid_arg "expected tuple"

  and clos_app globals head arg =
    match head with
    | ClosLit (code, env) ->
        code_app globals code env arg
    | _ -> invalid_arg "expected closure"

end


module Validation = struct

  let rec check globals locals (tm : tm) (expected_ty : ty) : unit =
    match tm, expected_ty with
    | tm, expected_ty ->
        let ty = synth globals locals tm in
        if ty = expected_ty then () else
          invalid_arg
            (Format.asprintf "@[<v 2>@[mismatched types:@]@ @[expected: %a@]@ @[found: %a@]@]"
              pp_ty expected_ty
              pp_ty ty)

  and synth globals locals (tm : tm) : ty =
    match tm with
    | LocalVar var ->
        begin match LocalVarMap.find_opt var locals with
        | Some ty -> ty
        | None -> invalid_arg "unbound variable"
        end
    | Let (def_var, _, def, body) ->
        let def_ty = synth globals locals def in
        synth globals (LocalVarMap.add def_var def_ty locals) body
    | BoolLit _ -> BoolType
    | IntLit _ -> IntType
    | PrimApp (`Neg, [t]) ->
        check globals locals t IntType;
        IntType
    | PrimApp ((`Add | `Sub | `Mul), [t1; t2]) ->
        check globals locals t1 IntType;
        check globals locals t2 IntType;
        IntType
    | PrimApp _ ->
        invalid_arg "invalid prim application"
    | CodeApp (code, env, arg) ->
        let code_ty = List.assoc code globals in
        check globals locals env code_ty.env_ty;
        check globals locals arg code_ty.param_ty;
        code_ty.body_ty
    | TupleLit tms ->
        TupleType (List.map (synth globals locals) tms)
    | TupleProj (head, label) ->
        begin match synth globals locals head with
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
        let code_ty = List.assoc code globals in
        check globals locals env code_ty.env_ty;
        ClosType (code_ty.param_ty, code_ty.body_ty)
    | ClosApp (head, arg) ->
        begin match synth globals locals head with
        | ClosType (param_ty, body_ty) ->
            check globals locals arg param_ty;
            body_ty
        | ty ->
            invalid_arg
              (Format.asprintf "expected closure but found term of type %a" pp_ty ty)
        end

  let rec synth_globals : globals -> global_tys =
    function
    | [] -> []
    | (var, code) :: globals ->
        let globals = synth_globals globals in
        let locals =
          LocalVarMap.empty
          |> LocalVarMap.add (fst code.env) (snd code.env)
          |> LocalVarMap.add (fst code.param) (snd code.param)
        in
        let code_ty = {
          env_ty = snd code.env;
          param_ty = snd code.param;
          body_ty = synth globals locals code.body
        } in
        (var, code_ty) :: globals

  let synth_lifted (tm : lifted_tm) : ty =
    let globals = synth_globals tm.globals in
    synth globals LocalVarMap.empty tm.main

end
