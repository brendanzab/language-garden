(** {0 Lambda lifted functional language} *)

(** {1 Variables} *)

(** Global variable identifiers *)
module Global_var = Fresh.Make ()

(** Local variable identifiers *)
module Local_var = Fresh.Make ()

module Local_var_map = Map.Make (Local_var)


(** {1 Syntax} *)

type ty =
  | Bool_type                       (** [ Bool ] *)
  | Int_type                        (** [ Int ] *)
  | Tuple_type of ty list           (** [ (t1, ... tn) ] *)
  | Clos_type of ty * ty            (** [ t1 -> t2 ] *)

type code_ty = {
  env_ty : ty;
  param_ty : ty;
  body_ty : ty;
}

type global_tys = (Global_var.t * code_ty) list

type tm =
  | Local_var of Local_var.t
  | Let of Local_var.t * ty * tm * tm
  | Bool_lit of bool
  | Int_lit of int
  | Prim_app of Prim.t * tm list
  | Code_app of Global_var.t * tm * tm
  | Tuple_lit of tm list
  | Tuple_proj of tm * int
  | Clos_lit of Global_var.t * tm
  | Clos_app of tm * tm

and code = {
  env : Local_var.t * ty;
  param : Local_var.t * ty;
  body : tm;
}

and globals = (Global_var.t * code) list

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

let rec pp_ty (fmt : Format.formatter) (ty : ty) =
  match ty with
  | Clos_type (param_ty, body_ty) ->
      Format.fprintf fmt "%a -> %a"
        pp_atomic_ty param_ty
        pp_ty body_ty
  | ty ->
      pp_atomic_ty fmt ty
and pp_atomic_ty (fmt : Format.formatter) (ty : ty) =
  match ty with
  | Bool_type -> Format.fprintf fmt "Bool"
  | Int_type -> Format.fprintf fmt "Int"
  | Tuple_type tys ->
      Format.fprintf fmt "@[(%a)@]" (pp_tuple_elems pp_ty) tys
  | ty ->
      Format.fprintf fmt "@[(%a)@]" pp_ty ty

let pp_global_var (fmt : Format.formatter) (var : Global_var.t) =
  Format.fprintf fmt "%s%i↑"
    (Global_var.name var)
    (Global_var.to_int var)

let pp_local_var (fmt : Format.formatter) (var : Local_var.t) =
  Format.fprintf fmt "%s%i"
    (Local_var.name var)
    (Local_var.to_int var)

let pp_name_ann (fmt : Format.formatter) (var, ty) =
  Format.fprintf fmt "@[<2>@[%a :@]@ %a@]"
    pp_local_var var
    pp_ty ty

let pp_param (fmt : Format.formatter) (var, ty) =
  Format.fprintf fmt "@[<2>(@[%a :@]@ %a)@]"
    pp_local_var var
    pp_ty ty

let rec pp_tm (fmt : Format.formatter) (tm : tm) =
  match tm with
  | Let _ as tm ->
      let rec go fmt tm =
        match tm with
        | Let (def_var, def_ty, def, body) ->
            Format.fprintf fmt "@[<2>@[let %a@ :=@]@ @[%a;@]@]@ %a"
              pp_name_ann (def_var, def_ty)
              pp_tm def
              go body
        | tm -> Format.fprintf fmt "@[%a@]" pp_tm tm
      in
      go fmt tm
  | tm ->
      pp_app_tm fmt tm
and pp_app_tm (fmt : Format.formatter) (tm : tm) =
  match tm with
  | Prim_app (head, args) ->
      Format.fprintf fmt "@[#%s@ %a@]"
        (Prim.to_string head)
        (Format.pp_print_list pp_proj_tm ~pp_sep:Format.pp_print_space) args
  | Code_app (code, env, arg) ->
      Format.fprintf fmt "@[%a@ %a@ %a@]"
        pp_global_var code
        pp_proj_tm env
        pp_proj_tm arg
  | Clos_app (head, arg) ->
      Format.fprintf fmt "@[%a@ %a@]"
        pp_app_tm head
        pp_proj_tm arg
  | tm ->
      pp_proj_tm fmt tm
and pp_proj_tm (fmt : Format.formatter) (tm : tm) =
  match tm with
  | Tuple_proj (head, label) ->
      Format.fprintf fmt "@[%a.%i@]"
        pp_proj_tm head
        label
  | tm ->
      pp_atomic_tm fmt tm
and pp_atomic_tm (fmt : Format.formatter) (tm : tm) =
  match tm with
  | Local_var var -> pp_local_var fmt var
  | Bool_lit true -> Format.fprintf fmt "true"
  | Bool_lit false -> Format.fprintf fmt "false"
  | Int_lit i -> Format.fprintf fmt "%i" i
  | Clos_lit (code, env) ->
      Format.fprintf fmt "@[<hv>clos(@;<0 2>%a,@;<1 2>%a@;<0 0>)@]"
        pp_global_var code
        pp_tm env
  | Tuple_lit tms ->
      Format.fprintf fmt "@[(%a)@]" (pp_tuple_elems pp_tm) tms
  | tm -> Format.fprintf fmt "@[(%a)@]" pp_tm tm

let pp_lifted_tm (fmt : Format.formatter) (tm : lifted_tm) =
  let rec pp_globals fmt globals =
    match globals with
    | [] -> Format.fprintf fmt ""
    | (var, code) :: globals ->
        Format.fprintf fmt "%a@[<hv 2>@[def@ %a@ %a@ %a@ :=@]@ %a;@]@ "
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
    | Bool_lit of bool
    | Int_lit of int
    | Tuple_lit of vtm list
    | Clos_lit of Global_var.t * vtm


  (** {1 Evaluation} *)

  let rec eval globals locals : tm -> vtm =
    function
    | Local_var var -> Local_var_map.find var locals
    | Let (def_var, _, def, body) ->
        let def = eval globals locals def in
        eval globals (Local_var_map.add def_var def locals) body
    | Bool_lit b -> Bool_lit b
    | Int_lit i -> Int_lit i
    | Prim_app (prim, args) ->
        prim_app prim (List.map (eval globals locals) args)
    | Code_app (head, env, arg) ->
        let env = eval globals locals env in
        let arg = eval globals locals arg in
        code_app globals head env arg
    | Tuple_lit tms ->
        Tuple_lit (List.map (eval globals locals) tms)
    | Tuple_proj (head, label) ->
        let head = eval globals locals head in
        tuple_proj head label
    | Clos_lit (code, env') ->
        Clos_lit (code, eval globals locals env')
    | Clos_app (head, arg) ->
        let head = eval globals locals head in
        let arg = eval globals locals arg in
        clos_app globals head arg

  and eval_lifted (tm : lifted_tm) : vtm =
    eval tm.globals Local_var_map.empty tm.main


  (** {1 Eliminators} *)

  and prim_app prim args =
    match prim, args with
    | `Neg, [Int_lit t1] -> Int_lit (-t1)
    | `Add, [Int_lit t1; Int_lit t2] -> Int_lit (t1 + t2)
    | `Sub, [Int_lit t1; Int_lit t2] -> Int_lit (t1 - t2)
    | `Mul, [Int_lit t1; Int_lit t2] -> Int_lit (t1 * t2)
    | _, _ -> invalid_arg "invalid prim application"

  and code_app globals head env arg =
    let code = List.assoc head globals in
    let locals =
      Local_var_map.empty
      |> Local_var_map.add (fst code.env) env
      |> Local_var_map.add (fst code.param) arg
    in
    eval globals locals code.body

  and tuple_proj head label =
    match head with
    | Tuple_lit vtms -> List.nth vtms label
    | _ -> invalid_arg "expected tuple"

  and clos_app globals head arg =
    match head with
    | Clos_lit (code, env) ->
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
    | Local_var var ->
        begin match Local_var_map.find_opt var locals with
        | Some ty -> ty
        | None -> invalid_arg "unbound variable"
        end
    | Let (def_var, _, def, body) ->
        let def_ty = synth globals locals def in
        synth globals (Local_var_map.add def_var def_ty locals) body
    | Bool_lit _ -> Bool_type
    | Int_lit _ -> Int_type
    | Prim_app (`Neg, [t]) ->
        check globals locals t Int_type;
        Int_type
    | Prim_app ((`Add | `Sub | `Mul), [t1; t2]) ->
        check globals locals t1 Int_type;
        check globals locals t2 Int_type;
        Int_type
    | Prim_app _ ->
        invalid_arg "invalid prim application"
    | Code_app (code, env, arg) ->
        let code_ty = List.assoc code globals in
        check globals locals env code_ty.env_ty;
        check globals locals arg code_ty.param_ty;
        code_ty.body_ty
    | Tuple_lit tms ->
        Tuple_type (List.map (synth globals locals) tms)
    | Tuple_proj (head, label) ->
        begin match synth globals locals head with
        | Tuple_type tys ->
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
    | Clos_lit (code, env) ->
        let code_ty = List.assoc code globals in
        check globals locals env code_ty.env_ty;
        Clos_type (code_ty.param_ty, code_ty.body_ty)
    | Clos_app (head, arg) ->
        begin match synth globals locals head with
        | Clos_type (param_ty, body_ty) ->
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
          Local_var_map.empty
          |> Local_var_map.add (fst code.env) (snd code.env)
          |> Local_var_map.add (fst code.param) (snd code.param)
        in
        let code_ty = {
          env_ty = snd code.env;
          param_ty = snd code.param;
          body_ty = synth globals locals code.body
        } in
        (var, code_ty) :: globals

  let synth_lifted (tm : lifted_tm) : ty =
    let globals = synth_globals tm.globals in
    synth globals Local_var_map.empty tm.main

end
