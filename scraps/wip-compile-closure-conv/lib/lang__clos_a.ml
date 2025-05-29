(** {0 Closure converted functional language (alpha renamed)}

    This is a version of {!Lang.Clos} that uses unique names to represent
    variable binding structure, as opposed to de Bruijn indices.
*)

(** {1 Variables} *)

module Var = Fresh.Make ()

module Var_map = Map.Make (Var)
module Var_set = Set.Make (Var)


(** {1 Syntax} *)

type ty =
  | Bool_type                       (** [ Bool ] *)
  | Int_type                        (** [ Int ] *)
  | Code_type of ty * ty * ty       (** [ Code(t_env, t1, t2) ] *)
  | Tuple_type of ty list           (** [ (t1, ... tn) ] *)
  | Clos_type of ty * ty            (** [ t1 -> t2 ] *)

type tm =
  | Var of Var.t
  | Let of Var.t * ty * tm * tm
  | Bool_lit of bool
  | Int_lit of int
  | Prim_app of Prim.t * tm list
  | Code_lit of (Var.t * ty) * (Var.t * ty) * tm
  | Tuple_lit of tm list
  | Tuple_proj of tm * int
  | Clos_lit of tm * tm
  | Clos_app of tm * tm


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
  | Code_type (env_ty, param_ty, body_ty) ->
      Format.fprintf fmt "@[Code(%a,@ %a,@ %a)@]"
        pp_ty env_ty
        pp_ty param_ty
        pp_ty body_ty
  | Tuple_type tys ->
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
  | Code_lit ((env_var, env_ty), (param_var, param_ty), body) ->
      Format.fprintf fmt "@[<hv 2>@[fun@ %a@ %a@ =>@]@ %a@]"
        pp_param (env_var, env_ty)
        pp_param (param_var, param_ty)
        pp_tm body
  | tm ->
      pp_app_tm fmt tm
and pp_app_tm (fmt : Format.formatter) (tm : tm) =
  match tm with
  | Prim_app (head, args) ->
      Format.fprintf fmt "@[#%s@ %a@]"
        (Prim.to_string head)
        (Format.pp_print_list pp_proj_tm ~pp_sep:Format.pp_print_space) args
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
  | Var var -> pp_var fmt var
  | Bool_lit true -> Format.fprintf fmt "true"
  | Bool_lit false -> Format.fprintf fmt "false"
  | Int_lit i -> Format.fprintf fmt "%i" i
  | Clos_lit (code, env) ->
      Format.fprintf fmt "@[<hv>clos(@;<0 2>%a,@;<1 2>%a@;<0 0>)@]"
        pp_tm code
        pp_tm env
  | Tuple_lit tms ->
      Format.fprintf fmt "@[(%a)@]" (pp_tuple_elems pp_tm) tms
  | tm -> Format.fprintf fmt "@[(%a)@]" pp_tm tm


module Semantics = struct

  (** {1 Values} *)

  type vtm =
    | Bool_lit of bool
    | Int_lit of int
    | Code_lit of (Var.t * ty) * (Var.t * ty) * tm
    | Tuple_lit of vtm list
    | Clos_lit of vtm * vtm


  (** {1 Evaluation} *)

  let rec eval env : tm -> vtm =
    function
    | Var var -> Var_map.find var env
    | Let (def_var, _, def, body) ->
        let def = eval env def in
        eval (Var_map.add def_var def env) body
    | Bool_lit b -> Bool_lit b
    | Int_lit i -> Int_lit i
    | Prim_app (prim, args) ->
        prim_app prim (List.map (eval env) args)
    | Code_lit (env, param, body) ->
        Code_lit (env, param, body)
    | Tuple_lit tms ->
        Tuple_lit (List.map (eval env) tms)
    | Tuple_proj (head, label) ->
        let head = eval env head in
        tuple_proj head label
    | Clos_lit (code, env') ->
        Clos_lit (eval env code, eval env env')
    | Clos_app (head, arg) ->
        let head = eval env head in
        let arg = eval env arg in
        clos_app head arg


  (** {1 Eliminators} *)

  and prim_app prim args =
    match prim, args with
    | `Neg, [Int_lit t1] -> Int_lit (-t1)
    | `Add, [Int_lit t1; Int_lit t2] -> Int_lit (t1 + t2)
    | `Sub, [Int_lit t1; Int_lit t2] -> Int_lit (t1 - t2)
    | `Mul, [Int_lit t1; Int_lit t2] -> Int_lit (t1 * t2)
    | _, _ -> invalid_arg "invalid prim application"

  and tuple_proj head label =
    match head with
    | Tuple_lit vtms -> List.nth vtms label
    | _ -> invalid_arg "expected tuple"

  and clos_app head arg =
    match head with
    | Clos_lit (Code_lit ((env_var, _), (param_var, _), body), env) ->
        let env =
          Var_map.empty
          |> Var_map.add env_var env
          |> Var_map.add param_var arg
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
        begin match Var_map.find_opt var context with
        | Some ty -> ty
        | None -> invalid_arg "unbound variable"
        end
    | Let (def_var, _, def, body) ->
        let def_ty = synth context def in
        synth (Var_map.add def_var def_ty context) body
    | Bool_lit _ -> Bool_type
    | Int_lit _ -> Int_type
    | Prim_app (`Neg, [t]) ->
        check context t Int_type;
        Int_type
    | Prim_app ((`Add | `Sub | `Mul), [t1; t2]) ->
        check context t1 Int_type;
        check context t2 Int_type;
        Int_type
    | Prim_app _ ->
        invalid_arg "invalid prim application"
    | Code_lit ((env_var, env_ty), (param_var, param_ty), body) ->
        (* Code literals capture no variables from the surrounding context, so
          the body of the closure is synthesised in a new context that assumes
          only the parameter and the environment. *)
        let body_context =
          Var_map.empty
          |> Var_map.add env_var env_ty
          |> Var_map.add param_var param_ty
        in
        let body_ty = synth body_context body in
        Code_type (env_ty, param_ty, body_ty)
    | Tuple_lit tms ->
        Tuple_type (List.map (synth context) tms)
    | Tuple_proj (head, label) ->
        begin match synth context head with
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
        begin match synth context code with
        | Code_type (env_ty, param_ty, body_ty) ->
            check context env env_ty;
            Clos_type (param_ty, body_ty)
        | ty ->
            invalid_arg
              (Format.asprintf "expected code but found term of type %a" pp_ty ty)
        end
    | Clos_app (head, arg) ->
        begin match synth context head with
        | Clos_type (param_ty, body_ty) ->
            check context arg param_ty;
            body_ty
        | ty ->
            invalid_arg
              (Format.asprintf "expected closure but found term of type %a" pp_ty ty)
        end

end
