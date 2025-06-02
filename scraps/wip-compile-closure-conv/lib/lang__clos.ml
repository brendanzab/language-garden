(** {0 Closure converted functional language}

    Unlike {!Fun_lang}, this language makes an explicit distinction between the
    ‘code’ of closures, and the captured variables they close over.
*)


(** {1 Syntax} *)

type ty =
  | Bool_type                             (** [ Bool ] *)
  | Int_type                              (** [ Int ] *)
  | Code_type of ty * ty * ty             (** [ Code(t_env, t1, t2) ] *)
  | Tuple_type of ty list                 (** [ (t1, ... tn) ] *)
  | Clos_type of ty * ty                  (** [ t1 -> t2 ] *)

type tm =
  | Var of int
  | Let of string * ty * tm * tm
  | Bool_lit of bool
  | Int_lit of int
  | Prim_app of Prim.t * tm list
  | Code_lit of ty * (string * ty) * tm   (** [ fun env x => e ] *)
  | Tuple_lit of tm list                  (** [ (e1, ..., en) ] *)
  | Tuple_proj of tm * int                (** [ e.n ] *)
  | Clos_lit of tm * tm                   (** [ clos(e1, e2) ] *)
  | Clos_app of tm * tm                   (** [ e1 e2 ] *)


(** {1 Pretty printing} *)

let pp_comma_sep (ppf : Format.formatter) () =
  Format.fprintf ppf ",@ "

let pp_tuple_elems (type a) (pp_elem : Format.formatter -> a -> unit) (ppf : Format.formatter) (elems : a list) =
  match elems with
  | [elem] -> Format.fprintf ppf "%a," pp_elem elem
  | elems ->
      Format.fprintf ppf "%a"
        (Format.pp_print_list pp_elem ~pp_sep:pp_comma_sep) elems

let rec pp_ty (ppf : Format.formatter) (ty : ty) =
  match ty with
  | Clos_type (param_ty, body_ty) ->
      Format.fprintf ppf "%a -> %a"
        pp_atomic_ty param_ty
        pp_ty body_ty
  | ty ->
      pp_atomic_ty ppf ty
and pp_atomic_ty (ppf : Format.formatter) (ty : ty) =
  match ty with
  | Bool_type -> Format.fprintf ppf "Bool"
  | Int_type -> Format.fprintf ppf "Int"
  | Code_type (env_ty, param_ty, body_ty) ->
      Format.fprintf ppf "@[Code(%a,@ %a,@ %a)@]"
        pp_ty env_ty
        pp_ty param_ty
        pp_ty body_ty
  | Tuple_type tys ->
      Format.fprintf ppf "@[(%a)@]" (pp_tuple_elems pp_ty) tys
  | ty ->
      Format.fprintf ppf "@[(%a)@]" pp_ty ty

let pp_name_ann (ppf : Format.formatter) (name, ty) =
  Format.fprintf ppf "@[<2>@[%s :@]@ %a@]" name pp_ty ty

let pp_param (ppf : Format.formatter) (name, ty) =
  Format.fprintf ppf "@[<2>(@[%s :@]@ %a)@]" name pp_ty ty

let rec pp_tm (names : string list) (ppf : Format.formatter) (tm : tm) =
  match tm with
  | Let _ as tm ->
      let rec go names ppf tm =
        match tm with
        | Let (name, def_ty, def, body) ->
            Format.fprintf ppf "@[<hv 2>@[let %a@ :=@]@ @[%a;@]@]@ %a"
              pp_name_ann (name, def_ty)
              (pp_tm names) def
              (go (name :: names)) body
        | tm -> Format.fprintf ppf "@[%a@]" (pp_tm names) tm
      in
      go names ppf tm
  | Code_lit (env_ty, (name, param_ty), body) ->
      Format.fprintf ppf "@[<hv 2>@[fun@ %a@ %a@ =>@]@ %a@]"
        pp_param ("env", env_ty)
        pp_param (name, param_ty)
        (pp_tm [name; "env"]) body
  | tm ->
      pp_app_tm names ppf tm
and pp_app_tm (names : string list) (ppf : Format.formatter) (tm : tm) =
  match tm with
  | Prim_app (head, args) ->
      Format.fprintf ppf "@[#%s@ %a@]"
        (Prim.to_string head)
        (Format.pp_print_list (pp_proj_tm names) ~pp_sep:Format.pp_print_space) args
  | Clos_app (head, arg) ->
      Format.fprintf ppf "@[%a@ %a@]"
        (pp_app_tm names) head
        (pp_proj_tm names) arg
  | tm ->
      pp_proj_tm names ppf tm
and pp_proj_tm (names : string list) (ppf : Format.formatter) (tm : tm) =
  match tm with
  | Tuple_proj (head, label) ->
      Format.fprintf ppf "@[%a.%i@]"
        (pp_proj_tm names) head
        label
  | tm ->
      pp_atomic_tm names ppf tm
and pp_atomic_tm (names : string list) (ppf : Format.formatter) (tm : tm) =
  match tm with
  | Var index ->
      Format.fprintf ppf "%s" (List.nth names index)
  | Bool_lit true -> Format.fprintf ppf "true"
  | Bool_lit false -> Format.fprintf ppf "false"
  | Int_lit i -> Format.fprintf ppf "%i" i
  | Clos_lit (code, env) ->
      Format.fprintf ppf "@[<hv>clos(@;<0 2>%a,@;<1 2>%a@;<0 0>)@]"
        (pp_tm names) code
        (pp_tm names) env
  | Tuple_lit tms ->
      Format.fprintf ppf "@[(%a)@]" (pp_tuple_elems (pp_tm names)) tms
  | tm -> Format.fprintf ppf "@[(%a)@]" (pp_tm names) tm


module Semantics = struct

  (** {1 Values} *)

  type vtm =
    | Bool_lit of bool
    | Int_lit of int
    | Code_lit of ty * (string * ty) * tm   (** [ fun env x => e ] *)
    | Tuple_lit of vtm list                 (** [ (v1, ..., v2) ] *)
    | Clos_lit of vtm * vtm                 (** [ clos(v1, v2)  ] *)


  (** {1 Evaluation} *)

  let rec eval env : tm -> vtm =
    function
    | Var index -> List.nth env index
    | Let (_, _, def, body) ->
        let def = eval env def in
        eval (def :: env) body
    | Bool_lit b -> Bool_lit b
    | Int_lit i -> Int_lit i
    | Prim_app (prim, args) ->
        prim_app prim (List.map (eval env) args)
    | Code_lit (env_ty, (name, param_ty), body) ->
        Code_lit (env_ty, (name, param_ty), body)
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
    | Clos_lit (Code_lit (_, _, body), env) -> eval [arg; env] body
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
    | Var index ->
        begin match List.nth_opt context index with
        | Some ty -> ty
        | None -> invalid_arg "unbound variable"
        end
    | Let (_, _, def, body) ->
        let def_ty = synth context def in
        synth (def_ty :: context) body
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
    | Code_lit (env_ty, (_, param_ty), body) ->
        (* Code literals capture no variables from the surrounding context, so
          the body of the closure is synthesised in a new context that assumes
          only the parameter and the environment. *)
        let body_ty = synth [param_ty; env_ty] body in
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
