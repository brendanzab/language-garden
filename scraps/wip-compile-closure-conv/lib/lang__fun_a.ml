(** {0 Simply typed lambda calculus (alpha renamed)}

    This is a version of {!Lang.Fun} that uses unique names to represent
    variable binding structure, as opposed to de Bruijn indices.
*)

(** {1 Variables} *)

module Var = Fresh.Make ()

module Var_map = Map.Make (Var)
module Var_set = Set.Make (Var)


(** {1 Syntax} *)

type ty = Lang__fun.ty =
  | Bool_type
  | Int_type
  | Fun_type of ty * ty

type tm =
  | Var of Var.t
  | Let of Var.t * ty * tm * tm
  | Bool_lit of bool
  | Int_lit of int
  | Prim_app of Prim.t * tm list
  | Fun_lit of Var.t * ty * tm
  | Fun_app of tm * tm


(** Return the part of an environment that is used in a term *)
let rec fvs : tm -> Var_set.t =
  function
  | Var var -> Var_set.singleton var
  | Let (def_var, _, def, body) -> Var_set.union (fvs def) (Var_set.remove def_var (fvs body))
  | Bool_lit _ -> Var_set.empty
  | Int_lit _ -> Var_set.empty
  | Prim_app (_, args) -> List.fold_left Var_set.union Var_set.empty (List.map fvs args)
  | Fun_lit (param_var, _, body) -> Var_set.remove param_var (fvs body)
  | Fun_app (head, arg) -> Var_set.union (fvs head) (fvs arg)


(** {1 Pretty printing} *)

let pp_ty = Lang__fun.pp_ty

let pp_var (ppf : Format.formatter) (var : Var.t) =
  Format.fprintf ppf "%s%i"
    (Var.name var)
    (Var.to_int var)

let pp_name_ann (ppf : Format.formatter) (var, ty) =
  Format.fprintf ppf "@[<2>@[%a :@]@ %a@]"
    pp_var var
    pp_ty ty

let pp_param (ppf : Format.formatter) (var, ty) =
  Format.fprintf ppf "@[<2>(@[%a :@]@ %a)@]"
    pp_var var
    pp_ty ty

let rec pp_tm (ppf : Format.formatter) (tm : tm) =
  match tm with
  | Let _ as tm ->
      let rec go ppf tm =
        match tm with
        | Let (def_var, def_ty, def, body) ->
            Format.fprintf ppf "@[<hv 2>@[let %a@ :=@]@ @[%a;@]@]@ %a"
              pp_name_ann (def_var, def_ty)
              pp_tm def
              go body
        | tm -> Format.fprintf ppf "@[%a@]" pp_tm tm
      in
      go ppf tm
  | Fun_lit (name, param_ty, body) ->
      let rec go ppf tm =
        match tm with
        | Fun_lit (name, param_ty, body) ->
            Format.fprintf ppf "@ @[fun@ %a@ =>@]%a"
              pp_param (name, param_ty)
              go body
        | tm -> Format.fprintf ppf "@]@ @[%a@]@]" pp_tm tm
      in
      Format.fprintf ppf "@[<hv 2>@[<hv>@[fun@ %a@ =>@]%a"
        pp_param (name, param_ty)
        go body
  | tm ->
      pp_app_tm ppf tm
and pp_app_tm (ppf : Format.formatter) (tm : tm) =
  match tm with
  | Prim_app (head, args) ->
      Format.fprintf ppf "@[#%s@ %a@]"
        (Prim.to_string head)
        (Format.pp_print_list pp_atomic_tm ~pp_sep:Format.pp_print_space) args
  | Fun_app (head, arg) ->
      Format.fprintf ppf "@[%a@ %a@]"
        pp_app_tm head
        pp_atomic_tm arg
  | tm ->
      pp_atomic_tm ppf tm
and pp_atomic_tm (ppf : Format.formatter) (tm : tm) =
  match tm with
  | Var var -> pp_var ppf var
  | Bool_lit true -> Format.fprintf ppf "true"
  | Bool_lit false -> Format.fprintf ppf "false"
  | Int_lit i -> Format.fprintf ppf "%i" i
  | tm -> Format.fprintf ppf "@[(%a)@]" pp_tm tm


module Semantics = struct

  (** {1 Values} *)

  type vtm =
    | Bool_lit of bool
    | Int_lit of int
    | Fun_lit of Var.t * ty * clos

  and clos = {
    env : vtm Var_map.t;
    body : tm;
  }


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
    | Fun_lit (param_var, param_ty, body) ->
        (* We do a naive form of dynamic closure conversion here, just capturing
          the entire environment along with the code of the body. We’ll do a
          more thorough job in the compiler though. *)
        Fun_lit (param_var, param_ty, { env; body })
    | Fun_app (head, arg) ->
        let head = eval env head in
        let arg = eval env arg in
        fun_app head arg


  (** {1 Eliminators} *)

  and prim_app prim args =
    match prim, args with
    | `Neg, [Int_lit t1] -> Int_lit (-t1)
    | `Add, [Int_lit t1; Int_lit t2] -> Int_lit (t1 + t2)
    | `Sub, [Int_lit t1; Int_lit t2] -> Int_lit (t1 - t2)
    | `Mul, [Int_lit t1; Int_lit t2] -> Int_lit (t1 * t2)
    | _, _ -> invalid_arg "invalid prim application"

  and fun_app head arg =
    match head with
    | Fun_lit (param_var, _, { env; body }) ->
        eval (Var_map.add param_var arg env) body
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
    | Fun_lit (param_var, param_ty, body) ->
        let body_ty = synth (Var_map.add param_var param_ty context) body in
        Fun_type (param_ty, body_ty)
    | Fun_app (head, arg) ->
        begin match synth context head with
        | Fun_type (param_ty, body_ty) ->
            check context arg param_ty;
            body_ty
        | ty ->
            invalid_arg
              (Format.asprintf "expected function but found term of type %a" pp_ty ty)
        end

end
