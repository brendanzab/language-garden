(** {0 Simply typed lambda calculus}

    This is a simply typed lambda calculus with let expressions, booleans and
    integers. Variables occurrences are represented with de Bruijn indices. This
    could be used as the basis of a core language for a more fully featured
    programming language.
*)


(** {1 Syntax} *)

type ty =
  | Bool_type
  | Int_type
  | Fun_type of ty * ty

type tm =
  | Var of int
  | Let of string * ty * tm * tm
  | Bool_lit of bool
  | Int_lit of int
  | Prim_app of Prim.t * tm list
  | Fun_lit of string * ty * tm
  | Fun_app of tm * tm


(** Exception raised during parsing if a name was unbound *)
exception Unbound_name of string


(** Compute the part of an environment that is used in a term. Returns a bitmask
    over the environment (indexed by level), raising an exception if a variable
    is found that is greater than or equal to [size]. *)
let fvs size tm : bool array =
  (* Traverse a term, recording any free variables in the supplied bitmask. *)
  let rec go mask offset =
    function
    | Var index when index < offset -> ()
    | Var index -> Array.set mask (size + offset - index - 1) true
    | Let (_, _, def, body) -> go mask offset def; go mask (offset + 1) body
    | Bool_lit _ -> ()
    | Int_lit _ -> ()
    | Prim_app (_, args) -> List.iter (go mask offset) args
    | Fun_lit (_, _, body) -> go mask (offset + 1) body
    | Fun_app (head, arg) -> go mask offset head; go mask offset arg
  in

  (* Initialise an array to serve as a bitmask over the environment *)
  let mask = Array.make size false in
  (* Update the bitmask with the free variables *)
  go mask 0 tm;

  mask


(** {1 Pretty printing} *)

let rec pp_ty (ppf : Format.formatter) (ty : ty) =
  match ty with
  | Fun_type (param_ty, body_ty) ->
      Format.fprintf ppf "%a -> %a"
        pp_atomic_ty param_ty
        pp_ty body_ty
  | ty ->
      pp_atomic_ty ppf ty
and pp_atomic_ty (ppf : Format.formatter) (ty : ty) =
  match ty with
  | Bool_type -> Format.fprintf ppf "Bool"
  | Int_type -> Format.fprintf ppf "Int"
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
  | Fun_lit (name, param_ty, body) ->
      let rec go names ppf tm =
        match tm with
        | Fun_lit (name, param_ty, body) ->
            Format.fprintf ppf "@ @[fun@ %a@ =>@]%a"
              pp_param (name, param_ty)
              (go (name :: names)) body
        | tm -> Format.fprintf ppf "@]@ @[%a@]@]" (pp_tm names) tm
      in
      Format.fprintf ppf "@[<hv 2>@[<hv>@[fun@ %a@ =>@]%a"
        pp_param (name, param_ty)
        (go (name :: names)) body
  | tm ->
      pp_app_tm names ppf tm
and pp_app_tm (names : string list) (ppf : Format.formatter) (tm : tm) =
  match tm with
  | Prim_app (head, args) ->
      Format.fprintf ppf "@[#%s@ %a@]"
        (Prim.to_string head)
        (Format.pp_print_list (pp_atomic_tm names) ~pp_sep:Format.pp_print_space) args
  | Fun_app (head, arg) ->
      Format.fprintf ppf "@[%a@ %a@]"
        (pp_app_tm names) head
        (pp_atomic_tm names) arg
  | tm ->
      pp_atomic_tm names ppf tm
and pp_atomic_tm (names : string list) (ppf : Format.formatter) (tm : tm) =
  match tm with
  | Var index ->
      Format.fprintf ppf "%s" (List.nth names index)
  | Bool_lit true -> Format.fprintf ppf "true"
  | Bool_lit false -> Format.fprintf ppf "false"
  | Int_lit i -> Format.fprintf ppf "%i" i
  | tm -> Format.fprintf ppf "@[(%a)@]" (pp_tm names) tm
