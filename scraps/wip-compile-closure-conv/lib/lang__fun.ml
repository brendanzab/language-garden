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


module Semantics = struct

  (** {1 Values} *)

  type vtm =
    | Bool_lit of bool
    | Int_lit of int
    | Fun_lit of string * ty * clos

  and clos = {
    env : vtm list;
    body : tm;
  }


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
    | Fun_lit (name, param_ty, body) ->
        (* We do a naive form of dynamic closure conversion here, just capturing
          the entire environment along with the code of the body. We’ll do a
          more thorough job in the compiler though. *)
        Fun_lit (name, param_ty, { env; body })
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
    | Fun_lit (_, _, { env; body }) -> eval (arg :: env) body
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
    | Fun_lit (_, param_ty, body) ->
        let body_ty = synth (param_ty :: context) body in
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


module Build = struct

  type 'a m = int -> 'a

  let var (level : int) : tm m =
    fun size ->
      Var (size - level - 1)

  let bind_var (type a) (cont : int -> a m) : a m =
    fun size ->
      cont size (size + 1)

  let scope (type a) (cont : tm m -> a m) : a m =
    bind_var (fun level -> cont (var level))


  (** {1 Types} *)

  let bool_ty : ty = Bool_type

  let int_ty : ty = Int_type

  let fun_ty (param_tys : ty list) (body_ty : ty) : ty =
    List.fold_right
      (fun param_ty acc -> Fun_type (param_ty, acc))
      param_tys
      body_ty


  (** {1 Terms} *)

  let let_ (name : string) (def_ty : ty) (def : tm m) (body : tm m -> tm m) : tm m =
    fun env ->
      Let (name, def_ty, def env, scope body env)

  let ( let* ) (name, ty, tm) body =
    let_ name ty tm body

  let bool_lit (b : bool) : tm m =
    fun _ -> Bool_lit b

  let int_lit (i : int) : tm m =
    fun _ -> Int_lit i

  let prim_app (prim : Prim.t) (args : tm m list) : tm m =
    fun env ->
      Prim_app (prim, List.map (fun arg -> arg env) args)

  let neg x : tm m =
    prim_app `Neg [x]

  let ( + ) x y : tm m =
    prim_app `Add [x; y]

  let ( - ) x y : tm m =
    prim_app `Sub [x; y]

  let ( * ) x y : tm m =
    prim_app `Mul [x; y]

  let fun_lit (name : string) (param_ty : ty) (body : tm m -> tm m) : tm m =
    fun env ->
      Fun_lit (name, param_ty, scope body env)

  let fun_app (head : tm m) (arg : tm m) : tm m =
    fun env ->
      Fun_app (head env, arg env)

  let fun_apps (head : tm m) (args : tm m list) : tm m =
    List.fold_left fun_app head args

end
