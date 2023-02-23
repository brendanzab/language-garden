(** {0 Simply typed lambda calculus}

    This is a simply typed lambda calculus with let expressions, booleans and
    integers. Variables occurances are represented with de Bruijn indices. This
    could be used as the basis of a core language for a more fully featured
    programming language.
*)


(** {1 Syntax} *)

type ty =
  | BoolType
  | IntType
  | FunType of ty * ty

type tm =
  | Var of int
  | Let of string * ty * tm * tm
  | BoolLit of bool
  | IntLit of int
  | PrimApp of Prim.t * tm list
  | FunLit of string * ty * tm
  | FunApp of tm * tm


(** Exception raised during parsing if a name was unbound *)
exception UnboundName of string


(** {1 Pretty printing} *)

let rec pp_ty fmt =
  function
  | FunType (param_ty, body_ty) ->
      Format.fprintf fmt "%a -> %a"
        pp_atomic_ty param_ty
        pp_ty body_ty
  | ty ->
      pp_atomic_ty fmt ty
and pp_atomic_ty fmt =
  function
  | BoolType -> Format.fprintf fmt "Bool"
  | IntType -> Format.fprintf fmt "Int"
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
  | FunLit (name, param_ty, body) ->
      Format.fprintf fmt "@[@[fun@ @%a@ =>@]@ %a@]"
        pp_param (name, param_ty)
        (pp_tm (name :: names)) body
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
  | FunApp (head, arg) ->
      Format.fprintf fmt "@[%a@ %a@]"
        (pp_app_tm names) head
        (pp_atomic_tm names) arg
  | PrimApp (`Neg, [arg]) ->
      Format.fprintf fmt "@[-%a@]"
        (pp_atomic_tm names) arg
  | tm ->
      pp_atomic_tm names fmt tm
and pp_atomic_tm names fmt = function
  | Var index ->
      Format.fprintf fmt "%s" (List.nth names index)
  | BoolLit true -> Format.fprintf fmt "true"
  | BoolLit false -> Format.fprintf fmt "false"
  | IntLit i -> Format.fprintf fmt "%i" i
  (* FIXME: Will loop forever on invalid primitive applications *)
  | tm -> Format.fprintf fmt "@[(%a)@]" (pp_tm names) tm


module Semantics = struct

  (** {1 Values} *)

  type vtm =
    | BoolLit of bool
    | IntLit of int
    | FunLit of string * ty * clos

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
    | BoolLit b -> BoolLit b
    | IntLit i -> IntLit i
    | PrimApp (prim, args) ->
        prim_app prim (List.map (eval env) args)
    | FunLit (name, param_ty, body) ->
        (* We do a naive form of dynamic closure conversion here, just capturing
          the entire environment along with the code of the body. We’ll do a
          more thorough job in the compiler though. *)
        FunLit (name, param_ty, { env; body })
    | FunApp (head, arg) ->
        let head = eval env head in
        let arg = eval env arg in
        fun_app head arg


  (** {1 Eliminators} *)

  and prim_app prim args =
    match prim, args with
    | `Neg, [IntLit t1] -> IntLit (-t1)
    | `Add, [IntLit t1; IntLit t2] -> IntLit (t1 + t2)
    | `Sub, [IntLit t1; IntLit t2] -> IntLit (t1 - t2)
    | `Mul, [IntLit t1; IntLit t2] -> IntLit (t1 * t2)
    | _, _ -> invalid_arg "invalid prim application"

  and fun_app head arg =
    match head with
    | FunLit (_, _, { env; body }) -> eval (arg :: env) body
    | _ -> invalid_arg "expected function"

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
    | FunLit (_, param_ty, body) ->
        let body_ty = synth (param_ty :: context) body in
        FunType (param_ty, body_ty)
    | FunApp (head, arg) ->
        begin match synth context head with
        | FunType (param_ty, body_ty) ->
            check context arg param_ty;
            body_ty
        | _ -> invalid_arg "expected function"
        end

end


module Build = struct

  type 'a m = int -> 'a

  let var (level : int) : tm m =
    fun size ->
      Var (size - level - 1)

  let bind_var (cont : int -> 'a m) : 'a m =
    fun size ->
      cont size (size + 1)

  let scope (cont : tm m -> 'a m) : 'a m =
    bind_var (fun level -> cont (var level))


  (** {1 Types} *)

  let bool_ty : ty = BoolType

  let int_ty : ty = IntType

  let fun_ty (param_tys : ty list) (body_ty : ty) : ty =
    List.fold_right
      (fun param_ty acc -> FunType (param_ty, acc))
      param_tys
      body_ty


  (** {1 Terms} *)

  let let_ (name : string) (def_ty : ty) (def : tm m) (body : tm m -> tm m) : tm m =
    fun env ->
      Let (name, def_ty, def env, scope body env)

  let ( let* ) (name, ty, tm) body =
    let_ name ty tm body

  let bool_lit (b : bool) : tm m =
    fun _ -> BoolLit b

  let int_lit (i : int) : tm m =
    fun _ -> IntLit i

  let prim_app (prim : Prim.t) (args : tm m list) : tm m =
    fun env ->
      PrimApp (prim, List.map (fun arg -> arg env) args)

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
      FunLit (name, param_ty, scope body env)

  let fun_app (head : tm m) (arg : tm m) : tm m =
    fun env ->
      FunApp (head env, arg env)

  let fun_apps (head : tm m) (args : tm m list) : tm m =
    List.fold_left fun_app head args

end
