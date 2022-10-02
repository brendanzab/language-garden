(** A tree based language of arithmetic expressions *)


(** Syntax of arithmetic expressions *)

type expr =
  | Int of int
  | Bool of bool
  | Neg of expr
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Eq of expr * expr
  | IfThenElse of expr * expr * expr


type ty =
  | TyInt
  | TyBool


let int n = Int n
let bool b = Bool b
let neg e = Neg e
let add e1 e2 = Add (e1, e2)
let sub e1 e2 = Sub (e1, e2)
let mul e1 e2 = Mul (e1, e2)
let div e1 e2 = Div (e1, e2)
let eq e1 e2 = Eq (e1, e2)
let if_then_else e1 e2 e3 = IfThenElse (e1, e2, e3)


(** Pretty printing *)

let rec pp_expr fmt expr =
  pp_if_expr fmt expr
and pp_if_expr fmt = function
  | IfThenElse (e1, e2, e3) ->
      Format.fprintf fmt "@[if@ %a@ then@]@ %a@ else@ %a"
        pp_eq_expr e1
        pp_eq_expr e2
        pp_if_expr e3
  | e -> pp_eq_expr fmt e
and pp_eq_expr fmt = function
  | Eq (e1, e2) -> Format.fprintf fmt "%a@ =@ %a" pp_add_expr e1 pp_eq_expr e2
  | e -> pp_add_expr fmt e
and pp_add_expr fmt = function
  | Add (e1, e2) -> Format.fprintf fmt "%a@ +@ %a" pp_mul_expr e1 pp_add_expr e2
  | Sub (e1, e2) -> Format.fprintf fmt "%a@ -@ %a" pp_mul_expr e1 pp_add_expr e2
  | e -> pp_mul_expr fmt e
and pp_mul_expr fmt = function
  | Mul (e1, e2) -> Format.fprintf fmt "%a@ *@ %a" pp_atomic_expr e1 pp_mul_expr e2
  | Div (e1, e2) -> Format.fprintf fmt "%a@ /@ %a" pp_atomic_expr e1 pp_mul_expr e2
  | e -> pp_atomic_expr fmt e
and pp_atomic_expr fmt = function
  | Int n -> Format.fprintf fmt "%d" n
  | Bool true -> Format.fprintf fmt "true"
  | Bool false -> Format.fprintf fmt "false"
  | Neg e -> Format.fprintf fmt "-%a" pp_atomic_expr e
  | e -> Format.fprintf fmt "@[<1>(%a)@]" pp_expr e

let pp_ty fmt = function
  | TyInt -> Format.fprintf fmt "Int"
  | TyBool -> Format.fprintf fmt "Bool"


(** Semantics of arithmetic expressions *)
module Semantics = struct

  type value =
    | Int of int
    | Bool of bool

  let rec eval : expr -> value =
    function
    | Int n -> Int n
    | Bool b -> Bool b
    | Neg e -> Int (-(eval_int e))
    | Add (e1, e2) -> Int (eval_int e1 + eval_int e2)
    | Sub (e1, e2) -> Int (eval_int e1 - eval_int e2)
    | Mul (e1, e2) -> Int (eval_int e1 * eval_int e2)
    | Div (e1, e2) -> Int (eval_int e1 / eval_int e2)
    | Eq (e1, e2) -> Bool (eval e1 = eval e2)
    | IfThenElse (e1, e2, e3) -> if eval_bool e1 then eval e2 else eval e3
  and eval_int expr =
    match eval expr with
    | Int n ->  n
    | _ -> failwith "not an integer"
  and eval_bool expr =
    match eval expr with
    | Bool b ->  b
    | _ -> failwith "not a boolean"


  let quote : value -> expr =
    function
    | Int i -> Int i
    | Bool b -> Bool b


  let normalise e =
    quote (eval e)

end


(** Type checking of arithmetic expressions *)
module Validation = struct

  (** An error raised if a type error was encountered *)
  exception Error of string


  (** Check that an expression is of a given type *)
  let rec check (e : expr) (t : ty) : unit =
    match e, t with
    | IfThenElse (e1, e2, e3), t ->
        check e1 TyBool; check e2 t; check e3 t
    | e, t ->
      let t' = synth e in
      if t != t' then
        let message = Format.asprintf "mismatched types: expected %a, found %a" pp_ty t pp_ty t' in
        raise (Error message)

  (** Synthesise the type of an expression *)
  and synth : expr -> ty =
    function
    | Int _ -> TyInt
    | Bool _ -> TyBool
    | Neg e -> check e TyInt; TyInt
    | Add (e1, e2) -> check e1 TyInt; check e2 TyInt; TyInt
    | Sub (e1, e2) -> check e1 TyInt; check e2 TyInt; TyInt
    | Mul (e1, e2) -> check e1 TyInt; check e2 TyInt; TyInt
    | Div (e1, e2) -> check e1 TyInt; check e2 TyInt; TyInt
    | Eq (e1, e2) ->
        (* TODO: Unify e1 and e2 *)
        let t1 = synth e1; in check e2 t1; TyBool
    | IfThenElse (e1, e2, e3) ->
        (* TODO: Unify e2 and e3 *)
        check e1 TyBool; let t2 = synth e2; in check e3 t2; t2

end
