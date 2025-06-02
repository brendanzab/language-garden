(** {0 Arithmetic expressions}

    Arithmetic expressions as a tree of nested subexpressions.
*)


(** {1 Syntax of arithmetic expressions} *)

(** These names are used as hints for pretty printing binders and variables. *)
type name = string

(** De Bruijn index, counting variables from the most recently bound to the
    least recently bound. *)
type index = int

(** Expressions *)
type expr =
  | Var of index
  | Let of string * expr * expr
  | Int of int
  | Bool of bool
  | Neg of expr
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Eq of expr * expr
  | If_then_else of expr * expr * expr

(** Types *)
type ty =
  | Ty_int
  | Ty_bool

(** {2 Constructor functions} *)

let var n = Var n
let let_ x e1 e2 = Let (x, e1, e2)
let int i = Int i
let bool b = Bool b
let neg e = Neg e
let add e1 e2 = Add (e1, e2)
let sub e1 e2 = Sub (e1, e2)
let mul e1 e2 = Mul (e1, e2)
let div e1 e2 = Div (e1, e2)
let eq e1 e2 = Eq (e1, e2)
let if_then_else e1 e2 e3 = If_then_else (e1, e2, e3)

(** Exception raised during parsing if a name was unbound *)
exception Unbound_name of (Lexing.position * Lexing.position) * name


(** {1 Pretty printing} *)

let rec pp_expr names ppf expr =
  pp_let_expr names ppf expr
and  pp_let_expr names ppf = function
  | Let (n, e1, e2) ->
      let pp_def names ppf (n, e1) =
        Format.fprintf ppf "@[let@ %s@ :=@]@ @[%a@];" n (pp_expr names) e1
      and pp_lets names ppf = function
        | Let (_, _, _) as e -> pp_expr names ppf e
        | e -> Format.fprintf ppf "@[%a@]" (pp_expr names) e
      in
      Format.fprintf ppf "@[<2>%a@]@ %a"
        (pp_def names) (n, e1)
        (pp_lets (n :: names)) e2
  | If_then_else (e1, e2, e3) ->
      Format.fprintf ppf "@[<hv>@[if@ %a@ then@]@;<1 2>@[%a@]@ else@;<1 2>@[%a@]@]"
        (pp_eq_expr names) e1
        (pp_eq_expr names) e2
        (pp_expr names) e3
  | e -> pp_eq_expr names ppf e
(* TODO: Let expressions *)
and pp_eq_expr names ppf = function
  | Eq (e1, e2) -> Format.fprintf ppf "%a@ =@ %a" (pp_add_expr names) e1 (pp_eq_expr names) e2
  | e -> pp_add_expr names ppf e
and pp_add_expr names ppf = function
  | Add (e1, e2) -> Format.fprintf ppf "%a@ +@ %a" (pp_mul_expr names) e1 (pp_add_expr names) e2
  | Sub (e1, e2) -> Format.fprintf ppf "%a@ -@ %a" (pp_mul_expr names) e1 (pp_add_expr names) e2
  | e -> pp_mul_expr names ppf e
and pp_mul_expr names ppf = function
  | Mul (e1, e2) -> Format.fprintf ppf "%a@ *@ %a" (pp_atomic_expr names) e1 (pp_mul_expr names) e2
  | Div (e1, e2) -> Format.fprintf ppf "%a@ /@ %a" (pp_atomic_expr names) e1 (pp_mul_expr names) e2
  | e -> pp_atomic_expr names ppf e
and pp_atomic_expr names ppf = function
  | Var n -> Format.pp_print_string ppf (List.nth names n)
  | Int i -> Format.fprintf ppf "%d" i
  | Bool true -> Format.fprintf ppf "true"
  | Bool false -> Format.fprintf ppf "false"
  | Neg e -> Format.fprintf ppf "-%a" (pp_atomic_expr names) e
  | e -> Format.fprintf ppf "@[<1>(%a)@]" (pp_expr names) e

let pp_ty ppf = function
  | Ty_int -> Format.fprintf ppf "Int"
  | Ty_bool -> Format.fprintf ppf "Bool"


(** Semantics of arithmetic expressions *)
module Semantics = struct

  type value =
    | Int of int
    | Bool of bool

  type env = value list

  let rec eval env : expr -> value =
    function
    | Var n -> List.nth env n
    | Let (_, e1, e2) -> eval (eval env e1 :: env) e2
    | Int i -> Int i
    | Bool b -> Bool b
    | Neg e -> Int (-(eval_int env e))
    | Add (e1, e2) -> Int (eval_int env e1 + eval_int env e2)
    | Sub (e1, e2) -> Int (eval_int env e1 - eval_int env e2)
    | Mul (e1, e2) -> Int (eval_int env e1 * eval_int env e2)
    | Div (e1, e2) -> Int (eval_int env e1 / eval_int env e2)
    | Eq (e1, e2) -> Bool (eval env e1 = eval env e2)
    | If_then_else (e1, e2, e3) -> if eval_bool env e1 then eval env e2 else eval env e3
  and eval_int env expr =
    match eval env expr with
    | Int n ->  n
    | _ -> failwith "not an integer"
  and eval_bool env expr =
    match eval env expr with
    | Bool b ->  b
    | _ -> failwith "not a boolean"


  let quote : value -> expr =
    function
    | Int i -> Int i
    | Bool b -> Bool b


  let normalise env e =
    quote (eval env e)

end


(** Type checking of arithmetic expressions *)
module Validation = struct

  (** An error raised if a type error was encountered *)
  exception Error of string

  type context = ty list


  (** Check that an expression is of a given type *)
  let rec check ctx (e : expr) (t : ty) : unit =
    match e, t with
    | Let (_, e1, e2), t ->
        let t' = synth ctx e1 in check (t' :: ctx) e2 t
    | If_then_else (e1, e2, e3), t ->
        check ctx e1 Ty_bool; check ctx e2 t; check ctx e3 t
    | e, t ->
      let t' = synth ctx e in
      if t != t' then
        let message = Format.asprintf "mismatched types: expected %a, found %a" pp_ty t pp_ty t' in
        raise (Error message)

  (** Synthesise the type of an expression *)
  and synth ctx : expr -> ty =
    function
    | Var n -> List.nth ctx n
    | Let (_, e1, e2) ->
        let t = synth ctx e1 in synth (t :: ctx) e2
    | Int _ -> Ty_int
    | Bool _ -> Ty_bool
    | Neg e -> check ctx e Ty_int; Ty_int
    | Add (e1, e2) -> check ctx e1 Ty_int; check ctx e2 Ty_int; Ty_int
    | Sub (e1, e2) -> check ctx e1 Ty_int; check ctx e2 Ty_int; Ty_int
    | Mul (e1, e2) -> check ctx e1 Ty_int; check ctx e2 Ty_int; Ty_int
    | Div (e1, e2) -> check ctx e1 Ty_int; check ctx e2 Ty_int; Ty_int
    | Eq (e1, e2) ->
        (* TODO: Unify e1 and e2 *)
        let t1 = synth ctx e1 in check ctx e2 t1; Ty_bool
    | If_then_else (e1, e2, e3) ->
        (* TODO: Unify e2 and e3 *)
        check ctx e1 Ty_bool; let t2 = synth ctx e2; in check ctx e3 t2; t2

end
