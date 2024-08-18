(** {0 Arithmetic expressions in A-Normal Form}

    {{:https://en.wikipedia.org/wiki/A-normal_form} A-Normal Form} is an
    intermediate representation that binds the result of each computation to an
    intermediate definition. *)


(** {1 Syntax} *)

(** A uniquely assigned variable id *)
type id = int

(** Expressions in A-Normal Form *)
type expr =
  | Let of id * comp * expr
  | LetJoin of id * id * expr * expr
  | JoinApp of id * atom
  | IfThenElse of atom * expr * expr
  | Comp of comp

(** Computation expressions *)
and comp =
  | Atom of atom
  | Neg of atom
  | Add of atom * atom
  | Sub of atom * atom
  | Mul of atom * atom
  | Div of atom * atom
  | Eq of atom * atom

(** Atomic expressions *)
and atom =
  | Var of id
  | Int of int
  | Bool of bool

(** {2 Constructor functions} *)

let comp c = Comp c
let atom a = Atom a


(** {1 Pretty printing} *)

let rec pp_expr fmt = function
  | Let (n, c, e) ->
      let n = Format.sprintf "e%i" n in
      Format.fprintf fmt "@[<2>@[let@ %s@ :=@]@ %a;@]@ %a" n
        pp_comp c
        pp_expr e
  | LetJoin (n, pn, e1, e2) ->
      let n = Format.sprintf "j%i" n in
      let pn = Format.sprintf "e%i" pn in
      Format.fprintf fmt "@[<2>@[let join@ %s@ %s@ :=@]@ %a;@]@ %a" n pn
        pp_expr e1
        pp_expr e2
  | JoinApp (n, a) ->
      Format.fprintf fmt "@[jump@ j%i@ %a@]" n pp_atom a
  | IfThenElse (a, e1, e2) ->
      Format.fprintf fmt "@[<v 2>@[if@ %a@ then@]@ @[<v>%a@]@]@ @[<v 2>else@ @[<v>%a@]@]"
        pp_atom a
        pp_expr e1
        pp_expr e2
  | Comp c ->
      Format.fprintf fmt "@[%a@]" pp_comp c
and pp_comp fmt = function
  | Atom a -> pp_atom fmt a
  | Neg a -> Format.fprintf fmt "neg %a" pp_atom a
  | Add (a1, a2) -> Format.fprintf fmt "add@ %a@ %a" pp_atom a1 pp_atom a2
  | Sub (a1, a2) -> Format.fprintf fmt "sub@ %a@ %a" pp_atom a1 pp_atom a2
  | Mul (a1, a2) -> Format.fprintf fmt "mul@ %a@ %a" pp_atom a1 pp_atom a2
  | Div (a1, a2) -> Format.fprintf fmt "div@ %a@ %a" pp_atom a1 pp_atom a2
  | Eq (a1, a2) -> Format.fprintf fmt "eq@ %a@ %a" pp_atom a1 pp_atom a2
and pp_atom fmt = function
  | Var n -> Format.fprintf fmt "e%i" n
  | Int i -> Format.pp_print_int fmt i
  | Bool true -> Format.fprintf fmt "true"
  | Bool false -> Format.fprintf fmt "false"


(** Semantics of arithmetic expressions *)
module Semantics = struct

  module Env = Map.Make (Int)

  type value =
    | Int of int
    | Bool of bool

  type defn =
    | Value of value
    | Join of id * expr

  let eval_atom env : atom -> value =
    function
    | Var id ->
        begin match Env.find id env with
        | Value e -> e
        | _ -> failwith "not a value"
        end
    | Int i -> Int i
    | Bool b -> Bool b

  let eval_int env expr =
    match eval_atom env expr with
    | Int i ->  i
    | _ -> failwith "not an integer"

  let eval_bool env expr =
    match eval_atom env expr with
    | Bool b ->  b
    | _ -> failwith "not a boolean"

  let rec eval env : expr -> value =
    function
    | Let (n, c, e) -> eval (Env.add n (Value (eval_comp env c)) env) e
    | LetJoin (n, pn, e1, e2) -> eval (Env.add n (Join (pn, e1)) env) e2
    | JoinApp (n, a) ->
        begin match Env.find n env with
        | Join (pn, e) -> eval (Env.add pn (Value (eval_atom env a)) env) e
        | _ -> failwith "not a value"
        end
    | IfThenElse (a, e1, e2) ->
        if eval_bool env a then eval env e1 else eval env e2
    | Comp c -> eval_comp env c
  and eval_comp env : comp -> value =
    function
    | Atom a -> eval_atom env a
    | Neg a -> Int (-(eval_int env a))
    | Add (a1, a2) -> Int (eval_int env a1 + eval_int env a2)
    | Sub (a1, a2) -> Int (eval_int env a1 - eval_int env a2)
    | Mul (a1, a2) -> Int (eval_int env a1 * eval_int env a2)
    | Div (a1, a2) -> Int (eval_int env a1 / eval_int env a2)
    | Eq (a1, a2) -> Bool (eval_atom env a1 = eval_atom env a2)

  let quote : value -> expr =
    function
    | Int i -> Comp (Atom (Int i))
    | Bool b -> Comp (Atom (Bool b))

  let normalise env e =
    quote (eval env e)

end
