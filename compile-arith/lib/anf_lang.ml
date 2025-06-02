(** {0 Arithmetic expressions in A-Normal Form}

    {{: https://en.wikipedia.org/wiki/A-normal_form} A-Normal Form} is an
    intermediate representation that binds the result of each computation to an
    intermediate definition.
*)


(** {1 Syntax} *)

(** A uniquely assigned variable id *)
type id = int

(** Expressions in A-Normal Form *)
type expr =
  | Let of id * comp * expr
  | Comp of comp

(** Computation expressions *)
and comp =
  | Atom of atom
  | Neg of atom
  | Add of atom * atom
  | Sub of atom * atom
  | Mul of atom * atom
  | Div of atom * atom

(** Atomic expressions *)
and atom =
  | Var of id
  | Int of int

(** {2 Constructor functions} *)

let comp c = Comp c
let atom a = Atom a


(** {1 Pretty printing} *)

let rec pp_expr ppf = function
  | Let (n, c, e) ->
      let name = Format.sprintf "e%i" n in
      Format.fprintf ppf "@[<2>@[let@ %s@ :=@]@ %a;@]@ %a" name
        pp_comp c
        pp_expr e
  | Comp c -> Format.fprintf ppf "@[%a@]" pp_comp c
and pp_comp ppf = function
  | Atom a -> pp_atom ppf a
  | Neg a -> Format.fprintf ppf "neg %a" pp_atom a
  | Add (a1, a2) -> Format.fprintf ppf "add@ %a@ %a" pp_atom a1 pp_atom a2
  | Sub (a1, a2) -> Format.fprintf ppf "sub@ %a@ %a" pp_atom a1 pp_atom a2
  | Mul (a1, a2) -> Format.fprintf ppf "mul@ %a@ %a" pp_atom a1 pp_atom a2
  | Div (a1, a2) -> Format.fprintf ppf "div@ %a@ %a" pp_atom a1 pp_atom a2
and pp_atom ppf = function
  | Var n -> Format.fprintf ppf "e%i" n
  | Int i -> Format.fprintf ppf "%d" i


(** Semantics of arithmetic expressions *)
module Semantics = struct

  module Env = Map.Make (Int)

  type value = int

  let eval_atom env : atom -> value =
    function
    | Var x -> Env.find x env
    | Int i -> i

  let eval_comp env : comp -> value =
    function
    | Atom a -> eval_atom env a
    | Neg a -> -(eval_atom env a)
    | Add (a1, a2) -> eval_atom env a1 + eval_atom env a2
    | Sub (a1, a2) -> eval_atom env a1 - eval_atom env a2
    | Mul (a1, a2) -> eval_atom env a1 * eval_atom env a2
    | Div (a1, a2) -> eval_atom env a1 / eval_atom env a2

  let rec eval env : expr -> value =
    function
    | Let (n, c, e) -> eval (Env.add n (eval_comp env c) env) e
    | Comp c -> eval_comp env c

end
