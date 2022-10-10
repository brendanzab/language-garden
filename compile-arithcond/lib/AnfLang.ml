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
  | IfThenElse of atom * expr * expr

(** Atomic expressions *)
and atom =
  | Var of id
  | Int of int
  | Bool of bool


(** {1 Pretty printing} *)

let rec pp_expr fmt = function
  | Let (id, c, e) ->
      let name = Format.sprintf "e%i" id in
      begin match c with
      | IfThenElse _ ->
          Format.fprintf fmt "@[<v 2>@[let@ %s@ :=@]@ @[<v>%a@];@]@ %a" name
            pp_comp c
            pp_expr e
      | c ->
          Format.fprintf fmt "@[<2>@[let@ %s@ :=@]@ @[%a@];@]@ %a" name
            pp_comp c
            pp_expr e
      end
  | Comp c ->
      begin match c with
      | IfThenElse _ -> Format.fprintf fmt "@[<v>%a@]" pp_comp c
      | _ -> Format.fprintf fmt "@[%a@]" pp_comp c
      end
and pp_comp fmt = function
  | Atom a -> pp_atom fmt a
  | Neg a -> Format.fprintf fmt "neg %a" pp_atom a
  | Add (a1, a2) -> Format.fprintf fmt "add@ %a@ %a" pp_atom a1 pp_atom a2
  | Sub (a1, a2) -> Format.fprintf fmt "sub@ %a@ %a" pp_atom a1 pp_atom a2
  | Mul (a1, a2) -> Format.fprintf fmt "mul@ %a@ %a" pp_atom a1 pp_atom a2
  | Div (a1, a2) -> Format.fprintf fmt "div@ %a@ %a" pp_atom a1 pp_atom a2
  | Eq (a1, a2) -> Format.fprintf fmt "eq@ %a@ %a" pp_atom a1 pp_atom a2
  | IfThenElse (a, e1, e2) ->
      Format.fprintf fmt "@[<v 2>@[if@ %a@ then@]@ @[<v>%a@]@]@ @[<v 2>else@ @[<v>%a@]@]"
        pp_atom a
        pp_expr e1
        pp_expr e2
and pp_atom fmt = function
  | Var id -> Format.fprintf fmt "e%i" id
  | Int i -> Format.fprintf fmt "%d" i
  | Bool true -> Format.fprintf fmt "true"
  | Bool false -> Format.fprintf fmt "false"


(** Semantics of arithmetic expressions *)
module Semantics = struct

  module Env = Map.Make (Int)

  type value =
    | Int of int
    | Bool of bool

  let eval_atom ?(env = Env.empty) : atom -> value =
    function
    | Var id -> Env.find id env
    | Int i -> Int i
    | Bool b -> Bool b

  let eval_int ?(env = Env.empty) expr =
    match eval_atom ~env expr with
    | Int i ->  i
    | _ -> failwith "not an integer"

  let eval_bool ?(env = Env.empty) expr =
    match eval_atom ~env expr with
    | Bool b ->  b
    | _ -> failwith "not a boolean"

  let rec eval ?(env = Env.empty) : expr -> value =
    function
    | Let (id, c, e) -> eval ~env:(Env.add id (eval_comp ~env c) env) e
    | Comp c -> eval_comp ~env c
  and eval_comp ?(env = Env.empty) : comp -> value =
    function
    | Atom a -> eval_atom ~env a
    | Neg a -> Int (-(eval_int ~env a))
    | Add (a1, a2) -> Int (eval_int ~env a1 + eval_int ~env a2)
    | Sub (a1, a2) -> Int (eval_int ~env a1 - eval_int ~env a2)
    | Mul (a1, a2) -> Int (eval_int ~env a1 * eval_int ~env a2)
    | Div (a1, a2) -> Int (eval_int ~env a1 / eval_int ~env a2)
    | Eq (a1, a2) -> Bool (eval_atom ~env a1 = eval_atom ~env a2)
    | IfThenElse (a, e1, e2) ->
        if eval_bool ~env a then eval ~env e1 else eval ~env e2

  let quote : value -> expr =
    function
    | Int i -> Comp (Atom (Int i))
    | Bool b -> Comp (Atom (Bool b))

  let normalise e =
    quote (eval e)
end
