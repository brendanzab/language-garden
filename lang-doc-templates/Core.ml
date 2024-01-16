type ty =
  | Fun of ty * ty
  | Text
  | Bool
  | Int

type tm =
  | Var of string
  | Let of string * ty * tm * tm
  | FunLit of string * ty * tm
  | FunApp of tm * tm
  | TextLit of string
  | TextConcat of tm * tm
  | BoolLit of bool
  | BoolElim of tm * tm * tm
  | IntLit of int
  | IntAdd of tm * tm
  (* | NodeLit of (string * tm) list * tm list *)

module Semantics = struct

  type vtm =
    | FunLit of string * (vtm -> vtm)
    | TextLit of string
    | BoolLit of bool
    | IntLit of int
    (* | NodeLit of (string * vtm) list * vtm list *)

  type env = (string * vtm) list

  let fun_app (head : vtm) (arg : vtm) : vtm =
    match head with
    | FunLit (_, f) -> f arg
    | _ -> invalid_arg "expected function literal"

  let text_concat (vtm1 : vtm) (vtm2 : vtm) : vtm =
    match vtm1, vtm2 with
    | TextLit s1, TextLit s2 -> TextLit (s1 ^ s2)
    | _ -> invalid_arg "expected text literal"

  let bool_elim (head : vtm) (vtm1 : vtm Lazy.t) (vtm2 : vtm Lazy.t) : vtm =
    match head with
    | BoolLit true -> Lazy.force vtm1
    | BoolLit false -> Lazy.force vtm2
    | _ -> invalid_arg "expected boolean literal"

  let int_add (vtm1 : vtm) (vtm2 : vtm) : vtm =
    match vtm1, vtm2 with
    | IntLit i1, IntLit i2 -> IntLit (i1 + i2)
    | _ -> invalid_arg "expected int literal"

  let rec eval (locals : env) (tm : tm) : vtm =
    match tm with
    | Var name ->
        List.assoc name locals
    | Let (name, _, def, body) ->
        eval ((name, eval locals def) :: locals) body
    | FunLit (name, _, body) ->
        FunLit (name, fun v -> eval ((name, v) :: locals) body)
    | FunApp (head, arg) ->
        fun_app (eval locals head) (eval locals arg)
    | TextLit s ->
        TextLit s
    | TextConcat (tm1, tm2) ->
        text_concat (eval locals tm1) (eval locals tm2)
    | BoolLit b ->
        BoolLit b
    | BoolElim (head, tm1, tm2) ->
        bool_elim
          (eval locals head)
          (lazy (eval locals tm1))
          (lazy (eval locals tm2))
    | IntLit n ->
        IntLit n
    | IntAdd (tm1, tm2) ->
        int_add (eval locals tm1) (eval locals tm2)

end
