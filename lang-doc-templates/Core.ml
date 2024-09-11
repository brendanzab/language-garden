type ty =
  | FunTy of ty * ty
  | TextTy
  | ListTy of ty
  | BoolTy
  | IntTy
  (* | NodeTy (* TODO: More precise node types *) *)

type tm =
  | Var of string
  | Let of string * ty * tm * tm
  | FunLit of string * ty * tm
  | FunApp of tm * tm
  | TextLit of string
  | TextConcat of tm * tm
  | ListNil
  | ListCons of tm * tm
  (* | ListElim of tm * tm * (string * string * tm) *)
  | BoolLit of bool
  | BoolElim of tm * tm * tm
  | IntLit of int
  | IntAdd of tm * tm
  (* | NodeLit of (string * tm) list * tm list *)
  (* | NodeElim of ... *)

module Semantics = struct

  (** {2 Semantic domain} *)

  type vtm =
    | FunLit of string * ty * (vtm -> vtm)
    | ListNil
    | ListCons of vtm * vtm
    | TextLit of string
    | BoolLit of bool
    | IntLit of int
    (* | NodeLit of (string * vtm) list * vtm list *)

  type env = (string * vtm) list

  (** {2 Evaluation} *)

  let rec eval (locals : env) (tm : tm) : vtm =
    match tm with
    | Var name ->
        List.assoc name locals
    | Let (name, _, def, body) ->
        eval ((name, eval locals def) :: locals) body
    | FunLit (name, ty, body) ->
        FunLit (name, ty, fun v -> eval ((name, v) :: locals) body)
    | FunApp (head, arg) ->
        begin match eval locals head with
        | FunLit (_, _, f) -> f (eval locals arg)
        | _ -> invalid_arg "expected function literal"
        end
    | ListNil -> ListNil
    | ListCons (tm, tms) ->
        ListCons (eval locals tm, eval locals tms)
    | TextLit s ->
        TextLit s
    | TextConcat (tm1, tm2) ->
        begin match eval locals tm1, eval locals tm2 with
        | TextLit s1, TextLit s2 -> TextLit (s1 ^ s2)
        | _ -> invalid_arg "expected text literal"
        end
    | BoolLit b ->
        BoolLit b
    | BoolElim (head, tm1, tm2) ->
        begin match eval locals head with
        | BoolLit true -> eval locals tm1
        | BoolLit false -> eval locals tm2
        | _ -> invalid_arg "expected boolean literal"
        end
    | IntLit n ->
        IntLit n
    | IntAdd (tm1, tm2) ->
        begin match eval locals tm1, eval locals tm2 with
        | IntLit i1, IntLit i2 -> IntLit (i1 + i2)
        | _ -> invalid_arg "expected int literal"
        end

end
