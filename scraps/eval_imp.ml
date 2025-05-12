(** A simple imperative language.

    https://softwarefoundations.cis.upenn.edu/lf-current/Imp.html
*)

[@@@warning "-unused-constructor"]
[@@@warning "-unused-value-declaration"]

type _ expr =
  | Id : string -> int expr
  | Int : int -> int expr
  | Add : int expr * int expr -> int expr
  | Sub : int expr * int expr -> int expr
  | Mul : int expr * int expr -> int expr
  | Bool : bool -> bool expr
  | Eq : int expr * int expr -> bool expr
  | Ne : int expr * int expr -> bool expr
  | Le : int expr * int expr -> bool expr
  | Gt : int expr * int expr -> bool expr
  | Not : bool expr -> bool expr
  | And : bool expr * bool expr -> bool expr

type command =
  | Assign of string * int expr
  | Seq of command * command
  | If of bool expr * command * command
  | While of bool expr * command

module Id_map = Map.Make (String)

type state = int Id_map.t

let rec eval_expr : type a. state -> a expr -> a =
  fun st e ->
    match e with
    | Id x -> Id_map.find x st
    | Int n -> n
    | Add (a1, a2) -> eval_expr st a1 + eval_expr st a2
    | Sub (a1, a2) -> eval_expr st a1 - eval_expr st a2
    | Mul (a1, a2) -> eval_expr st a1 * eval_expr st a2
    | Bool b -> b
    | Eq (a1, a2) -> eval_expr st a1 = eval_expr st a2
    | Ne (a1, a2) -> eval_expr st a1 <> eval_expr st a2
    | Le (a1, a2) -> eval_expr st a1 < eval_expr st a2
    | Gt (a1, a2) -> eval_expr st a1 > eval_expr st a2
    | Not b -> not (eval_expr st b)
    | And (b1, b2) -> eval_expr st b1 && eval_expr st b2

let rec eval_command (st : state) (c : command) : state =
  match c with
  | Assign (x, a) ->
      st |> Id_map.update x (fun _ -> Some (eval_expr st a))
  | Seq (c1, c2) ->
      let st' = eval_command st c1 in
      eval_command st' c2
  | If (b, c1, c2) ->
      begin match (eval_expr st b) with
      | true -> eval_command st c1
      | false -> eval_command st c2
      end
  | While (b, c) ->
      begin match (eval_expr st b) with
      | true -> eval_command st (Seq (c, While (b, c)))
      | false -> st
      end
