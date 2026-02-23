(** Evaluation for a first-order functional programming language with top-level items *)

module rec Item : sig

  type t =
    | Val of Expr.t
    | Fun of string list * Expr.t

end = Item

and Prim : sig

  type t =
    | Int_eq
    | Int_add
    | Int_sub
    | Int_mul

end = Prim

and Expr : sig

  type t =
    | Item of string * t list
    | Var of string
    | Let of string * t * t
    | Bool of bool
    | Bool_if of t * t * t
    | Int of int
    | Prim of Prim.t * t list

  module Value : sig

    type t =
      | Bool of bool
      | Int of int

  end

  val eval : (string * Item.t) list -> (string * Value.t) list -> t -> Value.t
  [@@warning "-unused-value-declaration"]

end = struct

  include Expr

  let rec eval (items : (string * Item.t) list) (env : (string * Value.t) list) (expr : t) : Value.t =
    match expr with
    | Item (name, args) ->
        begin match List.assoc name items with
        | Item.Val body -> assert (List.is_empty args); eval items env body
        | Item.Fun (names, body) ->
            let args = List.map2 (fun name arg -> name, eval items env arg) names args in
            eval items (List.append args env) body
        end
    | Var name -> List.assoc name env
    | Let (name, def, body) ->
        let def = eval items env def in
        eval items ((name, def) :: env) body
    | Bool bool -> Value.Bool bool
    | Bool_if (expr1, expr2, expr3) ->
        begin match eval items env expr1 with
        | Value.Bool true -> eval items env expr2
        | Value.Bool false -> eval items env expr3
        | _ -> failwith "eval"
        end
    | Int int -> Value.Int int
    | Prim (prim, args) ->
        begin match prim, List.map (eval items env) args with
        | Prim.Int_eq, [Value.Int int1; Value.Int int2] -> Value.Bool (Int.equal int1 int2)
        | Prim.Int_add, [Value.Int int1; Value.Int int2] -> Value.Int (Int.add int1 int2)
        | Prim.Int_sub, [Value.Int int1; Value.Int int2] -> Value.Int (Int.sub int1 int2)
        | Prim.Int_mul, [Value.Int int1; Value.Int int2] -> Value.Int (Int.mul int1 int2)
        | _, _ -> failwith "eval"
        end

end
