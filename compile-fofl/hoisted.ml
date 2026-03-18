(** An intermediate language where nested let expressions and conditionals are
    hoisted to the top of expressions.

    This language is very close to A-Normal form, but computations can still
    nested inside other expressions. This reduces the number of intermediate
    bindings we need when translating to web assembly.
*)

module Item_name = Core.Item_name
module Item_map = Core.Item_map

module Local_name = Name.Fresh.Make ()
module Local_map = Map.Make (Local_name)


module Ty = Core.Ty

module rec Expr : sig

  type t =
    | Let of Local_name.t * Ty.t * comp * t
    | Bool_if of comp * t * t
    | Comp of comp

  and comp =
    | Item of Item_name.t * comp Iarray.t option
    | Prim of Prim.Op.t * comp Iarray.t
    | Var of Local_name.t
    | Bool of bool
    | I32 of int32

  type value = Prim.Value.t =
    | Bool of bool
    | I32 of int32

  val eval : Item.t Item_map.t -> value Local_map.t -> t -> value

end = struct

  include Expr

  let rec eval (items : Item.t Item_map.t) (locals : value Local_map.t) (expr : t) : value =
    match expr with
    | Let (name, _, def, body) ->
        let def = eval_comp items locals def in
        eval items (Local_map.add name def locals) body
    | Bool_if (expr1, expr2, expr3) ->
        begin match eval_comp items locals expr1 with
        | Bool true -> eval items locals expr2
        | Bool false -> eval items locals expr3
        | _ -> failwith "Expr.eval"
        end
    | Comp expr -> eval_comp items locals expr

  and eval_comp (items : Item.t Item_map.t) (locals : value Local_map.t) (expr : comp) : value =
    match expr with
    | Item (name, args) ->
        begin match Item_map.find name items, args with
        | Item.Fun (names, _, body), Some args ->
            let eval_arg (name, _) arg = name, eval_comp items locals arg in
            let args = Seq.map2 eval_arg (Iarray.to_seq names) (Iarray.to_seq args) in
            eval items (Local_map.add_seq args locals) body
        | Item.Val (_, body), None -> eval items locals body
        | _ -> failwith "Expr.eval_atom"
        end
    | Prim (op, args) ->
        Prim.Op.app op (Iarray.map (eval_comp items locals) args)
    | Var name -> Local_map.find name locals
    | Bool bool -> Bool bool
    | I32 int -> I32 int

end

and Item : sig

  type t =
    | Val of Ty.t * Expr.t
    | Fun of (Local_name.t * Ty.t) Iarray.t * Ty.t * Expr.t

end = Item

module Program = struct

  type t = Item.t Item_map.t

end
