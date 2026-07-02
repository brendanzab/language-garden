(** The core language *)

module Item_name = Name.Make ()
module Item_map = Map.Make (Item_name)
module Local = Name.Debruijn.Make ()

module Ty = struct

  type t =
    | Bool
    | I32

  let pp (ty : t) : Format.formatter -> unit =
    match ty with
    | Bool -> Format.dprintf "Bool"
    | I32 -> Format.dprintf "I32"

  let of_prim (ty : Prim.Ty.t) : t =
    match ty with
    | Prim.Ty.Bool -> Bool
    | Prim.Ty.I32 -> I32

end

module rec Expr : sig

  type t =
    | Item of Item_name.t * t Iarray.t option
    | Var of Local.Index.t
    | Let of def * t
    | Bool of bool
    | Bool_if of t * t * t * Ty.t
    | I32 of int32
    | Prim of Prim.Op.t * t Iarray.t

  and def = string option * Ty.t * t

  type value =
    | Bool of bool
    | I32 of int32

  val eval : Item.t Item_map.t -> value Local.Env.t -> t -> value

end = struct

  include Expr

  let rec eval (items : Item.t Item_map.t) (locals : value Local.Env.t) (expr : t) : value =
    match expr with
    | Item (name, args) ->
        begin match Item_map.find name items, args with
        | Item.Val (_, body), None ->
            eval items locals body
        | Item.Fun (_, _, body), Some args ->
            let env = Iarray.to_seq args |> Seq.map (eval items locals) |> Local.Env.of_seq in
            eval items env body
        | _, _ -> failwith "Expr.eval"
        end
    | Var index -> Local.Env.lookup index locals
    | Let ((_, _, def), body) ->
        let def = eval items locals def in
        eval items (Local.Env.extend def locals) body
    | Bool bool -> Bool bool
    | Bool_if (expr1, expr2, expr3, _) ->
        begin match eval items locals expr1 with
        | Bool true -> eval items locals expr2
        | Bool false -> eval items locals expr3
        | _ -> failwith "Expr.eval"
        end
    | I32 int -> I32 int
    | Prim (op, args) ->
        let args =
          args |> Iarray.map @@ fun arg ->
            match eval items locals arg with
            | Bool bool -> Prim.Value.Bool bool
            | I32 int -> Prim.Value.I32 int
        in
        begin match Prim.Op.app op args with
        | Prim.Value.Bool bool -> Bool bool
        | Prim.Value.I32 int -> I32 int
        end

end

and Item : sig

  type t =
    | Val of Ty.t * Expr.t
    | Fun of (string option * Ty.t) Iarray.t * Ty.t * Expr.t

end = Item

module Module = struct

  type t = Item.t Item_map.t  (* TODO: Preserve order? *)

end
