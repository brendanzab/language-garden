(** The core language *)

module Item_name = Name.Make ()
module Item_map = Map.Make (Item_name)
module Local = Name.Debruijn.Make ()

module Ty = struct

  type t =
    | Bool
    | I32
    | Fun of t Iarray.t * t

  let rec pp (ty : t) : Format.formatter -> unit =
    match ty with
    | Fun (param_tys, result_ty) ->
        Format.dprintf "@[fun@ (%t)@ ->@]@ %t"
          (fun ppf ->
            Format.pp_print_iter Iarray.iter (Fun.flip pp) ppf param_tys
              ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ "))
          (pp result_ty)
    | ty -> pp_atomic ty
  and pp_atomic (ty : t) : Format.formatter -> unit =
    match ty with
    | Bool -> Format.dprintf "Bool"
    | I32 -> Format.dprintf "I32"
    | Fun _ as ty -> Format.dprintf "@[(%t)@]" (pp ty)

  let of_prim (ty : Prim.Ty.t) : t =
    match ty with
    | Prim.Ty.Bool -> Bool
    | Prim.Ty.I32 -> I32

end

module rec Expr : sig

  type t =
    | Item of Item_name.t * Ty.t
    | Var of Local.Index.t * Ty.t
    | Let of def * t
    | Fun_app of t * t Iarray.t
    | Bool of bool
    | Bool_if of t * t * t
    | I32 of int32
    | Prim of Prim.Op.t * t Iarray.t

  and def = string option * Ty.t * t

  val ty_of : t -> Ty.t

  type value =
    | Item of Item.t
    | Bool of bool
    | I32 of int32

  val eval : Item.t Item_map.t -> value Local.Env.t -> t -> value

end = struct

  include Expr

  let rec ty_of (expr : t) : Ty.t =
    match expr with
    | Item (_, ty) -> ty
    | Var (_, ty) -> ty
    | Let (_, body) -> ty_of body
    | Fun_app (head, args) ->
        begin match ty_of head with
        | Ty.Fun (_, result_ty) -> result_ty
        | _ -> invalid_arg "Core.Expr.ty_of: type error"
        end
    | Bool _ -> Ty.Bool
    | Bool_if (_, expr2, _) -> ty_of expr2
    | I32 _ -> Ty.I32
    | Prim (op, _) -> Ty.of_prim (snd (Prim.Op.ty op))

  let rec eval (items : Item.t Item_map.t) (locals : value Local.Env.t) (expr : t) : value =
    match expr with
    | Item (name, _) ->
        begin match Item_map.find name items with
        | Item.Val (_, body) -> eval items locals body
        | Item.Fun _ as fun_ -> Item fun_
        end
    | Var (index, _) -> Local.Env.lookup index locals
    | Let ((_, _, def), body) ->
        let def = eval items locals def in
        eval items (Local.Env.extend def locals) body
    | Fun_app (head, args) ->
        begin match eval items locals head with
        | Item (Item.Fun (_, _, body)) ->
            let env = Iarray.to_seq args |> Seq.map (eval items locals) |> Local.Env.of_seq in
            eval items env body
        | _ -> failwith "Expr.eval"
        end
    | Bool bool -> Bool bool
    | Bool_if (expr1, expr2, expr3) ->
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
            | _ -> failwith "Expr.eval"
        in
        match Prim.Op.app op args with
        | Prim.Value.Bool bool -> Bool bool
        | Prim.Value.I32 int -> I32 int

end

and Item : sig

  type t =
    | Val of Ty.t * Expr.t
    | Fun of (string option * Ty.t) Iarray.t * Ty.t * Expr.t

end = Item

module Module = struct

  type t = Item.t Item_map.t  (* TODO: Preserve order? *)

end
