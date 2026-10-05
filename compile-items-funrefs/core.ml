(** The core language *)

module Item_name = Name.Make ()
module Item_map = Map.Make (Item_name)
module Local = Name.Debruijn.Make ()

module Ty = struct

  type t =
    | Bool
    | I32
    | Fun of t Iarray.t * t

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

end

module Item = struct

  (** Visibility of an item *)
  type vis =
    | Pub
    | Priv

  type t =
    | Val of vis * Ty.t * Expr.t
    | Fun of vis * (string option * Ty.t) Iarray.t * Ty.t * Expr.t

end

module Module = struct

  type t = Item.t Item_map.t  (* TODO: Preserve order? *)

end

(** Tree-walking interpreter *)
module Interpret : sig

  type value =
    | Item of Item.t
    | Bool of bool
    | I32 of int32

  val eval_expr : Module.t -> Expr.t -> value

end = struct

  type value =
    | Item of Item.t
    | Bool of bool
    | I32 of int32

  let rec eval_expr (items : Item.t Item_map.t) (locals : value Local.Env.t) (expr : Expr.t) : value =
    match expr with
    | Expr.Item (name, _) ->
        begin match Item_map.find name items with
        | Item.Val (_, _, body) -> eval_expr items locals body
        | Item.Fun _ as fun_ -> Item fun_
        end
    | Expr.Var (index, _) -> Local.Env.lookup index locals
    | Expr.Let ((_, _, def), body) ->
        let def = eval_expr items locals def in
        eval_expr items (Local.Env.extend def locals) body
    | Expr.Fun_app (head, args) ->
        begin match eval_expr items locals head with
        | Item (Item.Fun (_, _, _, body)) ->
            let env = Iarray.to_seq args |> Seq.map (eval_expr items locals) |> Local.Env.of_seq in
            eval_expr items env body
        | _ -> failwith "Expr.eval"
        end
    | Expr.Bool bool -> Bool bool
    | Expr.Bool_if (expr1, expr2, expr3) ->
        begin match eval_expr items locals expr1 with
        | Bool true -> eval_expr items locals expr2
        | Bool false -> eval_expr items locals expr3
        | _ -> failwith "Expr.eval"
        end
    | Expr.I32 int -> I32 int
    | Expr.Prim (op, args) ->
        let args =
          args |> Iarray.map @@ fun arg ->
            match eval_expr items locals arg with
            | Bool bool -> Prim.Value.Bool bool
            | I32 int -> Prim.Value.I32 int
            | _ -> failwith "Expr.eval"
        in
        match Prim.Op.app op args with
        | Prim.Value.Bool bool -> Bool bool
        | Prim.Value.I32 int -> I32 int

  let eval_expr (items : Item.t Item_map.t) (expr : Expr.t) : value =
    eval_expr items Local.Env.empty expr

end

(** Pretty printing *)
module Pretty : sig

  val pp_ty : Ty.t -> Format.formatter -> unit

end = struct

  let rec pp_ty (ty : Ty.t) : Format.formatter -> unit =
    match ty with
    | Ty.Fun (param_tys, result_ty) ->
        Format.dprintf "@[fun@ (%t)@ ->@]@ %t"
          (fun ppf ->
            Format.pp_print_iter Iarray.iter (Fun.flip pp_ty) ppf param_tys
              ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ "))
          (pp_ty result_ty)
    | ty -> pp_atomic_ty ty

  and pp_atomic_ty (ty : Ty.t) : Format.formatter -> unit =
    match ty with
    | Ty.Bool -> Format.dprintf "Bool"
    | Ty.I32 -> Format.dprintf "I32"
    | Ty.Fun _ as ty -> Format.dprintf "@[(%t)@]" (pp_ty ty)

  (* TODO: Pretty print expressions and modules *)

end
