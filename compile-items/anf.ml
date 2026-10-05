(** A language that binds the result of each computation to an intermediate
    definitions, and only supports branching at the top of expressions. This
    should make it easier to translate to languages like LLVM-IR.

    - {{: https://en.wikipedia.org/wiki/A-normal_form} A-Normal Form} on Wikipedia
    - {{: https://doi.org/10.1145/173262.155113} The essence of compiling with continuations}
    - {{: https://matt.might.net/articles/a-normalization/} A-Normalization: Why and How (with code)}
*)

module Item_name = Core.Item_name
module Item_map = Core.Item_map

module Local_id = Name.Make ()
module Local_map = Map.Make (Local_id)

module Join_id = Name.Make ()
module Join_map = Map.Make (Join_id)

module Ty = Core.Ty

module rec Expr : sig

  (** Top-level expressions *)
  type t =
    | Let of Local_id.t * Ty.t * comp * t
    | Bool_if of atom * t * t
    | Return of comp

    | Join of Join_id.t * (Local_id.t * Ty.t) * t * t
    (** A join point, where different paths of computation come together. This
        is like a let binding, but it must only ever be invoked in the
        tail-position. *)

    | Jump of Join_id.t * atom
    (** Jump to a join point *)

  (** Computation expressions *)
  and comp =
    | Item of Item_name.t * atom Iarray.t * Ty.t
    | Prim of Prim.Op.t * atom Iarray.t
    | Atom of atom

  (** Atomic expressions *)
  and atom =
    | Item of Item_name.t * Ty.t
    | Var of Local_id.t * Ty.t
    | Bool of bool
    | I32 of int32

  val ty_of_comp : comp -> Ty.t
  val ty_of_atom : atom -> Ty.t

end = struct

  include Expr

  let rec ty_of_atom (expr : atom) : Ty.t =
    match expr with
    | Item (_, ty) -> ty
    | Var (_, ty) -> ty
    | Bool _ -> Ty.Bool
    | I32 _ -> Ty.I32

  let rec ty_of_comp (expr : comp) : Ty.t =
    match expr with
    | Item (_, _, ty) -> ty
    | Prim (op, _) -> Ty.of_prim (snd (Prim.Op.ty op))
    | Atom expr -> ty_of_atom expr

end

module Item = struct

  (** Visibility of an item *)
  type vis = Core.Item.vis =
    | Pub
    | Priv

  type t =
    | Val of vis * Ty.t * Expr.t
    | Fun of vis * (Local_id.t * Ty.t) Iarray.t * Ty.t * Expr.t

end

module Module = struct

  type t = Item.t Item_map.t

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

  let eval_expr (items : Module.t) (expr : Expr.t) : value =
    let rec eval_expr (joins : (Local_id.t * Expr.t) Join_map.t) (locals : value Local_map.t) (expr : Expr.t) : value =
      match expr with
      | Expr.Let (id, _, def, body) ->
          let def = eval_comp joins locals def in
          eval_expr joins (Local_map.add id def locals) body
      | Expr.Join (id, (param_id, _), cont, body) ->
          eval_expr (Join_map.add id (param_id, cont) joins) locals body
      | Expr.Jump (id, arg) ->
          let param_id, def = Join_map.find id joins in
          eval_expr joins (Local_map.add param_id (eval_atom locals arg) locals) def
      | Expr.Bool_if (expr1, expr2, expr3) ->
          begin match eval_atom locals expr1 with
          | Bool true -> eval_expr joins locals expr2
          | Bool false -> eval_expr joins locals expr3
          | _ -> failwith "Expr.eval"
          end
      | Expr.Return expr -> eval_comp joins locals expr

    and eval_comp (joins : (Local_id.t * Expr.t) Join_map.t) (locals : value Local_map.t) (expr : Expr.comp) : value =
      match expr with
      | Expr.Item (id, args, _) ->
          begin match Item_map.find id items, args with
          | Item.Fun (_, params, _, body), args ->
              let eval_arg (id, _) arg = id, eval_atom locals arg in
              let args = Seq.map2 eval_arg (Iarray.to_seq params) (Iarray.to_seq args) in
              eval_expr joins (Local_map.add_seq args locals) body
          | _ -> failwith "Expr.eval_comp"
          end
      | Expr.Prim (op, args) ->
          let args =
            args |> Iarray.map @@ fun arg ->
              match eval_atom locals arg with
              | Bool bool -> Prim.Value.Bool bool
              | I32 int -> Prim.Value.I32 int
              | _ -> failwith "Expr.eval"
          in
          begin match Prim.Op.app op args with
          | Prim.Value.Bool bool -> Bool bool
          | Prim.Value.I32 int -> I32 int
          end
      | Expr.Atom expr ->
          eval_atom locals expr

    and eval_atom (locals : value Local_map.t) (expr : Expr.atom) : value =
      match expr with
      | Expr.Item (name, _) ->
          begin match Item_map.find name items with
          | Item.Val (_, _, body) -> eval_expr (Join_map.empty) locals body
          | Item.Fun _ as fun_ -> Item fun_
          end
      | Expr.Var (id, _) -> Local_map.find id locals
      | Expr.Bool bool -> Bool bool
      | Expr.I32 int -> I32 int
    in

    eval_expr Join_map.empty Local_map.empty expr

end

(** Pretty printing *)
module Pretty : sig

  val pp_ty : Ty.t -> Format.formatter -> unit
  val pp_module : Module.t -> Format.formatter -> unit

end = struct

  let pp_ty = Core.Pretty.pp_ty

  let pp_atom (expr : Expr.atom) =
    match expr with
    | Expr.Item (id, _) -> Format.dprintf "%t" (Item_name.pp id)
    | Expr.Var (id, _) -> Format.dprintf "%t" (Local_id.pp id)
    | Expr.Bool true -> Format.dprintf "true"
    | Expr.Bool false -> Format.dprintf "false"
    | Expr.I32 int -> Format.dprintf "%li" int

  let pp_args (args : Expr.atom Iarray.t) (ppf : Format.formatter) =
    (* TODO: trailing comma *)
    let pp_sep ppf () = Format.fprintf ppf ",@ " in
    Format.pp_print_iter Iarray.iter (Fun.flip pp_atom) ppf args ~pp_sep

  let pp_comp (expr : Expr.comp) =
    match expr with
    | Expr.Item (id, args, _) -> Format.dprintf "%t(%t)" (Item_name.pp id) (pp_args args)
    | Expr.Prim (op, args) -> Format.dprintf "%t(%t)" (Prim.Op.pp op) (pp_args args)
    | Expr.Atom expr -> pp_atom expr

  let rec pp_expr (expr : Expr.t) =
    match expr with
    | Expr.Let (_, _, _, _) | Expr.Join (_, _, _, _) ->
        let rec go expr =
          match expr with
          | Expr.Let (id, def_ty, def, body) ->
              Format.dprintf "@[<2>@[let %t@ :=@]@ @[%t;@]@]@ %t"
                (Format.dprintf "@[<2>@[%t :@]@ %t@]"
                  (Local_id.pp id)
                  (pp_ty def_ty))
                (pp_comp def)
                (go body)
          | Expr.Join (id, (param_id, param_ty), cont, body) ->
              Format.dprintf "@[<2>@[join %s@ %t@ :=@]@ @[%t;@]@]@ %t"
                (Join_id.to_string id)
                (Format.dprintf "@[<2>(@[%t@ :@]@ %t)@]"
                  (Local_id.pp param_id)
                  (pp_ty param_ty))
                (pp_expr cont)
                (go body)
          | _ ->
              Format.dprintf "@[%t@]" (pp_expr expr)
        in
        Format.dprintf "@[<v>%t@]" (go expr)
    | Expr.Jump (id, arg) ->
        Format.dprintf "@[<2>@[jump@ %s@]@ %t@]"
          (Join_id.to_string id)
          (pp_atom arg)
    | Expr.Bool_if (expr1, expr2, expr3) ->
        Format.dprintf "@[<hv>@[if@ %t@ then@]@;<1 2>@[%t@]@ else@;<1 2>@[%t@]@]"
          (pp_atom expr1)
          (pp_expr expr2) (* FIXME: precedence *)
          (pp_expr expr3)
    | Expr.Return expr ->
        pp_comp expr

  let pp_params (args : (Local_id.t * Ty.t) Iarray.t) (ppf : Format.formatter) =
    (* TODO: trailing comma *)
    let pp_sep ppf () = Format.fprintf ppf ",@ " in
    let pp_param ppf (id, ty) =
      Format.fprintf ppf "%t@ :@ %t" (Local_id.pp id) (pp_ty ty)
    in
    Format.pp_print_iter Iarray.iter pp_param ppf args ~pp_sep

  let pp_vis (vis : Item.vis) =
    match vis with
    | Item.Pub -> Format.dprintf "pub"
    | Item.Priv -> Format.dprintf "priv"

  let rec pp_item (name, item : Item_name.t * Item.t) =
    match item with
    | Item.Val (vis, ty, expr) ->
        Format.dprintf "@[<2>@[%t@ val %t@ :@ %t@ :=@]@ @[%t;@]@]\n"
          (pp_vis vis)
          (Item_name.pp name)
          (pp_ty ty)
          (pp_expr expr)

    | Item.Fun (vis, params, ty, expr) ->
        Format.dprintf "@[<2>@[%t@ fun %t(%t)@ :@ %t@ :=@]@ @[%t;@]@]\n"
          (pp_vis vis)
          (Item_name.pp name)
          (pp_params params)
          (pp_ty ty)
          (pp_expr expr)

  let rec pp_module (mod_ : Module.t) (ppf : Format.formatter) =
    Format.pp_print_seq (Fun.flip pp_item) ppf (Item_map.to_seq mod_)
      ~pp_sep:Format.pp_print_newline

end
