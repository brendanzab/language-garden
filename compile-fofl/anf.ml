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
    | Let of Local_id.t * Ty.t option * comp * t
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
    | Item of Item_name.t * atom Iarray.t option
    | Prim of Prim.Op.t * atom Iarray.t
    | Atom of atom

  (** Atomic expressions *)
  and atom =
    | Var of Local_id.t
    | Bool of bool
    | I32 of int32

  type value = Prim.Value.t =
    | Bool of bool
    | I32 of int32

  val eval : Item.t Item_map.t -> t -> value

  val pp : t -> Format.formatter -> unit

end = struct

  include Expr

  (* Evaluation *)

  let eval (items : Item.t Item_map.t) (expr : t) : value =
    let rec eval (joins : (Local_id.t * t) Join_map.t) (locals : value Local_map.t) (expr : t) : value =
      match expr with
      | Let (id, _, def, body) ->
          let def = eval_comp joins locals def in
          eval joins (Local_map.add id def locals) body
      | Join (id, (param_id, _), cont, body) ->
          eval (Join_map.add id (param_id, cont) joins) locals body
      | Jump (id, arg) ->
          let param_id, def = Join_map.find id joins in
          eval joins (Local_map.add param_id (eval_atom locals arg) locals) def
      | Bool_if (expr1, expr2, expr3) ->
          begin match eval_atom locals expr1 with
          | Bool true -> eval joins locals expr2
          | Bool false -> eval joins locals expr3
          | _ -> failwith "Expr.eval"
          end
      | Return expr -> eval_comp joins locals expr

    and eval_comp (joins : (Local_id.t * t) Join_map.t) (locals : value Local_map.t) (expr : comp) : value =
      match expr with
      | Item (id, args) ->
          begin match Item_map.find id items, args with
          | Item.Fun (params, _, body), Some args ->
              let eval_arg (id, _) arg = id, eval_atom locals arg in
              let args = Seq.map2 eval_arg (Iarray.to_seq params) (Iarray.to_seq args) in
              eval joins (Local_map.add_seq args locals) body
          | Item.Val (_, body), None -> eval joins locals body
          | _ -> failwith "Expr.eval_atom"
          end
      | Prim (op, args) ->
          Prim.Op.app op (Iarray.map (eval_atom locals) args)
      | Atom expr ->
          eval_atom locals expr

    and eval_atom (locals : value Local_map.t) (expr : atom) : value =
      match expr with
      | Var id -> Local_map.find id locals
      | Bool bool -> Bool bool
      | I32 int -> I32 int
    in

    eval Join_map.empty Local_map.empty expr


  (* Pretty printing *)

  let pp_atom (expr : atom) =
    match expr with
    | Var id -> Format.dprintf "%t" (Local_id.pp id)
    | Bool true -> Format.dprintf "true"
    | Bool false -> Format.dprintf "false"
    | I32 int -> Format.dprintf "%li" int

  let pp_args (args : atom Iarray.t) (ppf : Format.formatter) =
    (* TODO: trailing comma *)
    let pp_sep ppf () = Format.fprintf ppf ",@ " in
    Format.pp_print_iter Iarray.iter (Fun.flip pp_atom) ppf args ~pp_sep

  let pp_comp (expr : comp) =
    match expr with
    | Item (id, None) -> Format.dprintf "%t" (Item_name.pp id)
    | Item (id, Some args) -> Format.dprintf "%t(%t)" (Item_name.pp id) (pp_args args)
    | Prim (op, args) -> Format.dprintf "%t(%t)" (Prim.Op.pp op) (pp_args args)
    | Atom expr -> pp_atom expr

  let rec pp (expr : t) =
    match expr with
    | Let (_, _, _, _) | Join (_, _, _, _) ->
        let rec go expr =
          match expr with
          | Let (id, def_ty, def, body) ->
              Format.dprintf "@[<2>@[let %t@ :=@]@ @[%t;@]@]@ %t"
                (match def_ty with
                  | None -> Format.dprintf "%t" (Local_id.pp id)
                  | Some def_ty ->
                      Format.dprintf "@[<2>@[%t :@]@ %t@]"
                        (Local_id.pp id)
                        (Ty.pp def_ty))
                (pp_comp def)
                (go body)
          | Join (id, (param_id, param_ty), cont, body) ->
              Format.dprintf "@[<2>@[join %s@ %t@ :=@]@ @[%t;@]@]@ %t"
                (Join_id.to_string id)
                (Format.dprintf "@[<2>(@[%t@ :@]@ %t)@]"
                  (Local_id.pp param_id)
                  (Ty.pp param_ty))
                (pp cont)
                (go body)
          | _ ->
              Format.dprintf "@[%t@]" (pp expr)
        in
        Format.dprintf "@[<v>%t@]" (go expr)
    | Jump (id, arg) ->
        Format.dprintf "@[<2>@[jump@ %s@]@ %t@]"
          (Join_id.to_string id)
          (pp_atom arg)
    | Bool_if (expr1, expr2, expr3) ->
        Format.dprintf "@[<hv>@[if@ %t@ then@]@;<1 2>@[%t@]@ else@;<1 2>@[%t@]@]"
          (pp_atom expr1)
          (pp expr2) (* FIXME: precedence *)
          (pp expr3)
    | Return expr ->
        pp_comp expr

end

and Item : sig

  type t =
    | Val of Ty.t * Expr.t
    | Fun of (Local_id.t * Ty.t) Iarray.t * Ty.t * Expr.t

end = Item

module Module = struct

  type t = Item.t Item_map.t

  let pp_params (args : (Local_id.t * Ty.t) Iarray.t) (ppf : Format.formatter) =
    (* TODO: trailing comma *)
    let pp_sep ppf () = Format.fprintf ppf ",@ " in
    let pp_param ppf (id, ty) =
      Format.fprintf ppf "%t@ :@ %t" (Local_id.pp id) (Ty.pp ty)
    in
    Format.pp_print_iter Iarray.iter pp_param ppf args ~pp_sep

  let rec pp_item (name, item : Item_name.t * Item.t) =
    match item with
    | Item.Val (ty, expr) ->
        Format.dprintf "@[<2>@[val %t@ :@ %t@ :=@]@ @[%t;@]@]\n"
          (Item_name.pp name)
          (Ty.pp ty)
          (Expr.pp expr)

    | Item.Fun (params, ty, expr) ->
        Format.dprintf "@[<2>@[fun %t(%t)@ :@ %t@ :=@]@ @[%t;@]@]\n"
          (Item_name.pp name)
          (pp_params params)
          (Ty.pp ty)
          (Expr.pp expr)

  let rec pp (mod_ : t) (ppf : Format.formatter) =
    Format.pp_print_seq (Fun.flip pp_item) ppf (Item_map.to_seq mod_)
      ~pp_sep:Format.pp_print_newline

end
