(** Translate hoisted language to WAT (WebAssembly Text Format) *)

module Emit : sig

  val pp_program : ?tail_call:bool ->  Hoisted.Program.t -> Format.formatter -> unit
    [@@warning "-unused-value-declaration"]

end = struct

  module H = Hoisted

  type options = {
    tail_call : bool;
  }

  type item_ty_env = H.Ty.t H.Item_map.t
  type local_ty_env = H.Ty.t H.Local_map.t

  (** Collect the types of local definitions in an expression. This is useful
      pre-declaring locals inside function definitions *)
  let local_def_tys (expr : H.Expr.t) : local_ty_env =
    let union = H.Local_map.union (fun _ ty _ -> Some ty) in
    let rec go_expr expr =
      match expr with
      | H.Expr.Let (name, ty, _, body) -> H.Local_map.add name ty (go_expr body)
      | H.Expr.Bool_if (_, expr2, expr3) -> union (go_expr expr2) (go_expr expr3)
      (* All local definitions are floated to the top, so we don't need to
         traverse any further *)
      | H.Expr.Comp _ -> H.Local_map.empty
    in
    go_expr expr

  (** Find the type of an expression. This is useful for finding the result
      type of conditionals. *)
  let ty_of_expr (item_tys : item_ty_env) (local_tys : local_ty_env) (expr : H.Expr.t) =
    let rec go_expr expr =
      match expr with
      | H.Expr.Let (_, _, _, body) -> go_expr body
      | H.Expr.Bool_if (_, expr2, _) -> go_expr expr2
      | H.Expr.Comp expr -> go_comp expr
    and go_comp expr =
      match expr with
      | H.Expr.Prim (op, _) -> snd (Prim.Op.ty op)
      | H.Expr.Item (name, _) -> H.Item_map.find name item_tys
      | H.Expr.Var name -> H.Local_map.find name local_tys
      | H.Expr.Bool _ -> H.Ty.Bool
      | H.Expr.I32 _ -> H.Ty.I32
    in
    go_expr expr

  let pp_quoted s ppf = Format.fprintf ppf "\"%s\"" s
  let pp_i32 i ppf = Format.fprintf ppf "%li" i

  let pp_spaced_iter iter exprs ppf =
    Format.pp_print_iter iter (fun ppf expr -> expr ppf) ppf exprs
      ~pp_sep:Format.pp_print_space

  let pp_sexpr_cmd_iter name iter exprs ppf =
    Format.fprintf ppf "@[<hv 2>(%s@ %t)@]" name (pp_spaced_iter iter exprs)

  let pp_spaced = pp_spaced_iter List.iter
  let pp_spaced_seq = pp_spaced_iter Seq.iter
  let pp_sexpr_cmd name = pp_sexpr_cmd_iter name List.iter
  let pp_sexpr_cmd_seq name = pp_sexpr_cmd_iter name Seq.iter

  let pp_item_name (name : H.Item_name.t) (ppf : Format.formatter) =
    Format.fprintf ppf "$%s" (H.Item_name.to_string name)

  let pp_local_name (name : H.Local_name.t) (ppf : Format.formatter) =
    Format.fprintf ppf "$%s" (H.Local_name.to_string name)

  let pp_type (ty : H.Ty.t) (ppf : Format.formatter) =
    match ty with
    | Core.Ty.Bool -> Format.fprintf ppf "i32"
    | Core.Ty.I32 -> Format.fprintf ppf "i32"

  (** Emit expressions in {{: https://webassembly.github.io/spec/core/text/instructions.html#folded-instructions}
      {e folded} form}. *)
  let pp_expr (opts : options) (item_tys : item_ty_env) (local_tys : local_ty_env) (expr : H.Expr.t) (ppf : Format.formatter) =
    let rec go_expr expr =
      match expr with
      | H.Expr.Let (name, _, expr, body) ->
          pp_spaced [
            pp_sexpr_cmd "local.set" [
              pp_local_name name;
              go_comp expr;
            ];
            go_expr body;
          ]
      | H.Expr.Bool_if (expr1, expr2, expr3) ->
          pp_sexpr_cmd "if" [
            pp_sexpr_cmd "result" [pp_type (ty_of_expr item_tys local_tys expr2)];
            go_comp expr1;
            pp_sexpr_cmd "then" [go_expr expr2];
            pp_sexpr_cmd "else" [go_expr expr3];
          ]
      (* Emit tail-calls if possible *)
      | H.Expr.Comp (Item (name, Some args)) when opts.tail_call ->
          pp_sexpr_cmd "return_call" [pp_item_name name; go_args args]
      | H.Expr.Comp (Item (name, None)) when opts.tail_call ->
          pp_sexpr_cmd "return_call" [pp_item_name name]
      (* Otherwise emit a return instruction *)
      | H.Expr.Comp expr ->
          pp_sexpr_cmd "return" [go_comp expr]
    and go_comp expr =
      match expr with
      | H.Expr.Prim (Prim.Op.I32_eq, args) -> pp_sexpr_cmd "i32.eq" [go_args args];
      | H.Expr.Prim (Prim.Op.I32_add, args) -> pp_sexpr_cmd "i32.add" [go_args args];
      | H.Expr.Prim (Prim.Op.I32_sub, args) -> pp_sexpr_cmd "i32.sub" [go_args args];
      | H.Expr.Prim (Prim.Op.I32_mul, args) -> pp_sexpr_cmd "i32.mul" [go_args args];
      | H.Expr.Prim (Prim.Op.I32_neg, args) -> pp_sexpr_cmd "i32.neg" [go_args args];
      | H.Expr.Prim (Prim.Op.Bool_eq, args) -> pp_sexpr_cmd "i32.eq" [go_args args];
      | H.Expr.Item (name, Some args) -> pp_sexpr_cmd "call" [pp_item_name name; go_args args]
      | H.Expr.Item (name, None) -> pp_sexpr_cmd "call" [pp_item_name name]
      | H.Expr.Var name -> pp_sexpr_cmd "local.get" [pp_local_name name]
      | H.Expr.Bool true -> pp_sexpr_cmd "i32.const" [pp_i32 1l]
      | H.Expr.Bool false -> pp_sexpr_cmd "i32.const" [pp_i32 0l]
      | H.Expr.I32 int -> pp_sexpr_cmd "i32.const" [pp_i32 int]
    and go_args args =
      pp_spaced_seq (Iarray.to_seq args |> Seq.map go_comp)
    in
    go_expr expr ppf

  let pp_item (opts : options) (item_tys : item_ty_env) (name, item : H.(Item_name.t * Item.t)) (ppf : Format.formatter) =
    let pp_fun params ret_ty body =
      let pp_param (name, ty) = pp_sexpr_cmd "param" [pp_local_name name; pp_type ty]
      and pp_local (name, ty) = pp_sexpr_cmd "local" [pp_local_name name; pp_type ty]
      and locals = local_def_tys body in

      (* https://webassembly.github.io/spec/core/text/modules.html#functions *)
      pp_sexpr_cmd_seq "func" (Seq.concat @@ List.to_seq [
        Seq.singleton (pp_item_name name);
        Seq.singleton (pp_sexpr_cmd "export" [pp_quoted (H.Item_name.to_string name)]);
        Iarray.to_seq params |> Seq.map pp_param;
        Seq.singleton (pp_sexpr_cmd "result" [pp_type ret_ty]);
        H.Local_map.to_seq locals |> Seq.map pp_local;
        Seq.singleton (
          let local_tys =
            Seq.append (Iarray.to_seq params) (H.Local_map.to_seq locals)
            |> H.Local_map.of_seq
          in
          pp_expr opts item_tys local_tys body
        );
      ])
    in
    match item with
    | H.Item.Val (ty, expr) -> pp_fun [||] ty expr ppf (* FIXME: re-evaluation of top-level values *)
    | H.Item.Fun (params, ret_ty, body) -> pp_fun params ret_ty body ppf

  let pp_program ?(tail_call = false) (program : H.Program.t) : Format.formatter -> unit =
    let opts = { tail_call } in
    let item_tys = program |> H.Item_map.map @@ function
      | H.Item.Val (ty, _) -> ty
      | H.Item.Fun (_, ret_ty, _) -> ret_ty
    in
    (* https://webassembly.github.io/spec/core/text/modules.html#text-module *)
    pp_sexpr_cmd_seq "module"
      (H.Item_map.to_seq program |> Seq.map (pp_item opts item_tys))

end

include Emit
