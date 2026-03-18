(** {0 Surface language}

    The surface language closely mirrors what the programmer originally wrote,
    including syntactic sugar and higher level language features that make
    programming more convenient (in comparison to the {!Core}).
*)

(** {1 Syntax} *)

(** The start and end position in a source file *)
module Span = struct

  type t =
    Lexing.position * Lexing.position

end

(** Spanned nodes *)
module Spanned = struct

  type 'a t = {
    span : Span.t;
    data : 'a;
  }

end

(** Types in the surface language *)
module Ty = struct

  type t =
    data Spanned.t

  and data =
    | Name of string

end

(** Terms in the surface language *)
module Expr = struct

  type t =
    data Spanned.t

  and data =
    | Name of string Spanned.t * t Iarray.t option
    | Prim of string Spanned.t * t Iarray.t
    | Let of binder * Ty.t option * t * t
    | Ann of t * Ty.t
    | I32 of int32
    | If_then_else of t * t * t
    | Infix of [`Eq | `Add | `Sub | `Mul] * t * t
    | Prefix of [`Neg] * t

  (** Names that bind definitions or parameters *)
  and binder =
    string option Spanned.t

end

module Item = struct

  type t =
    | Val of binder * Ty.t * Expr.t
    | Fun of binder * param Iarray.t * Ty.t * Expr.t

  and binder =
    string Spanned.t

  (** Function parameters, with optional type annotations *)
  and param =
    Expr.binder * Ty.t

  let name item =
    match item with
    | Val (name, _, _) -> Core.Item_name.make name.data
    | Fun (name, _, _, _) -> Core.Item_name.make name.data

end

module Program = struct

  type t = Item.t list

end

(** Elaboration from the surface language into the core language *)
module Elab = struct

  (** {2 Elaboration context} *)

  type item_sig =
    | Val of Core.Ty.t
    | Fun of Core.Ty.t Iarray.t * Core.Ty.t

  module Env = struct

    type t = {
      item_sigs : item_sig Core.Item_map.t;
      local_names : string option Core.Local.Env.t;
      local_tys : Core.Ty.t Core.Local.Env.t;
    }

    let make (item_sigs : item_sig Core.Item_map.t) : t =
      { item_sigs;
        local_names = Core.Local.Env.empty;
        local_tys = Core.Local.Env.empty;
      }

    let add_local (env : t) (name : string option) (ty : Core.Ty.t) : t =
      { env with
        local_names = Core.Local.Env.extend name env.local_names;
        local_tys = Core.Local.Env.extend ty env.local_tys;
      }

    let lookup (env : t) (name : string) : [
      | `Item of  Core.Item_name.t * item_sig
      | `Expr of Core.Expr.t * Core.Ty.t
    ] option =
      let item_name = Core.Item_name.make name in
      match Core.Item_map.find_opt item_name env.item_sigs with
      | Some item -> Some (`Item (item_name, item))
      | None ->
          match Core.Local.Env.find_index ((=) (Some name)) env.local_names with
          | Some index -> Some (`Expr Core.(Expr.Var index, Local.Env.lookup index env.local_tys))
          | None when name = "true" -> Some (`Expr Core.(Expr.Bool true, Ty.Bool))
          | None when name = "false" -> Some (`Expr Core.(Expr.Bool false, Ty.Bool))
          | None -> None

  end


  (** {2 Elaboration errors} *)

  exception Error of Span.t * string

  (** Raise an elaboration error with a formatted message *)
  let error (type a b) (span : Span.t) : (b, Format.formatter, unit, a) format4 -> b =
    Format.kasprintf (fun message -> raise (Error (span, message)))

  let unify_tys (span : Span.t) ~(found : Core.Ty.t) ~(expected : Core.Ty.t) =
    if found = expected then () else
      error span "@[<v 2>@[mismatched types:@]@ @[expected: %t@]@ @[   found: %t@]@]"
        (Core.Ty.pp expected)
        (Core.Ty.pp found)


  (** {2 Bidirectional type checking} *)

  let check_ty (t : Ty.t) : Core.Ty.t =
    match t.data with
    | Ty.Name "Bool" -> Core.Ty.Bool
    | Ty.Name "I32" -> Core.Ty.I32
    | Ty.Name name -> error t.span "unbound type name `%s`" name

  let rec infer_expr (env : Env.t) (expr : Expr.t) : Core.Expr.t * Core.Ty.t =
    match expr.data with
    | Expr.Name (name, args) ->
        (* TODO: Clean this up!! *)
        begin match Env.lookup env name.data, args with
        | Some (`Item (name, Val ty)), None -> Core.Expr.Item (name, None), ty
        | Some (`Item (name, Fun (params, ty))), Some args when Iarray.length params = Iarray.length args ->
            Core.Expr.Item (name, Some (Iarray.map2 (check_expr env) args params)), ty
        | Some (`Expr (expr, ty)), None -> expr, ty
        | Some _, Some _ -> error name.span "mismatched arity"
        | Some _, None -> error name.span "unexpected arguments"
        | None, _ -> error name.span "unbound name"
        end

    | Expr.Prim (name, args) ->
        begin match Prim.Op.lookup name.data |> Option.map (fun op -> op, Prim.Op.ty op) with
        | Some (op, (params, ty)) when Iarray.length params = Iarray.length args ->
            Core.Expr.Prim (op, Iarray.map2 (check_expr env) args params), ty
        | Some _ -> error name.span "mismatched arity"
        | None -> error name.span "unknown primitive operation `#%s`" name.data
        end

    | Expr.Let (name, def_ty, def, body) ->
        let env, def, def_ty = check_def env name def_ty def in
        let body, ty = infer_expr env body in
        Core.(Expr.Let (name.data, def_ty, def, body), ty)

    | Expr.Ann (expr, ty) ->
        let ty = check_ty ty in
        check_expr env expr ty, ty

    | Expr.I32 int ->
        Core.Expr.I32 int, Core.Ty.I32

    | Expr.If_then_else (expr1, expr2, expr3) ->
        let expr1 = check_expr env expr1 Core.Ty.Bool in
        let expr2, ty = infer_expr env expr2 in
        let expr3 = check_expr env expr3 ty in
        Core.Expr.Bool_if (expr1, expr2, expr3), ty

    | Expr.Infix (`Eq, expr1, expr2) ->
        let expr1, ty1 = infer_expr env expr1 in
        let expr2, ty2 = infer_expr env expr2 in
        unify_tys expr.span ~found:ty2 ~expected:ty1;
        begin match ty1 with
        | Core.Ty.Bool -> Core.Expr.Prim (Prim.Op.Bool_eq, [|expr1; expr2|]), Core.Ty.Bool
        | Core.Ty.I32 -> Core.Expr.Prim (Prim.Op.I32_eq, [|expr1; expr2|]), Core.Ty.Bool
        end

    | Expr.Infix ((`Add | `Sub | `Mul) as op, expr1, expr2) ->
        let op =
          match op with
          | `Add -> Prim.Op.I32_add
          | `Sub -> Prim.Op.I32_sub
          | `Mul -> Prim.Op.I32_mul
        in
        let expr1 = check_expr env expr1 Core.Ty.I32 in
        let expr2 = check_expr env expr2 Core.Ty.I32 in
        Core.Expr.Prim (op, [|expr1; expr2|]), Core.Ty.I32

    | Expr.Prefix (`Neg, expr) ->
        let expr = check_expr env expr Core.Ty.I32 in
        Core.Expr.Prim (Prim.Op.I32_neg, [|expr|]), Core.Ty.I32

  and check_expr (env : Env.t) (expr : Expr.t) (ty : Core.Ty.t) : Core.Expr.t =
    match expr.data with
    | Expr.Let (name, def_ty, def, body) ->
        let env, def, def_ty = check_def env name def_ty def in
        let body = check_expr env body ty in
        Core.Expr.Let (name.data, def_ty, def, body)

    | Expr.If_then_else (expr1, expr2, expr3) ->
        let expr1 = check_expr env expr1 Core.Ty.Bool in
        let expr2 = check_expr env expr2 ty in
        let expr3 = check_expr env expr3 ty in
        Core.Expr.Bool_if (expr1, expr2, expr3)

    | _ ->
        let expr', found_ty = infer_expr env expr in
        unify_tys expr.span ~found:found_ty ~expected:ty;
        expr'

  and check_def (env : Env.t) (name : Expr.binder) (def_ty : Ty.t option) (def : Expr.t) : Env.t * Core.Expr.t * Prim.Ty.t =
    let def, def_ty =
      match def_ty with
      | Some ty ->
          let ty = check_ty ty in
          check_expr env def ty, ty
      | None -> infer_expr env def
    in
    Env.add_local env name.data def_ty, def, def_ty

  let check_program (prog : Program.t) : Core.Program.t =
    let check_item_sig items_sigs item =
      let name =
        match item with
        | Item.Val (n, _, _) | Item.Fun (n, _, _, _) ->
            let name = Core.Item_name.make n.data in
            if not (Core.Item_map.mem name items_sigs) then name else
              error n.span "item name already used"
      in
      match item with
      | Item.Val (_, ty, _) ->
          items_sigs |> Core.Item_map.add name (Val (check_ty ty))
      | Item.Fun (_, params, ty, _) ->
          let params = Iarray.map (fun (_, ty) -> check_ty ty) params in
          items_sigs |> Core.Item_map.add name (Fun (params, check_ty ty))
    in

    let item_sigs =
      List.fold_left check_item_sig Core.Item_map.empty prog
    in

    let check_item item =
      let name = Item.name item in
      let env = Env.make item_sigs in
      match item, Core.Item_map.find name item_sigs with
      | Item.Val (_, _, expr), Val ty ->
          name, Core.Item.Val (ty, check_expr env expr ty)
      | Item.Fun (_, params, _, expr), Fun (param_tys, ty) ->
          (* FIXME: Check duplicate names *)
          let params = Iarray.map2 (fun (name, _ : Expr.binder * _) ty -> name.data, ty) params param_tys in
          let env = Iarray.fold_right (fun (name, ty) env -> Env.add_local env name ty) params env in
          let expr = check_expr env expr ty in
          name, Core.Item.Fun (params, ty, expr)
      | _, _ ->
          failwith "item does not match item signature"
    in

    List.to_seq prog
    |> Seq.map check_item
    |> Core.Item_map.of_seq

end
