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
    | Let of def * t
    | Ann of t * Ty.t
    | I32 of int32
    | If_then_else of t * t * t
    | Infix of [`Eq | `Add | `Sub | `Mul] * t * t
    | Prefix of [`Neg] * t

  and pattern =
    string option Spanned.t

  and param =
    pattern * Ty.t

  and def =
    pattern * Ty.t option * t

end

module Item = struct

  type t =
    | Val of binder * Ty.t * Expr.t
    | Fun of binder * Expr.param Iarray.t * Ty.t * Expr.t

  and binder =
    string Spanned.t

  let name item =
    match item with
    | Val (name, _, _) -> name
    | Fun (name, _, _, _) -> name

end

module Program = struct

  type t = Item.t list

end

(** Elaboration from the surface language into the core language *)
module Elab : sig

  val check_program : Program.t -> (Core.Program.t, Span.t * string) result

end = struct

  (** {2 Elaboration context} *)

  module Env = struct

    type item_decl =
      | Val of Core.Ty.t
      | Fun of Core.Ty.t Iarray.t * Core.Ty.t

    type t = {
      item_decls : item_decl Core.Item_map.t;
      local_names : string option Core.Local.Env.t;
      local_tys : Core.Ty.t Core.Local.Env.t;
    }

    let empty : t = {
      item_decls = Core.Item_map.empty;
      local_names = Core.Local.Env.empty;
      local_tys = Core.Local.Env.empty;
    }

    let add_local (env : t) (name : string option) (ty : Core.Ty.t) : t =
      { env with
        local_names = Core.Local.Env.extend name env.local_names;
        local_tys = Core.Local.Env.extend ty env.local_tys;
      }

    let add_item (env : t) (name : string) (decl : item_decl) : t =
      let name = Core.Item_name.make name in
      { env with
        item_decls = env.item_decls |> Core.Item_map.add name decl;
      }

    let lookup_item (env : t) (name : string) : (Core.Item_name.t * item_decl) option =
      let name = Core.Item_name.make name in
      Core.Item_map.find_opt name env.item_decls
      |> Option.map (fun item -> name, item)

    let lookup_local (env : t) (name : string) : (Core.Expr.t * Core.Ty.t) option =
      match Core.Local.Env.find_index ((=) (Some name)) env.local_names with
      | Some index -> Some (Core.(Expr.Var index, Local.Env.lookup index env.local_tys))
      | None when name = "true" -> Some (Core.(Expr.Bool true, Ty.Bool))
      | None when name = "false" -> Some (Core.(Expr.Bool false, Ty.Bool))
      | None -> None

    let lookup (env : t) (name : string) : [
      | `Item of  Core.Item_name.t * item_decl
      | `Expr of Core.Expr.t * Core.Ty.t
    ] option =
      match lookup_item env name with
      | Some (name, item) -> Some (`Item (name, item))
      | None ->
          match lookup_local env name with
          | Some (expr, ty) -> Some (`Expr (expr, ty))
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

    | Expr.Let (def, body) ->
        let env, def = check_def env def in
        let body, ty = infer_expr env body in
        Core.(Expr.Let (def, body), ty)

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
    | Expr.Let (def, body) ->
        let env, def = check_def env def in
        let body = check_expr env body ty in
        Core.Expr.Let (def, body)

    | Expr.If_then_else (expr1, expr2, expr3) ->
        let expr1 = check_expr env expr1 Core.Ty.Bool in
        let expr2 = check_expr env expr2 ty in
        let expr3 = check_expr env expr3 ty in
        Core.Expr.Bool_if (expr1, expr2, expr3)

    | _ ->
        let expr', found_ty = infer_expr env expr in
        unify_tys expr.span ~found:found_ty ~expected:ty;
        expr'

  and check_def (env : Env.t) ((name, ty, expr) : Expr.def) : Env.t * Core.Expr.def =
    let expr, ty =
      match ty with
      | Some ty ->
          let ty = check_ty ty in
          check_expr env expr ty, ty
      | None ->
          infer_expr env expr
    in
    Env.add_local env name.data ty, (name.data, ty, expr)

  let check_program (prog : Program.t) : Core.Program.t =
    let check_item_decl (env : Env.t) (item : Item.t) : Env.t =
      let name = Item.name item in
      if Option.is_some (Env.lookup_item env name.data) then
        error name.span "item name already used";

      match item with
      | Item.Val (_, ty, _) ->
          Env.add_item env name.data (Env.Val (check_ty ty))
      | Item.Fun (_, params, ty, _) ->
          (* FIXME: Check duplicate parameter names *)
          let params = Iarray.map (fun (_, ty) -> check_ty ty) params in
          Env.add_item env name.data (Env.Fun (params, check_ty ty))

    and check_item (env : Env.t) (item : Item.t) : Core.Item_name.t * Core.Item.t =
      match item, Env.lookup_item env (Item.name item).data with
      | Item.Val (_, _, expr), Some (name, Env.Val ty) ->
          name, Core.Item.Val (ty, check_expr env expr ty)
      | Item.Fun (_, params, _, expr), Some (name, Env.Fun (param_tys, ty))
        when Iarray.(length params = length param_tys) ->
          (* FIXME: Check duplicate parameter names *)
          let params = Iarray.map2 (fun (name, _ : Expr.param) ty -> name.data, ty) params param_tys in
          let env = Iarray.fold_right (fun (name, ty) env -> Env.add_local env name ty) params env in
          let expr = check_expr env expr ty in
          name, Core.Item.Fun (params, ty, expr)
      | _, _ ->
          failwith "item does not match item signature"
    in

    let env = List.fold_left check_item_decl Env.empty prog in

    List.to_seq prog
    |> Seq.map (check_item env)
    |> Core.Item_map.of_seq


  (** {2 Public API} *)

  let check_program (prog : Program.t) : (Core.Program.t, Span.t * string) result =
    try Ok (check_program prog) with
    | Error (span, msg) -> Error (span, msg)

end
