[@@@warning "-unused-constructor"]
[@@@warning "-unused-value-declaration"]

(** Core language *)
module Core = struct

  (** Syntax *)

  type ty =
    | Unit_ty : ty
    | Fun_ty : ty * ty -> ty

  type index = int

  type expr =
    | Var : index -> expr
    | Let : ty * expr * expr -> expr
    | Fun_lit : ty * expr -> expr
    | Fun_app : expr * expr -> expr
    | Unit_lit : expr

end

(** Surface language *)
module Surface = struct

  (** Syntax *)

  type ty =
    | Name of string
    | Fun_ty of ty * ty

  type expr =
    | Name : string -> expr
    | Ann : expr * ty -> expr
    | Let : (string * ty option * expr) * expr -> expr
    | Fun_lit : (string * ty option) * expr -> expr
    | Fun_app : expr * expr -> expr
    | Unit_lit : expr

  (** Elaboration *)

  exception Unbound_var of string
  exception Type_mismatch
  exception Ambiguous_param of string

  let rec elab_ty : ty -> Core.ty =
    function
    | Name "Unit" -> Unit_ty
    | Name name -> raise (Unbound_var name)
    | Fun_ty (arg_ty, body_ty) ->
        let arg_ty = elab_ty arg_ty in
        let body_ty = elab_ty body_ty in
        Fun_ty (arg_ty, body_ty)

  let rec check_expr : (string * Core.ty) list -> expr -> Core.ty -> Core.expr =
    fun env expr expected_ty ->
      match expr, expected_ty with
      | Let ((name, def_ty, def_expr), body_expr), body_ty ->
          let def_expr, def_ty = elab_ann_expr env def_expr def_ty in
          let body_expr = check_expr ((name, def_ty) :: env) body_expr body_ty in
          Let (def_ty, def_expr, body_expr)
      | Fun_lit ((name, None), body_expr), Fun_ty (param_ty, body_ty) ->
          let body_expr = check_expr ((name, param_ty) :: env) body_expr body_ty in
          Fun_lit (param_ty, body_expr)
      | _ ->
          let expr, ty = synth_expr env expr in
          if ty = expected_ty then expr else
            raise Type_mismatch

  and synth_expr : (string * Core.ty) list -> expr -> Core.expr * Core.ty =
    fun env expr ->
      match expr with
      | Name name ->
          begin match env |> List.find_mapi (fun i (n, ty) -> if n = name then Some (i, ty) else None) with
          | Some (index, ty) -> Core.Var index, ty
          | None when name = "unit" -> Core.Unit_lit, Core.Unit_ty
          | None -> raise (Unbound_var name)
          end
      | Ann (expr, ty) ->
          elab_ann_expr env expr (Some ty)
      | Let ((name, def_ty, def_expr), body_expr) ->
          let def_expr, def_ty = elab_ann_expr env def_expr def_ty in
          let body_expr, body_ty = synth_expr ((name, def_ty) :: env) body_expr in
          Let (def_ty, def_expr, body_expr), body_ty
      | Fun_lit ((name, Some param_ty), body_expr) ->
          let param_ty = elab_ty param_ty in
          let body_expr, body_ty = synth_expr ((name, param_ty) :: env) body_expr in
          Fun_lit (param_ty, body_expr), Fun_ty (param_ty, body_ty)
      | Fun_lit ((name, None), _) ->
          raise (Ambiguous_param name)
      | Fun_app (fn_expr, arg_expr) ->
          begin match synth_expr env fn_expr with
          | fn_expr, Fun_ty (param_ty, body_ty) ->
              let arg_expr = check_expr env arg_expr param_ty in
              Fun_app (fn_expr, arg_expr), body_ty
          | _ -> raise Type_mismatch
          end
      | Unit_lit ->
          Unit_lit, Unit_ty

  and elab_ann_expr : (string * Core.ty) list -> expr -> ty option -> Core.expr * Core.ty =
    fun env expr ty ->
      match ty with
      | Some ty ->
          let ty = elab_ty ty in
          check_expr env expr ty, ty
      | None -> synth_expr env expr

end

let () = begin

  print_string "Running tests ...";

  assert (Surface.synth_expr [] (Fun_lit (("x", Some (Name "Unit")), Name "x"))
    = (Fun_lit (Unit_ty, Var 0), Fun_ty (Unit_ty, Unit_ty)));

  assert (Surface.synth_expr [] (Fun_lit (("x", Some (Name "Unit")), Fun_lit (("y", Some (Name "Unit")), Name "x")))
    = (Fun_lit (Unit_ty, Fun_lit (Unit_ty, Var 1)), Fun_ty (Unit_ty, Fun_ty (Unit_ty, Unit_ty))));

  assert (Surface.synth_expr [] (Fun_lit (("x", Some (Name "Unit")), Fun_lit (("x", Some (Name "Unit")), Name "x")))
    = (Fun_lit (Unit_ty, Fun_lit (Unit_ty, Var 0)), Fun_ty (Unit_ty, Fun_ty (Unit_ty, Unit_ty))));

  assert (Surface.check_expr [] (Fun_lit (("x", None), Name "x")) (Fun_ty (Unit_ty, Unit_ty))
    = Fun_lit (Unit_ty, Var 0));

  print_string " ok!\n";

end
