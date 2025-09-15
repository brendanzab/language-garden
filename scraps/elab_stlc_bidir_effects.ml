(** An elaborator that uses effects to keep a record of local bindings.

    Extends [elab_stlc_bidir.ml].

    - Conor McBride, {{: https://github.com/pigworker/Samizdat/blob/main/EffW.hs}
      An Effects-and-Handlers Implementation of Hindley-Milner Typechecking}
    - Andrej Bauer, {{: https://github.com/andrejbauer/faux-type-theory/tree/main/algebraic-fauxtt}
      faux-type-theory/algebraic-fauxtt}
    - Andrej Bauer, {{: https://github.com/andrejbauer/faux-type-theory/blob/main/slides/PL-for-PA-lecture-4-handout.pdf}
      Lecture 4: Variables as Computational Effects}
*)

[@@@warning "-unused-constructor"]
[@@@warning "-unused-value-declaration"]

(** Core language *)
module Core = struct

  (** Syntax *)

  type ty =
    | Unit_ty : ty
    | Fun_ty : ty * ty -> ty

  type index = int
  type level = int

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

  (* Effects *)

  exception Unbound_var of string
  exception Type_mismatch
  exception Ambiguous_param of string

  type _ Effect.t +=
    | Size : Core.level Effect.t
    | Lookup : string -> (Core.level * Core.ty) option Effect.t

  let size () = Effect.perform Size
  let lookup name = Effect.perform (Lookup name)

  let with_empty (type a) (prog : unit -> a) : a =
    try prog () with
    | effect Size, k -> Effect.Deep.continue k 0
    | effect Lookup _, k -> Effect.Deep.continue k None

  let with_var (type a) (name : string) (ty : Core.ty) (prog : unit -> a) : a =
    try prog () with
    | effect Size, k -> Effect.Deep.continue k (size () + 1)
    | effect Lookup n, k when n = name -> Effect.Deep.continue k (Some (size (), ty))

  (** Elaboration *)

  let rec elab_ty : ty -> Core.ty =
    function
    | Name "Unit" -> Unit_ty
    | Name name -> raise (Unbound_var name)
    | Fun_ty (arg_ty, body_ty) ->
        let arg_ty = elab_ty arg_ty in
        let body_ty = elab_ty body_ty in
        Fun_ty (arg_ty, body_ty)

  let rec check_expr : expr -> Core.ty -> Core.expr =
    fun expr expected_ty ->
      match expr, expected_ty with
      | Let ((name, def_ty, def_expr), body_expr), body_ty ->
          let def_expr, def_ty = elab_ann_expr def_expr def_ty in
          with_var name def_ty @@ fun () ->
            let body_expr = check_expr body_expr body_ty in
            Core.Let (def_ty, def_expr, body_expr)
      | Fun_lit ((name, None), body_expr), Core.Fun_ty (param_ty, body_ty) ->
          with_var name param_ty @@ fun () ->
            let body_expr = check_expr body_expr body_ty in
            Core.Fun_lit (param_ty, body_expr)
      | _ ->
          let expr, ty = synth_expr expr in
          if ty = expected_ty then expr else
            raise Type_mismatch

  and synth_expr : expr -> Core.expr * Core.ty =
    fun expr ->
      match expr with
      | Name name ->
          begin match lookup name with
          | Some (level, ty) -> Core.Var (size () - level - 1), ty
          | None when name = "unit" -> Core.Unit_lit, Core.Unit_ty
          | None -> raise (Unbound_var name)
          end
      | Ann (expr, ty) ->
          elab_ann_expr expr (Some ty)
      | Let ((name, def_ty, def_expr), body_expr) ->
          let def_expr, def_ty = elab_ann_expr def_expr def_ty in
          with_var name def_ty @@ fun () ->
            let body_expr, body_ty = synth_expr body_expr in
            Core.Let (def_ty, def_expr, body_expr), body_ty
      | Fun_lit ((name, Some param_ty), body_expr) ->
          let param_ty = elab_ty param_ty in
          with_var name param_ty @@ fun () ->
            let body_expr, body_ty = synth_expr body_expr in
            Core.Fun_lit (param_ty, body_expr), Core.Fun_ty (param_ty, body_ty)
      | Fun_lit ((name, None), _) ->
          raise (Ambiguous_param name)
      | Fun_app (fn_expr, arg_expr) ->
          begin match synth_expr fn_expr with
          | fn_expr, Fun_ty (param_ty, body_ty) ->
              let arg_expr = check_expr arg_expr param_ty in
              Core.Fun_app (fn_expr, arg_expr), body_ty
          | _ -> raise Type_mismatch
          end

  and elab_ann_expr : expr -> ty option -> Core.expr * Core.ty =
    fun expr ty ->
      match ty with
      | Some ty ->
          let ty = elab_ty ty in
          check_expr expr ty, ty
      | None -> synth_expr expr

  let check_expr : expr -> Core.ty -> Core.expr =
    fun expr expected_ty ->
      with_empty @@ fun () ->
        check_expr expr expected_ty

  let synth_expr : expr -> Core.expr * Core.ty =
    fun expr ->
      with_empty @@ fun () ->
        synth_expr expr

end

let () = begin

  print_string "Running tests ...";

  assert (Surface.synth_expr (Fun_lit (("x", Some (Name "Unit")), Name "x"))
    = (Fun_lit (Unit_ty, Var 0), Fun_ty (Unit_ty, Unit_ty)));

  assert (Surface.synth_expr (Fun_lit (("x", Some (Name "Unit")), Fun_lit (("y", Some (Name "Unit")), Name "x")))
    = (Fun_lit (Unit_ty, Fun_lit (Unit_ty, Var 1)), Fun_ty (Unit_ty, Fun_ty (Unit_ty, Unit_ty))));

  assert (Surface.synth_expr (Fun_lit (("x", Some (Name "Unit")), Fun_lit (("x", Some (Name "Unit")), Name "x")))
    = (Fun_lit (Unit_ty, Fun_lit (Unit_ty, Var 0)), Fun_ty (Unit_ty, Fun_ty (Unit_ty, Unit_ty))));

  assert (Surface.check_expr (Fun_lit (("x", None), Name "x")) (Fun_ty (Unit_ty, Unit_ty))
    = Fun_lit (Unit_ty, Var 0));

  print_string " ok!\n";

end
