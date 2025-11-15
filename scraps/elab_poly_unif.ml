(** An implementation of elaboration for a polymorphic functional language
    with explicit type parameters (similar to languages like Rust, Typescript,
    C#, and Java).

    Compare this with [elab_poly_algorithm_j], which implements generalisation.
*)

module Core = struct

  module Ty = struct

    (** Names of bound type variables *)
    type name = string

    (** Types *)
    type t =
      | Var of name         (* Type variables (bound by type parameters) *)
      | Meta of meta        (* Metavariables (used for unification) *)
      | Fun of t * t
      | Unit
      | Bool

    (** Mutable representation of metavariables, to be updated in-place during
        unification and typechecking *)
    and meta = meta_state ref

    (** Identifier used for pretty printing metavariables. *)
    and meta_id = int

    (** The current state of a metavariable. *)
    and meta_state =
      | Solved of t
      | Unsolved of meta_id

    (** Create a fresh, unsolved metavariable at a given level in the type
        environment *)
    let fresh_meta : unit -> meta =
      let next = ref 0 in
      fun () ->
        let id = !next in
        incr next;
        ref (Unsolved id)

    (** Replace bound variables in a type *)
    let rec subst (ty : t) (mapping : (string * t) list) : t =
      match ty with
      | Var name -> List.assoc_opt name mapping |> Option.value ~default:ty
      | Meta { contents = Solved ty } -> subst ty mapping
      | Meta { contents = Unsolved _ } -> ty
      | Fun (param_ty, body_ty) -> Fun (subst param_ty mapping, subst body_ty mapping)
      | Unit -> Unit
      | Bool -> Bool

    exception Mismatched_types of t * t
    exception Infinite_type

    (** Ensure that the candidate type does not refer to the to-be-solved
        metavariable, and raise the level of metavariables as required. *)
    let rec occurs (m : meta) (ty : t) =
      match ty with
      | Var _ -> ()
      | Fun (param_ty, body_ty) ->
          occurs m param_ty;
          occurs m body_ty
      | Unit -> ()
      | Bool -> ()
      | Meta m' when m == m' -> raise Infinite_type
      | Meta { contents = Solved ty } -> occurs m ty
      | Meta { contents = Unsolved _ } -> ()

    let rec unify (ty1 : t) (ty2 : t) =
      match ty1, ty2 with
      | Var name1, Var name2 when name1 = name2 -> ()
      | Meta m1, Meta m2 when m1 == m2 -> ()    (* NOTE: using pointer equality for references *)
      | Unit, Unit -> ()
      | Bool, Bool -> ()
      | Fun (param_ty1, body_ty1), Fun (param_ty2, body_ty2) ->
          unify param_ty1 param_ty2;
          unify body_ty1 body_ty2

      (* Unify through solved metavariables *)
      | Meta { contents = Solved ty1 }, ty2 -> unify ty1 ty2
      | ty1, Meta { contents = Solved ty2 } -> unify ty1 ty2

      (* Update unsolved metavariables in-place *)
      | Meta ({ contents = Unsolved _ } as m), ty
      | ty, Meta ({ contents = Unsolved _ } as m) ->
          occurs m ty;
          m := Solved ty

      | _, _ -> raise (Mismatched_types (ty1, ty2))

    (** Inline solved metavariables in types *)
    let rec zonk (ty : t) : t =
      match ty with
      | Var _ -> ty
      | Meta { contents = Solved ty } -> zonk ty
      | Meta { contents = Unsolved _ } -> ty
      | Fun (param_ty, body_ty) -> Fun (zonk param_ty, zonk body_ty)
      | Unit -> ty
      | Bool -> ty

  end

  module Expr = struct

    type t =
      | Var of string * Ty.t list
      (** A variable that is possibly applied to a series of type arguments
          (which will be applied a type lambda in a definition). *)

      | Let of string * Ty.name list * Ty.t * t * t
      (** Let bindings that are polymorphic over a series of type parameters.
          The type parameters are bound in a forall in the type annotation, and
          in a type lambda in the definition. In System F this would look like:

          {@text[
            let foo : ∀ a₁ ... aₙ. t :=
              Λ a₁ ... aₙ. e₁
            in e₂
          ]}
      *)

      | Fun_lit of string * Ty.t * t
      | Fun_app of t * t
      | Unit_lit
      | Bool_lit of bool
      | Bool_if of t * t * t

    let rec zonk (expr : t) : t =
      match expr with
      | Var (name, ty_args) -> Var (name, List.map Ty.zonk ty_args)
      | Let (name, ty_params, def_ty, def, body) ->
          Let (name, ty_params, Ty.zonk def_ty, zonk def, zonk body)
      | Fun_lit (name, ty, body) ->
          Fun_lit (name, Ty.zonk ty, zonk body)
      | Fun_app (fn, arg) ->
          Fun_app (zonk fn, zonk arg)
      | Unit_lit -> expr
      | Bool_lit _ -> expr
      | Bool_if (pred, if_true, if_false) ->
          Bool_if (zonk pred, zonk if_true, zonk if_false)

  end

end


module Surface = struct

  module Ty = struct

    type t =
      | Fun of t * t
      | Name of string
      | Placeholder
      [@@warning "-unused-constructor"]

  end

  module Expr = struct

    type t =
      | Name of string * Ty.t list
      | Let of string * string list * Ty.t option * t * t
      | Ann of t * Ty.t
      | Fun of string * Ty.t option * t
      | App of t * t
      | Unit
      | Bool of bool
      | If of t * t * t
      [@@warning "-unused-constructor"]

  end

  (** The main type checking algorithm *)
  module Elab : sig

    val check_ty : Ty.t -> (Core.Ty.t, string) result [@@warning "-unused-value-declaration"]
    val check_expr : Expr.t -> Core.Ty.t -> (Core.Expr.t, string) result [@@warning "-unused-value-declaration"]
    val infer_expr : Expr.t -> (Core.Expr.t * Core.Ty.t, string) result

  end = struct

    (** Error handling *)

    exception Error of string

    let error (type a b) (f : (b, Format.formatter, unit, a) format4) : b =
      Format.kasprintf (fun msg -> raise (Error msg)) f

    type poly_ty = Core.Ty.name list * Core.Ty.t


    (** Elaboration contexts *)
    module Ctx : sig

      type t

      val create : unit -> t

      val fresh_meta : t -> Core.Ty.t

      val extend_tys : t -> string list -> t
      val extend_expr : t -> string -> poly_ty -> t

      val lookup_ty : t -> string -> unit option
      val lookup_expr : t -> string -> poly_ty option

      val has_unsolved_metas : t -> bool

    end = struct

      type t = {
        metas : Core.Ty.meta Dynarray.t;
        (* NOTE: We could provide additional information alongside metavariables
                 to provide a better ambiguity error at the end of typechecking.
                 See [elab-stlc-unification] for an example of this. *)

        ty_names : string list;
        expr_tys : (string * poly_ty) list;
      }

      let create () = {
        metas = Dynarray.create ();
        ty_names = [];
        expr_tys = [];
      }

      let fresh_meta (ctx : t) : Core.Ty.t =
        let meta = Core.Ty.fresh_meta () in
        Dynarray.add_last ctx.metas meta;
        Core.Ty.Meta meta

      let extend_tys (ctx : t) (names : string list) : t =
        { ctx with ty_names = List.rev_append names ctx.ty_names }

      let extend_expr (ctx : t) (name : string) (ty_params, ty : poly_ty) : t =
        { ctx with expr_tys = (name, (ty_params, ty)) :: ctx.expr_tys }

      let lookup_ty (ctx : t) (name : string) : unit option =
        if List.mem name ctx.ty_names then Some () else None

      let lookup_expr (ctx : t) (name : string) : poly_ty option =
        List.assoc_opt name ctx.expr_tys

      let has_unsolved_metas (ctx : t) : bool =
        ctx.metas |> Dynarray.exists @@ function
          | { contents = Core.Ty.Unsolved _ } -> true
          | { contents = Core.Ty.Solved _ } -> false

    end

    let unify_tys (ty1 : Core.Ty.t) (ty2 : Core.Ty.t) =
      try Core.Ty.unify ty1 ty2 with
      | Core.Ty.Mismatched_types _ -> error "type mismatch"
      | Core.Ty.Infinite_type -> error "infinite type"


    (* Bidirectional elaboration *)

    let rec check_ty (ctx : Ctx.t) (ty : Ty.t) : Core.Ty.t =
      match ty with
      | Ty.Name name ->
          begin match Ctx.lookup_ty ctx name with
          | Some () -> Core.Ty.Var name
          | None when name = "Bool" -> Core.Ty.Bool
          | None when name = "Unit" -> Core.Ty.Unit
          | None -> error "unbound type variable `%s`" name
          end
      | Ty.Fun (ty1, ty2) -> Core.Ty.Fun (check_ty ctx ty1, check_ty ctx ty2)
      | Ty.Placeholder -> Ctx.fresh_meta ctx

    let rec check_expr (ctx : Ctx.t) (expr : Expr.t) (ty : Core.Ty.t) : Core.Expr.t =
      match expr, ty with
      | Expr.Let (name, ty_params, def_ty, def, body), body_ty ->
          let def, def_ty = infer_def ctx ty_params def_ty def in
          let body = check_expr (Ctx.extend_expr ctx name (ty_params, def_ty)) body body_ty in
          Core.Expr.Let (name, ty_params, def_ty, def, body)

      | Expr.Fun (name, None, body), Core.Ty.Fun (param_ty', body_ty) ->
          let body = check_expr (Ctx.extend_expr ctx name ([], param_ty')) body body_ty in
          Core.Expr.Fun_lit (name, param_ty', body)

      | Expr.Fun (name, Some param_ty, body), Core.Ty.Fun (param_ty', body_ty) ->
          let param_ty = check_ty ctx param_ty in
          unify_tys param_ty param_ty';
          let body = check_expr (Ctx.extend_expr ctx name ([], param_ty')) body body_ty in
          Core.Expr.Fun_lit (name, param_ty, body)

      | Expr.If (pred, if_true, if_false), body_ty ->
          let pred = check_expr ctx pred Core.Ty.Bool in
          let if_true = check_expr ctx if_true body_ty in
          let if_false = check_expr ctx if_false body_ty in
          Core.Expr.Bool_if (pred, if_true, if_false)

      | expr, ty ->
          let expr, found_ty = infer_expr ctx expr in
          unify_tys ty found_ty;
          expr

    and infer_expr (ctx : Ctx.t) (expr : Expr.t) : Core.Expr.t * Core.Ty.t =
      match expr with
      | Expr.Name (name, ty_args) ->
          begin match Ctx.lookup_expr ctx name, ty_args with
          | Some (ty_params, ty), [] ->
              let mapping = ty_params |> List.map (fun name -> name, Ctx.fresh_meta ctx) in
              Core.Expr.Var (name, List.map snd mapping), Core.Ty.subst ty mapping

          | Some (ty_params, ty), ty_args when List.length ty_params = List.length ty_args ->
              let ty_args = List.map (check_ty ctx) ty_args in
              let mapping = List.combine ty_params ty_args in
              Core.Expr.Var (name, ty_args), Core.Ty.subst ty mapping

          | Some (_, _), _ -> error "mismatched type parameter list";
          | None, _ -> error "unbound variable `%s`" name
          end

      | Expr.Let (name, ty_params, def_ty, def, body) ->
          let def, def_ty = infer_def ctx ty_params def_ty def in
          let body, body_ty = infer_expr (Ctx.extend_expr ctx name (ty_params, def_ty)) body in
          Core.Expr.Let (name, ty_params, def_ty, def, body), body_ty

      | Expr.Ann (tm, ty) ->
          let ty = check_ty ctx ty in
          check_expr ctx tm ty, ty

      | Expr.Fun (name, None, body) ->
          let param_ty = Ctx.fresh_meta ctx in
          let body, body_ty = infer_expr (Ctx.extend_expr ctx name ([], param_ty)) body in
          Core.Expr.Fun_lit (name, param_ty, body), Core.Ty.Fun (param_ty, body_ty)

      | Expr.Fun (name, Some param_ty, body) ->
          let param_ty = check_ty ctx param_ty in
          let body, body_ty = infer_expr (Ctx.extend_expr ctx name ([], param_ty)) body in
          Core.Expr.Fun_lit (name, param_ty, body), Core.Ty.Fun (param_ty, body_ty)

      | Expr.App (fn, arg) ->
          let fn, fn_ty = infer_expr ctx fn in
          let arg, arg_ty = infer_expr ctx arg in
          let body_ty = Ctx.fresh_meta ctx in
          unify_tys fn_ty (Core.Ty.Fun (arg_ty, body_ty));
          Core.Expr.Fun_app (fn, arg), body_ty

      | Expr.Unit ->
          Core.Expr.Unit_lit, Core.Ty.Unit

      | Expr.Bool b ->
          Core.Expr.Bool_lit b, Core.Ty.Bool

      | Expr.If (pred, if_true, if_false) ->
          let pred = check_expr ctx pred Core.Ty.Bool in
          let if_true, if_true_ty = infer_expr ctx if_true in
          let if_false, if_false_ty = infer_expr ctx if_false in
          unify_tys if_true_ty if_false_ty;
          Core.Expr.Bool_if (pred, if_true, if_false), if_true_ty

    and infer_def ctx ty_params def_ty def =
      if List.length ty_params <> List.length (List.sort_uniq String.compare ty_params) then
        error "type parameter names must be unique";

      match def_ty with
      | None -> infer_expr (Ctx.extend_tys ctx ty_params) def
      | Some def_ty ->
          let def_ty = check_ty (Ctx.extend_tys ctx ty_params) def_ty in
          check_expr (Ctx.extend_tys ctx ty_params) def def_ty, def_ty


    (** Running elaboration *)

    let run (type a) (prog : Ctx.t -> a) : (a, string) result =
      let ctx = Ctx.create () in
      match prog ctx with
      | x when not (Ctx.has_unsolved_metas ctx) -> Ok x
      | _ -> Error "unsolved metavariables"
      | exception Error msg -> Error msg


    (** Public API *)

    let check_ty (ty : Ty.t) : (Core.Ty.t, string) result =
      run @@ fun ctx ->
        Core.Ty.zonk (check_ty ctx ty)

    let check_expr (expr : Expr.t) (ty : Core.Ty.t) : (Core.Expr.t, string) result =
      run @@ fun ctx ->
        Core.Expr.zonk (check_expr ctx expr ty)

    let infer_expr (expr : Expr.t) : (Core.Expr.t * Core.Ty.t, string) result =
      run @@ fun ctx ->
        let expr, ty = infer_expr ctx expr in
        Core.Expr.zonk expr, Core.Ty.zonk ty

  end

end


let () = begin

  Printexc.record_backtrace true;

  print_string "Running tests ...";

  let open Surface in

  let ( $ ) f x = Expr.App (f, x) in

  let expect_ok result =
    match result with
    | Ok x -> x
    | Error msg -> failwith msg
  in

  begin

    (* Polymorphic identity function *)
    let expr =
      Expr.Let ("id", ["a"], None,
        Fun ("x", Some (Name "a"), Name ("x", [])),
        Name ("id", []) $ Unit)
    in
    assert (Elab.infer_expr expr |> expect_ok = Core.(
      Expr.Let ("id", ["a"], Ty.Fun (Var "a", Var "a"),
        Fun_lit ("x", Ty.Var "a", Var ("x", [])),
        Fun_app (Var ("id", [Ty.Unit]), Unit_lit)),
      Ty.Unit
    ));

    (* Explicit type application *)
    let expr =
      Expr.Let ("id", ["a"], None,
        Fun ("x", Some (Name "a"), Name ("x", [])),
        Name ("id", [Name "Unit"]) $ Unit)
    in
    assert (Elab.infer_expr expr |> expect_ok = Core.(
      Expr.Let ("id", ["a"], Ty.Fun (Var "a", Var "a"),
        Fun_lit ("x", Ty.Var "a", Var ("x", [])),
        Fun_app (Var ("id", [Ty.Unit]), Unit_lit)),
      Ty.Unit
    ));

    (* Constant function *)
    let expr =
      Expr.Let ("id", ["a"], None,
        Fun ("x", Some (Name "a"), Name ("x", [])),
        Let ("const", ["a"; "b"], None,
          Fun ("x", Some (Name "a"), Fun ("y", Some (Name "b"), Name ("x", []))),
          Name ("const", []) $ Unit $ (Name ("id", []) $ Bool true)))
    in
    assert (Elab.infer_expr expr |> expect_ok = Core.(
      let ( $ ) f x = Expr.Fun_app (f, x) in
      Expr.Let ("id", ["a"], Ty.Fun (Var "a", Var "a"),
        Fun_lit ("x", Ty.Var "a", Var ("x", [])),
        Expr.Let ("const", ["a"; "b"], Ty.Fun (Var "a", Ty.Fun (Var "b", Var "a")),
          Fun_lit ("x", Ty.Var "a", Fun_lit ("y", Ty.Var "b", Var ("x", []))),
          Var ("const", [Ty.Unit; Ty.Bool]) $ Unit_lit $ (Var ("id", [Ty.Bool]) $ Bool_lit true))),
      Ty.Unit
    ));

    (* TODO: More tests *)

  end;

  print_string " ok!\n";

end
