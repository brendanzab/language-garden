(** An implementation of elaboration for a polymorphic functional language
    with explicit type parameters (similar to languages like Rust, Typescript,
    C#, and Java).

    This supports typechecking expressions like:

    {@text[
      let id [A] (x : A) := x;
      let const [A, B] (x : A) (y : B) := x;
      const unit (id true)
    ]}

    and explicit type applications like:

    {@text[
      let id [A] (x : A) := x;
      let const [A, B] (x : A) (y : B) := x;
      const [Unit, Int] unit (id [Bool] true)
    ]}

    This is what I think most people are asking for when they ask “how do I
    implement generics”. This is in contrast to Hindley Milner type systems that
    implement generalisation (see [check_poly_algorithm_j.ml] and
    [elab_poly_algorithm_j.ml]).

    {2 Future work}

    - Recursive definitions (see [elab-stlc-letrec-unification])
    - Datatype definitions (see [misc_local_datatypes.ml] and {{: https://doi.org/10.1145/3729338}
      Practical Type Inference with Levels})
    - Type aliases
*)

(** The explicitly typed core language that the surface language will be
    elaborated to. *)
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
        metavariable *)
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

    (** Check that two types are the same, wile updating unsolved metavariables
        with known type information as required. *)
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

    let pp (ty : t) (ppf : Format.formatter) : unit =
      let rec pp_ty ty ppf =
        match ty with
        | Meta { contents = Solved ty } -> pp_ty ty ppf
        | Fun (param_ty, body_ty) ->
            Format.fprintf ppf "%t -> %t"
              (pp_atomic_ty param_ty)
              (pp_ty body_ty)
        | ty ->
            pp_atomic_ty ty ppf
      and pp_atomic_ty ty ppf =
        match ty with
        | Var name -> Format.fprintf ppf "%s" name
        | Meta { contents = Solved ty } -> pp_atomic_ty ty ppf
        | Meta { contents = Unsolved id } -> Format.fprintf ppf "$%i" id
        | Unit -> Format.fprintf ppf "Unit"
        | Bool -> Format.fprintf ppf "Bool"
        | Fun _ as ty -> Format.fprintf ppf "@[(%t)@]" (pp_ty ty)
      in
      pp_ty ty ppf

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
      | Fun_app (head, arg) ->
          Fun_app (zonk head, zonk arg)
      | Unit_lit -> expr
      | Bool_lit _ -> expr
      | Bool_if (head, true_body, false_body) ->
          Bool_if (zonk head, zonk true_body, zonk false_body)

  end

end


(** The surface language closely mirrors what the programmer originally wrote,
    including syntactic sugar and higher level language features that make
    programming more convenient. *)
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

  (** Elaboration of the surface language. This performs type checking and
      type-directed lowering to the core language (e.g. inserting explicit type
      annotations and explicit type applications). *)
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

      val fresh_meta : t -> string -> Core.Ty.t

      val extend_tys : t -> string list -> t
      val extend_expr : t -> string -> poly_ty -> t

      val lookup_ty : t -> string -> unit option
      val lookup_expr : t -> string -> poly_ty option

      val unsolved_metas : t -> string Seq.t

    end = struct

      type t = {
        metas : (Core.Ty.meta * string) Dynarray.t;
        ty_names : string list list;
        expr_tys : (string * poly_ty) list;
      }

      let create () = {
        metas = Dynarray.create ();
        ty_names = [];
        expr_tys = [];
      }

      let fresh_meta (ctx : t) (desc : string) : Core.Ty.t =
        let meta = Core.Ty.fresh_meta () in
        Dynarray.add_last ctx.metas (meta, desc);
        Core.Ty.Meta meta

      let extend_tys (ctx : t) (names : string list) : t =
        { ctx with ty_names = names :: ctx.ty_names }

      let extend_expr (ctx : t) (name : string) (ty_params, ty : poly_ty) : t =
        { ctx with expr_tys = (name, (ty_params, ty)) :: ctx.expr_tys }

      let lookup_ty (ctx : t) (name : string) : unit option =
        if List.exists (List.mem name) ctx.ty_names then Some () else None

      let lookup_expr (ctx : t) (name : string) : poly_ty option =
        List.assoc_opt name ctx.expr_tys

      let unsolved_metas (ctx : t) : string Seq.t =
        Dynarray.to_seq ctx.metas |> Seq.filter_map @@ function
          | { contents = Core.Ty.Unsolved _ }, desc -> Some desc
          | { contents = Core.Ty.Solved _ }, _ -> None

    end

    let unify_tys ~(found : Core.Ty.t) ~(expected : Core.Ty.t) =
      try Core.Ty.unify expected found with
      | Core.Ty.Mismatched_types _ ->
          error "expected: %t, found: %t" (Core.Ty.pp expected) (Core.Ty.pp found)
      | Core.Ty.Infinite_type ->
          error "infinite type"


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
      | Ty.Placeholder -> Ctx.fresh_meta ctx "placeholder"

    let rec check_expr (ctx : Ctx.t) (expr : Expr.t) (ty : Core.Ty.t) : Core.Expr.t =
      match expr, ty with
      | Expr.Let (name, ty_params, def_ty, def, body), body_ty ->
          let def, def_ty = infer_def ctx ty_params def_ty def in
          let body = check_expr (Ctx.extend_expr ctx name (ty_params, def_ty)) body body_ty in
          Core.Expr.Let (name, ty_params, def_ty, def, body)

      | Expr.Fun (name, None, body), Core.Ty.Fun (param_ty, body_ty) ->
          let body = check_expr (Ctx.extend_expr ctx name ([], param_ty)) body body_ty in
          Core.Expr.Fun_lit (name, param_ty, body)

      | Expr.Fun (name, Some param_ty, body), Core.Ty.Fun (param_ty', body_ty) ->
          let param_ty = check_ty ctx param_ty in
          unify_tys ~found:param_ty ~expected:param_ty';
          let body = check_expr (Ctx.extend_expr ctx name ([], param_ty')) body body_ty in
          Core.Expr.Fun_lit (name, param_ty, body)

      | Expr.If (head, true_body, false_body), body_ty ->
          let head = check_expr ctx head Core.Ty.Bool in
          let true_body = check_expr ctx true_body body_ty in
          let false_body = check_expr ctx false_body body_ty in
          Core.Expr.Bool_if (head, true_body, false_body)

      | expr, expected_ty ->
          let expr, found_ty = infer_expr ctx expr in
          unify_tys ~found:found_ty ~expected:expected_ty;
          expr

    and infer_expr (ctx : Ctx.t) (expr : Expr.t) : Core.Expr.t * Core.Ty.t =
      match expr with
      | Expr.Name (name, ty_args) ->
          begin match Ctx.lookup_expr ctx name, ty_args with
          | Some (ty_params, ty), [] ->
              let mapping = ty_params |> List.map (fun name -> name, Ctx.fresh_meta ctx "type argument") in
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
          let param_ty = Ctx.fresh_meta ctx "function parameter" in
          let body, body_ty = infer_expr (Ctx.extend_expr ctx name ([], param_ty)) body in
          Core.Expr.Fun_lit (name, param_ty, body), Core.Ty.Fun (param_ty, body_ty)

      | Expr.Fun (name, Some param_ty, body) ->
          let param_ty = check_ty ctx param_ty in
          let body, body_ty = infer_expr (Ctx.extend_expr ctx name ([], param_ty)) body in
          Core.Expr.Fun_lit (name, param_ty, body), Core.Ty.Fun (param_ty, body_ty)

      | Expr.App (head, arg) ->
          begin match infer_expr ctx head with
          | head, Core.Ty.Fun (param_ty, body_ty) ->
              let arg = check_expr ctx arg param_ty in
              Core.Expr.Fun_app (head, arg), body_ty
          | head, head_ty ->
              let arg, arg_ty = infer_expr ctx arg in
              let body_ty = Ctx.fresh_meta ctx "return type" in
              unify_tys ~found:head_ty ~expected:(Core.Ty.Fun (arg_ty, body_ty));
              Core.Expr.Fun_app (head, arg), body_ty
          end

      | Expr.Unit ->
          Core.Expr.Unit_lit, Core.Ty.Unit

      | Expr.Bool b ->
          Core.Expr.Bool_lit b, Core.Ty.Bool

      | Expr.If (head, true_body, false_body) ->
          let head = check_expr ctx head Core.Ty.Bool in
          let body_ty = Ctx.fresh_meta ctx "if branches" in
          let true_body = check_expr ctx true_body body_ty in
          let false_body = check_expr ctx false_body body_ty in
          Core.Expr.Bool_if (head, true_body, false_body), body_ty

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
      try
        let ctx = Ctx.create () in
        let result = prog ctx in
        Ctx.unsolved_metas ctx |> Seq.iter (error "ambiguous %s");
        Ok result
      with
      | Error msg -> Error msg


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
      Expr.Let ("id", ["A"], None,
        Fun ("x", Some (Name "A"), Name ("x", [])),
        Name ("id", []) $ Unit)
    in
    assert (Elab.infer_expr expr |> expect_ok = Core.(
      Expr.Let ("id", ["A"], Ty.Fun (Var "A", Var "A"),
        Fun_lit ("x", Ty.Var "A", Var ("x", [])),
        Fun_app (Var ("id", [Ty.Unit]), Unit_lit)),
      Ty.Unit
    ));

    (* Explicit type application *)
    let expr =
      Expr.Let ("id", ["A"], None,
        Fun ("x", Some (Name "A"), Name ("x", [])),
        Name ("id", [Name "Unit"]) $ Unit)
    in
    assert (Elab.infer_expr expr |> expect_ok = Core.(
      Expr.Let ("id", ["A"], Ty.Fun (Var "A", Var "A"),
        Fun_lit ("x", Ty.Var "A", Var ("x", [])),
        Fun_app (Var ("id", [Ty.Unit]), Unit_lit)),
      Ty.Unit
    ));

    (* Constant function *)
    let expr =
      Expr.Let ("id", ["A"], None,
        Fun ("x", Some (Name "A"), Name ("x", [])),
        Let ("const", ["A"; "B"], None,
          Fun ("x", Some (Name "A"), Fun ("y", Some (Name "B"), Name ("x", []))),
          Name ("const", []) $ Unit $ (Name ("id", []) $ Bool true)))
    in
    assert (Elab.infer_expr expr |> expect_ok = Core.(
      let ( $ ) f x = Expr.Fun_app (f, x) in
      Expr.Let ("id", ["A"], Ty.Fun (Var "A", Var "A"),
        Fun_lit ("x", Ty.Var "A", Var ("x", [])),
        Let ("const", ["A"; "B"], Ty.Fun (Var "A", Ty.Fun (Var "B", Var "A")),
          Fun_lit ("x", Ty.Var "A", Fun_lit ("y", Ty.Var "B", Var ("x", []))),
          Var ("const", [Ty.Unit; Ty.Bool]) $ Unit_lit $ (Var ("id", [Ty.Bool]) $ Bool_lit true))),
      Ty.Unit
    ));

    (* TODO: More tests *)

  end;

  print_string " ok!\n";

end
