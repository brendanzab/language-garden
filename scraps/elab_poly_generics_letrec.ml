(** Elaboration for a polymorphic functional language with explicit type
    parameters and general recursion.

    Extends [elab_poly_generics.ml].
*)

(** Returns a list of the duplicate elements in a list *)
let find_dupes (type a) (xs : a list) : a list =
  let rec go acc xs =
    match xs with
    | [] -> List.rev acc
    | x :: xs when List.mem x xs && not (List.mem x acc) -> go (x :: acc) xs
    | _ :: xs -> go acc xs
  in
  go [] xs

let uncons (type a) (xs : a list) : (a * a list) option =
  match xs with
  | [] -> None
  | x :: xs -> Some (x, xs)


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
      | Tuple of t list
      | Bool
      | Int

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

    (** Force any solved metavariables on the outermost part of a type. This is
        usually done before pattern matching on a type. *)
    let rec force (ty : t) : t =
      match ty with
      | Meta ({ contents = Solved ty } as m) ->
          let ty = force ty in
          m := Solved ty;
          ty
      | ty -> ty

    (** Replace bound variables in a type *)
    let rec subst (mapping : (string * t) list) (ty : t) : t =
      match ty with
      | Var name -> List.assoc_opt name mapping |> Option.value ~default:ty
      | Meta { contents = Solved ty } -> subst mapping ty
      | Meta { contents = Unsolved _ } -> ty
      | Fun (param_ty, body_ty) -> Fun (subst mapping param_ty, subst mapping body_ty)
      | Tuple elem_tys -> Tuple (List.map (subst mapping) elem_tys)
      | Bool -> Bool
      | Int -> Int

    exception Mismatched_types
    exception Infinite_type

    (** Ensure that the candidate type does not refer to the to-be-solved
        metavariable *)
    let rec occurs (m : meta) (ty : t) =
      match ty with
      | Var _ -> ()
      | Meta m' when m == m' -> raise Infinite_type
      | Meta { contents = Solved ty } -> occurs m ty
      | Meta { contents = Unsolved _ } -> ()
      | Fun (param_ty, body_ty) ->
          occurs m param_ty;
          occurs m body_ty
      | Tuple elem_tys ->
          List.iter (occurs m) elem_tys
      | Bool -> ()
      | Int -> ()

    (** Check that two types are the same, wile updating unsolved metavariables
        with known type information as required. *)
    let rec unify (ty1 : t) (ty2 : t) =
      match force ty1, force ty2 with
      | Var name1, Var name2 when name1 = name2 -> ()
      | Meta m1, Meta m2 when m1 == m2 -> ()    (* NOTE: using pointer equality for references *)
      | Fun (param_ty1, body_ty1), Fun (param_ty2, body_ty2) ->
          unify param_ty1 param_ty2;
          unify body_ty1 body_ty2
      | Tuple [], Tuple [] -> ()
      | Tuple (elem_ty1 :: elem_tys1), Tuple (elem_ty2 :: elem_tys2) ->
          unify elem_ty1 elem_ty2;
          unify (Tuple elem_tys1) (Tuple elem_tys2)
      | Bool, Bool -> ()
      | Int, Int -> ()

      (* Unify through solved metavariables *)
      | Meta { contents = Solved ty1 }, ty2 -> unify ty1 ty2
      | ty1, Meta { contents = Solved ty2 } -> unify ty1 ty2

      (* Update unsolved metavariables in-place *)
      | Meta ({ contents = Unsolved _ } as m), ty
      | ty, Meta ({ contents = Unsolved _ } as m) ->
          occurs m ty;
          m := Solved ty

      | _, _ -> raise Mismatched_types

    (** Inline solved metavariables in types *)
    let rec zonk (ty : t) : t =
      match ty with
      | Var _ -> ty
      | Meta { contents = Solved ty } -> zonk ty
      | Meta { contents = Unsolved _ } -> ty
      | Fun (param_ty, body_ty) -> Fun (zonk param_ty, zonk body_ty)
      | Tuple elem_tys -> Tuple (List.map zonk elem_tys)
      | Bool -> ty
      | Int -> Int

    let pp (ty : t) (ppf : Format.formatter) : unit =
      let rec pp_ty ty ppf =
        match ty with
        | Meta m -> pp_meta pp_ty m ppf
        | Fun (param_ty, body_ty) ->
            Format.fprintf ppf "%t -> %t"
              (pp_atomic_ty param_ty)
              (pp_ty body_ty)
        | ty ->
            pp_atomic_ty ty ppf
      and pp_atomic_ty ty ppf =
        match ty with
        | Var name -> Format.fprintf ppf "%s" name
        | Meta m -> pp_meta pp_atomic_ty m ppf
        | Tuple [] -> Format.fprintf ppf "()"
        | Tuple elem_tys ->
            Format.fprintf ppf "@[(%a)@]"
              (Format.pp_print_list (Fun.flip pp_ty)
                ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ "))
              elem_tys
        | Bool -> Format.fprintf ppf "Bool"
        | Int -> Format.fprintf ppf "Int"
        | Fun _ as ty -> Format.fprintf ppf "@[(%t)@]" (pp_ty ty)
      and pp_meta pp_ty m ppf =
        match !m with
        | Solved ty -> pp_ty ty ppf
        | Unsolved id -> Format.fprintf ppf "?%i" id
      in
      pp_ty ty ppf

  end

  module Expr = struct

    (** Names of bound variables *)
    type name = string

    type t =
      | Var of name * Ty.t list
      | Let of def * t
      | Let_rec of def list * t
      | Fun_lit of name * Ty.t * t
      | Fun_app of t * t
      | Tuple_lit of t list
      | Tuple_proj of t * int
      | Bool_lit of bool
      | Bool_if of t * t * t
      | Int_lit of int

    (** Polymorphic definitions, parameterised by a series of type arguments.
        In System F this would look like:

        {@text[
          x : ∀ a₁ ... aₙ. t := Λ a₁ ... aₙ. e₁
        ]}
    *)
    and def = {
      name : name;
      ty_params : Ty.name list; [@warning "-unused-field"]
      ty : Ty.t;
      expr : t;
    }

    module Value = struct

      type t =
        | Fun_lit of (t -> t)
        | Tuple_lit of t list
        | Bool_lit of bool
        | Int_lit of int

      let[@warning "-unused-value-declaration"] rec pp (vexpr : t) (ppf : Format.formatter) : unit =
        match vexpr with
        | Fun_lit _ -> Format.fprintf ppf "<function>"
        | Tuple_lit [] -> Format.fprintf ppf "()"
        | Tuple_lit elems ->
            Format.fprintf ppf "@[(%a)@]"
              (Format.pp_print_list (Fun.flip pp)
                ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ "))
              elems
        | Bool_lit true -> Format.fprintf ppf "true"
        | Bool_lit false -> Format.fprintf ppf "false"
        | Int_lit i -> Format.fprintf ppf "%i" i

    end

    let rec eval (env : (string * Value.t ref) list) (expr : t) : Value.t =
      match expr with
      | Var (name, _) ->
          !(List.assoc name env)
      | Let (def, body) ->
          let vdef = eval env def.expr in
          eval ((def.name, ref vdef) :: env) body
      | Let_rec (defs, body) ->
          (* Add placeholder bindings to the environment then back-patch them
             with their corresponding recursive definitions. This is inspired by
             R⁵RS Scheme’s approach to encoding [letrec] (see Section 7.3 of the
             {{: https://dl.acm.org/doi/10.1145/290229.290234} Revised⁵ Report
             on the Algorithmic Language Scheme}).

             This relies on each definition being a function in order to avoid
             the bindings from being accessed before they have been defined. *)
          let undefined = Value.Fun_lit (fun _ -> failwith "undefined") in
          let bindings = defs |> List.map (fun def -> def.name, ref undefined) in
          let env = List.rev_append bindings env in
          List.iter2 (fun (_, vdef) def -> vdef := eval env def.expr) bindings defs;
          eval env body
      | Fun_lit (name, _, body) ->
          Value.Fun_lit (fun varg ->
            eval ((name, ref varg) :: env) body)
      | Fun_app (head, arg) ->
          begin match eval env head with
          | Value.Fun_lit vbody -> vbody (eval env arg)
          | _ -> invalid_arg "eval"
          end
      | Tuple_lit elems ->
          Value.Tuple_lit (List.map (eval env) elems)
      | Tuple_proj (head, index) ->
          begin match eval env head with
          | Value.Tuple_lit velems -> List.nth velems index
          | _ -> invalid_arg "eval"
          end
      | Bool_lit b -> Value.Bool_lit b
      | Bool_if (head, true_body, false_body) ->
          begin match eval env head with
          | Value.Bool_lit true -> eval env true_body
          | Value.Bool_lit false -> eval env false_body
          | _ -> invalid_arg "eval"
          end
      | Int_lit i -> Value.Int_lit i

    let rec zonk (expr : t) : t =
      match expr with
      | Var (name, ty_args) -> Var (name, List.map Ty.zonk ty_args)
      | Let (def, body) ->
          Let (zonk_def def, zonk body)
      | Let_rec (defs, body) ->
          Let_rec (List.map zonk_def defs, zonk body)
      | Fun_lit (name, ty, body) ->
          Fun_lit (name, Ty.zonk ty, zonk body)
      | Fun_app (head, arg) ->
          Fun_app (zonk head, zonk arg)
      | Tuple_lit elems ->
          Tuple_lit (List.map zonk elems)
      | Tuple_proj (head, index) ->
          Tuple_proj (zonk head, index)
      | Bool_lit _ -> expr
      | Bool_if (head, true_body, false_body) ->
          Bool_if (zonk head, zonk true_body, zonk false_body)
      | Int_lit _ -> expr

    and zonk_def ({ ty; expr; _ } as def : def) : def =
      { def with ty = Ty.zonk ty; expr = zonk expr }

  end

end


(** The surface language closely mirrors what the programmer originally wrote,
    including syntactic sugar and higher level language features that make
    programming more convenient. *)
module Surface = struct

  module Ty = struct

    type t =
      | Name of string
      | Fun of t * t
      | Tuple of t list
      | Placeholder
      [@@warning "-unused-constructor"]

  end

  module Expr = struct

    type t =
      | Name of string * Ty.t list
      | Let of def * t
      | Let_rec of def list * t
      | Ann of t * Ty.t
      | Fun of string * Ty.t option * t
      | Tuple of t list
      | Int of int
      | App of t * t
      | Proj of t * int
      | If of t * t * t
      [@@warning "-unused-constructor"]

    and def = {
      name : string;
      ty_params : string list;
      ty : Ty.t option;
      expr : t;
    }

  end

  (** Elaboration of the surface language into the core language.

      This is where we implement user-facing type checking, while also
      translating the surface language into the simpler, more explicit core
      language (e.g. with explicit type annotations and type applications).
  *)
  module Elab : sig

    val check_ty : Ty.t -> (Core.Ty.t, string) result [@@warning "-unused-value-declaration"]
    val check_expr : Expr.t -> Core.Ty.t -> (Core.Expr.t, string) result [@@warning "-unused-value-declaration"]
    val infer_expr : Expr.t -> (Core.Expr.t * Core.Ty.t, string) result

  end = struct

    (** An exception raised on an elaboration error. These should be caught
        before they escape this module. *)
    exception Error of string

    (** Raise an elaboration error with a formatted message *)
    let error (type a b) (f : (b, Format.formatter, unit, a) format4) : b =
      Format.kasprintf (fun msg -> raise (Error msg)) f


    (** Elaboration contexts *)
    module Ctx : sig

      type t

      val create : unit -> t

      val fresh_meta : t -> string -> Core.Ty.t

      val extend_tys : t -> Core.Ty.name list -> t
      val extend_expr : t -> Core.Expr.name -> Core.(Ty.name list * Ty.t) -> t

      val lookup_ty : t -> Core.Ty.name -> unit option
      val lookup_expr : t -> Core.Expr.name -> Core.(Ty.name list * Ty.t) option

      val unsolved_metas : t -> string Seq.t

    end = struct

      type t = {
        metas : (Core.Ty.meta * string) Dynarray.t;
        ty_names : Core.Ty.name list list;
        expr_tys : (Core.Expr.name * Core.(Ty.name list * Ty.t)) list;
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

      let extend_tys (ctx : t) (names : Core.Ty.name list) : t =
        { ctx with ty_names = names :: ctx.ty_names }

      let extend_expr (ctx : t) (name : Core.Expr.name) (ty_params, ty : Core.(Ty.name list * Ty.t)) : t =
        { ctx with expr_tys = (name, (ty_params, ty)) :: ctx.expr_tys }

      let lookup_ty (ctx : t) (name : Core.Ty.name) : unit option =
        if List.exists (List.mem name) ctx.ty_names then Some () else None

      let lookup_expr (ctx : t) (name : Core.Expr.name) : Core.(Ty.name list * Ty.t) option =
        List.assoc_opt name ctx.expr_tys

      let unsolved_metas (ctx : t) : string Seq.t =
        Dynarray.to_seq ctx.metas |> Seq.filter_map @@ function
          | { contents = Core.Ty.Unsolved _ }, desc -> Some desc
          | { contents = Core.Ty.Solved _ }, _ -> None

    end

    let unify_tys ~(found : Core.Ty.t) ~(expected : Core.Ty.t) =
      try Core.Ty.unify expected found with
      | Core.Ty.Mismatched_types ->
          error "expected: %t, found: %t" (Core.Ty.pp expected) (Core.Ty.pp found)
      | Core.Ty.Infinite_type ->
          error "infinite type"


    (* Bidirectional elaboration *)

    (** Elaborate a type, checking that it is well-formed. *)
    let rec check_ty (ctx : Ctx.t) (ty : Ty.t) : Core.Ty.t =
      match ty with
      | Ty.Name name ->
          begin match Ctx.lookup_ty ctx name with
          | Some () -> Core.Ty.Var name
          | None when name = "Bool" -> Core.Ty.Bool
          | None -> error "unbound type variable `%s`" name
          end
      | Ty.Fun (ty1, ty2) -> Core.Ty.Fun (check_ty ctx ty1, check_ty ctx ty2)
      | Ty.Tuple elem_tys -> Core.Ty.Tuple (List.map (check_ty ctx) elem_tys)
      | Ty.Placeholder -> Ctx.fresh_meta ctx "placeholder"

    (** Elaborate a term, given an expected type. *)
    let rec check_expr (ctx : Ctx.t) (expr : Expr.t) (ty : Core.Ty.t) : Core.Expr.t =
      match expr, Core.Ty.force ty with
      | Expr.Let (def, body), body_ty ->
          let ctx, def = infer_def ctx def in
          let body = check_expr ctx body body_ty in
          Core.Expr.Let (def, body)

      | Expr.Let_rec (defs, body), body_ty ->
          let ctx, defs = infer_rec_defs ctx defs in
          let body = check_expr ctx body body_ty in
          Core.Expr.Let_rec (defs, body)

      | Expr.Fun (name, None, body), Core.Ty.Fun (param_ty, body_ty) ->
          let body = check_expr (Ctx.extend_expr ctx name ([], param_ty)) body body_ty in
          Core.Expr.Fun_lit (name, param_ty, body)

      | Expr.Fun (name, Some param_ty, body), Core.Ty.Fun (param_ty', body_ty) ->
          let param_ty = check_ty ctx param_ty in
          unify_tys ~found:param_ty ~expected:param_ty';
          let body = check_expr (Ctx.extend_expr ctx name ([], param_ty')) body body_ty in
          Core.Expr.Fun_lit (name, param_ty, body)

      | Expr.Tuple elems, Core.Ty.Tuple elem_tys ->
          if List.length elems <> List.length elem_tys then
            error "expected %i elements, found %i elements"
              (List.length elem_tys)
              (List.length elems);
          Core.Expr.Tuple_lit (List.map2 (check_expr ctx) elems elem_tys)

      | Expr.If (head, true_body, false_body), body_ty ->
          let head = check_expr ctx head Core.Ty.Bool in
          let true_body = check_expr ctx true_body body_ty in
          let false_body = check_expr ctx false_body body_ty in
          Core.Expr.Bool_if (head, true_body, false_body)

      | expr, expected_ty ->
          let expr, found_ty = infer_expr ctx expr in
          unify_tys ~found:found_ty ~expected:expected_ty;
          expr

    (** Elaborate a term, inferring its type. *)
    and infer_expr (ctx : Ctx.t) (expr : Expr.t) : Core.Expr.t * Core.Ty.t =
      match expr with
      | Expr.Name (name, ty_args) ->
          let expr, pty =
            match Ctx.lookup_expr ctx name with
            | Some pty -> (fun ty_args -> Core.Expr.Var (name, ty_args)), pty
            | None when name = "true" -> (fun[@warning "-partial-match"] [] -> Core.Expr.Bool_lit true), ([], Core.Ty.Bool)
            | None when name = "false" -> (fun[@warning "-partial-match"] [] -> Core.Expr.Bool_lit true), ([], Core.Ty.Bool)
            | None -> error "unbound variable `%s`" name
          in

          begin match pty, ty_args with
          | (ty_params, ty), [] ->
              let mapping = ty_params |> List.map (fun name -> name, Ctx.fresh_meta ctx "type argument") in
              expr (List.map snd mapping), Core.Ty.subst mapping ty

          | (ty_params, ty), ty_args when List.length ty_params = List.length ty_args ->
              let ty_args = List.map (check_ty ctx) ty_args in
              let mapping = List.combine ty_params ty_args in
              expr (List.map snd mapping), Core.Ty.subst mapping ty

          | (ty_params, _), ty_args ->
              error "expected %i type %s, found %i"
                (List.length ty_params)
                (match ty_params with [_] -> "argument" | _ -> "arguments")
                (List.length ty_args)
          end

      | Expr.Let (def, body) ->
          let ctx, def = infer_def ctx def in
          let body, body_ty = infer_expr ctx body in
          Core.Expr.Let (def, body), body_ty

      | Expr.Let_rec (defs, body) ->
          let ctx, defs = infer_rec_defs ctx defs in
          let body, body_ty = infer_expr ctx body in
          Core.Expr.Let_rec (defs, body), body_ty

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

      | Expr.Tuple elems ->
          let elems, elem_tys = List.split (List.map (infer_expr ctx) elems) in
          Core.Expr.Tuple_lit elems, Core.Ty.Tuple elem_tys

      | Expr.Int i ->
          Core.Expr.Int_lit i, Core.Ty.Int

      | Expr.App (head, arg) ->
          let head, head_ty = infer_expr ctx head in
          begin match Core.Ty.force head_ty with
          | Core.Ty.Fun (param_ty, body_ty) ->
              let arg = check_expr ctx arg param_ty in
              Core.Expr.Fun_app (head, arg), body_ty
          | Core.Ty.Meta _ as head_ty ->
              let arg, arg_ty = infer_expr ctx arg in
              let body_ty = Ctx.fresh_meta ctx "return type" in
              unify_tys ~found:head_ty ~expected:(Core.Ty.Fun (arg_ty, body_ty));
              Core.Expr.Fun_app (head, arg), body_ty
          |_ -> error "unexpected argument"
          end

      | Expr.Proj (head, index) ->
          let head, head_ty = infer_expr ctx head in
          begin match Core.Ty.force head_ty with
          | Core.Ty.Tuple elem_tys ->
              begin match List.nth_opt elem_tys index with
              | Some ty -> Core.Expr.Tuple_proj (head, index), ty
              | None -> error "unknown field"
              end
          | _ -> error "expected tuple, found: %t" (Core.Ty.pp head_ty)
          end

      | Expr.If (head, true_body, false_body) ->
          let head = check_expr ctx head Core.Ty.Bool in
          let body_ty = Ctx.fresh_meta ctx "if branches" in
          let true_body = check_expr ctx true_body body_ty in
          let false_body = check_expr ctx false_body body_ty in
          Core.Expr.Bool_if (head, true_body, false_body), body_ty

    (** Elaborate a definition, return a context with the definition bound. *)
    and infer_def (ctx : Ctx.t) ({ name; ty_params; ty; expr } : Expr.def) : Ctx.t * Core.Expr.def =
      uncons (find_dupes ty_params) |> Option.iter (fun (name, names) ->
        error "reused type parameter names: %s"
          (String.concat "," (name :: names)));

      let expr, ty =
        match ty with
        | None -> infer_expr (Ctx.extend_tys ctx ty_params) expr
        | Some ty ->
            let ty = check_ty (Ctx.extend_tys ctx ty_params) ty in
            check_expr (Ctx.extend_tys ctx ty_params) expr ty, ty
      in
      Ctx.extend_expr ctx name (ty_params, ty),
      Core.Expr.{ name; ty_params; ty; expr }

    (** Elaborate a series of mutually recursive definitions, returning a
        context with them bound. *)
    and infer_rec_defs (ctx : Ctx.t) (defs : Expr.def list) : Ctx.t * Core.Expr.def list =
      let names = List.map (fun Expr.{ name; _ } -> name) defs in
      uncons (find_dupes names) |> Option.iter (fun (name, names) ->
        error "reused names in recursive binding: %s"
          (String.concat "," (name :: names)));

      (* Extend the typing context with forward declarations for each
         recursive definition *)
      let ctx =
        ListLabels.fold_right defs
          ~init:ctx
          ~f:(fun Expr.{ name; ty_params; ty; _ } ctx ->
            uncons (find_dupes ty_params) |> Option.iter (fun (name, names) ->
              error "reused type parameter names: %s"
                (String.concat "," (name :: names)));

            let ty =
              match ty with
              | None -> Ctx.fresh_meta ctx "definition type"
              | Some ty -> check_ty (Ctx.extend_tys ctx ty_params) ty
            in
            Ctx.extend_expr ctx name (ty_params, ty))
      in

      (* Elaborate the definitions with the recursive definitions in scope *)
      let defs =
        defs |> List.map @@ fun Expr.{ name; expr; _ } ->
          let ty_params, ty = Option.get (Ctx.lookup_expr ctx name) in
          let expr = check_expr (Ctx.extend_tys ctx ty_params) expr ty in
          match Core.Ty.force ty with
          | Core.Ty.Fun _ -> Core.Expr.{ name; ty_params; ty; expr }
          | _ -> error "definitions must be functions in recursive let bindings"
      in

      ctx, defs


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
      Expr.Let (
        { name = "id";
          ty_params = ["A"];
          ty = None;
          expr = Fun ("x", Some (Name "A"), Name ("x", []));
        },
        Name ("id", []) $ Tuple [])
    in
    begin
      let expr, ty = Elab.infer_expr expr |> expect_ok in
      assert (expr = Core.(
        Expr.Let (
          { name = "id";
            ty_params = ["A"];
            ty = Ty.Fun (Var "A", Var "A");
            expr = Fun_lit ("x", Ty.Var "A", Var ("x", []));
          },
          Fun_app (Var ("id", [Ty.Tuple []]), Tuple_lit []))
      ));
      assert (ty = Core.Ty.Tuple []);
      assert (Core.Expr.eval [] expr = Core.Expr.Value.Tuple_lit []);
    end;

    (* Explicit type application *)
    let expr =
      Expr.Let (
        { name = "id";
          ty_params = ["A"];
          ty = None;
          expr = Fun ("x", Some (Name "A"), Name ("x", []));
        },
        Name ("id", [Ty.Tuple []]) $ Tuple [])
    in
    begin
      let expr, ty = Elab.infer_expr expr |> expect_ok in
      assert (expr = Core.(
        Expr.Let (
          { name = "id";
            ty_params = ["A"];
            ty = Ty.Fun (Var "A", Var "A");
            expr = Fun_lit ("x", Ty.Var "A", Var ("x", []));
          },
          Fun_app (Var ("id", [Ty.Tuple []]), Tuple_lit []))
      ));
      assert (ty = Core.Ty.Tuple []);
      assert (Core.Expr.eval [] expr = Core.Expr.Value.Tuple_lit []);
    end;

    (* Constant function *)
    let expr =
      Expr.Let (
        { name = "id";
          ty_params = ["A"];
          ty = None;
          expr = Fun ("x", Some (Name "A"), Name ("x", []));
        },
        Let (
          { name = "const";
            ty_params = ["A"; "B"];
            ty = None;
            expr = Fun ("x", Some (Name "A"), Fun ("y", Some (Name "B"), Name ("x", [])));
          },
          Name ("const", []) $ Tuple [] $ (Name ("id", []) $ Name ("true", []))))
    in
    begin
      let expr, ty = Elab.infer_expr expr |> expect_ok in
      assert (expr = Core.(
        let ( $ ) f x = Expr.Fun_app (f, x) in
        Expr.Let (
          { name = "id";
            ty_params = ["A"];
            ty = Ty.Fun (Var "A", Var "A");
            expr = Fun_lit ("x", Ty.Var "A", Var ("x", []));
          },
          Let (
            { name = "const";
              ty_params = ["A"; "B"];
              ty = Ty.Fun (Var "A", Ty.Fun (Var "B", Var "A"));
              expr = Fun_lit ("x", Ty.Var "A", Fun_lit ("y", Ty.Var "B", Var ("x", [])));
            },
            Var ("const", [Ty.Tuple []; Ty.Bool]) $ Tuple_lit [] $ (Var ("id", [Ty.Bool]) $ Bool_lit true)))
      ));
      assert (ty = Core.Ty.Tuple []);
      assert (Core.Expr.eval [] expr = Core.Expr.Value.Tuple_lit []);
    end;

    (* Locally polymorphic definitions *)
    let expr =
      (* False combinator https://www.angelfire.com/tx4/cus/combinator/birds.html *)
      Expr.Let (
        { name = "kite";
          ty_params = ["A"; "B"];
          ty = Some (Fun (Name "A", Fun (Name "B", Name "B")));
          expr =
            Fun ("x", None,
              Expr.Let (
                { name = "id";
                  ty_params = ["A"];
                  ty = None;
                  expr = Fun ("x", Some (Name "A"), Name ("x", []));
                },
                Name ("id", [])));
        },
        Name ("kite", []) $ Tuple [] $ Name ("true", []))
    in
    begin
      let expr, ty = Elab.infer_expr expr |> expect_ok in
      assert (expr = Core.(
        let ( $ ) f x = Expr.Fun_app (f, x) in
        Expr.Let (
          { name = "kite";
            ty_params = ["A"; "B"];
            ty = Ty.Fun (Var "A", Ty.Fun (Var "B", Var "B"));
            expr =
              Fun_lit ("x", Ty.Var "A",
                Let (
                  { name = "id";
                    ty_params = ["A"];
                    ty = Ty.Fun (Var "A", Var "A");
                    expr = Fun_lit ("x", Ty.Var "A", Var ("x", []));
                  },
                  Var ("id", [Ty.Var "B"])));
          },
          Var ("kite", [Ty.Tuple []; Ty.Bool]) $ Tuple_lit [] $ Bool_lit true)
      ));
      assert (ty = Core.Ty.Bool);
      assert (Core.Expr.eval [] expr = Core.Expr.Value.Bool_lit true);
    end;

    (* TODO: More tests *)

  end;

  print_string " ok!\n";

end
