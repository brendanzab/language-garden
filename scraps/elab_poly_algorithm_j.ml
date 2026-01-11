(** An implementation of elaboration for a polymorphic functional language
    using in-place, imperative unification (a.k.a. Algorithm J).

    Extends [check_stlc_poly_algorithm_j].
*)

(** Fresh name generation *)
module Fresh = struct

  module type S = sig

    type t

    val fresh : unit -> t
    val compare : t -> t -> int
    val to_int : t -> int [@@warning "-unused-value-declaration"]

  end

  (** Create a new fresh name generator *)
  module Make () : S = struct

    type t = int

    let next = ref 0

    let fresh () =
      let x = !next in
      incr next;
      x

    let compare = Int.compare
    let to_int = Fun.id

  end

end

module Core = struct

  (** Monotypes *)
  module Ty = struct

    (** Number of type binders in the type environment. This is incremented every
        time we go under a “forall”, and is used to make the generalisation of
        metavariables more efficient compared to using free variable sets. *)
    type level = int

    (** Fresh identifiers used for bound type variables and metavariables. *)
    module Id = Fresh.Make ()

    (* TODO: we could possibly use separate identifier namespaces for type
             variables and metavariables *)

    (** Monotypes *)
    type t =
      | Var of Id.t         (* Type variables (bound by foralls) *)
      | Meta of meta        (* Metavariables (used for unification) *)
      | Fun of t * t
      | Unit
      | Bool

    (** Mutable representation of metavariables, to be updated in-place during
        unification and typechecking *)
    and meta = meta_state ref

    (** The current state of a metavariable. *)
    and meta_state =
      | Solved of t
      | Unsolved of { id : Id.t; level : level }

    (** Create a fresh, unsolved metavariable at a given level in the type
        environment *)
    let fresh_meta (size : level) : meta =
      ref (Unsolved { id = Id.fresh (); level = size })

    (** Replace bound variables in a type *)
    let rec subst (ty : t) (mapping : (Id.t * t) list) : t =
      match ty with
      | Var id -> List.assoc_opt id mapping |> Option.value ~default:ty
      | Meta { contents = Solved ty } -> subst ty mapping
      | Meta { contents = Unsolved _ } -> ty
      | Fun (param_ty, body_ty) -> Fun (subst param_ty mapping, subst body_ty mapping)
      | Unit -> Unit
      | Bool -> Bool

    exception Mismatched_types of t * t
    exception Infinite_type

    (** Ensure that the candidate type does not refer to the to-be-solved
        metavariable, and raise the level of metavariables as required. *)
    let rec occurs (m : meta) (level : level) (ty : t) =
      match ty with
      | Var _ -> ()
      | Fun (param_ty, body_ty) ->
          occurs m level param_ty;
          occurs m level body_ty
      | Unit -> ()
      | Bool -> ()
      (* Occurs check and level raising *)
      | Meta m' when m == m' -> raise Infinite_type
      | Meta { contents = Solved ty } -> occurs m level ty
      | Meta ({ contents = Unsolved other } as m') ->
          if level < other.level then
            m' := Unsolved { other with level }

    let rec unify (size : level) (ty1 : t) (ty2 : t) =
      match ty1, ty2 with
      | Var id1, Var id2 when id1 = id2 -> ()
      | Meta m1, Meta m2 when m1 == m2 -> ()    (* NOTE: using pointer equality for references *)
      | Unit, Unit -> ()
      | Bool, Bool -> ()
      | Fun (param_ty1, body_ty1), Fun (param_ty2, body_ty2) ->
          unify size param_ty1 param_ty2;
          unify size body_ty1 body_ty2

      (* Unify through solved metavariables *)
      | Meta { contents = Solved ty1 }, ty2 -> unify size ty1 ty2
      | ty1, Meta { contents = Solved ty2 } -> unify size ty1 ty2

      (* Update unsolved metavariables in-place *)
      | Meta ({ contents = Unsolved { level; _ } } as m), ty
      | ty, Meta ({ contents = Unsolved { level; _ } } as m) ->
          occurs m level ty;
          m := Solved ty

      | _, _ -> raise (Mismatched_types (ty1, ty2))

    (** Inline metavariables into a type, and default unsolved metavariables to
        the unit type (this should be safe, as any escaping metavariables should
        have been previously generalised). *)
    let rec zonk (ty : t) : t =
      match ty with
      | Var _ -> ty
      | Meta m ->
          let ty =
            match !m with
            | Solved ty -> zonk ty
            | Unsolved _ -> Unit  (* default unsolved metavariables to Unit *)
          in
          m := Solved ty;
          ty
      | Fun (param_ty, body_ty) ->
          Fun (zonk param_ty, zonk body_ty)
      | Unit -> ty
      | Bool -> ty

  end

  module Expr = struct

    type t =
      | Var of string * Ty.t list
      (** A variable that is possibly applied to a series of type arguments
          (which will be applied a type lambda in a definition). *)

      | Let of string * Ty.Id.t list * Ty.t * t * t
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
      | Var (x, ty_args) -> Var (x, List.map Ty.zonk ty_args)
      | Let (x, ty_params, def_ty, def, body) ->
          Let (x, ty_params, Ty.zonk def_ty, zonk def, zonk body)
      | Fun_lit (x, ty, body) ->
          Fun_lit (x, Ty.zonk ty, zonk body)
      | Fun_app (fn, arg) ->
          Fun_app (zonk fn, zonk arg)
      | Unit_lit -> expr
      | Bool_lit _ -> expr
      | Bool_if (pred, if_true, if_false) ->
          Bool_if (zonk pred, zonk if_true, zonk if_false)

  end

end

module Surface = struct

  type expr =
    | Var of string
    | Let of string * expr * expr
    | Fun of string * expr
    | App of expr * expr
    | Unit
    | Bool of bool [@warning "-unused-constructor"]
    | If of expr * expr * expr [@warning "-unused-constructor"]

  (** The main type checking algorithm *)
  module Elab : sig

    val infer : expr -> (Core.Ty.Id.t list * Core.Expr.t * Core.Ty.t, string) result

  end = struct

    open Core

    exception Error of string

    type poly_ty = Ty.Id.t list * Ty.t

    type context = {
      ty_size : Ty.level;
      expr_tys : (string * poly_ty) list;
    }

    let empty_ctx = {
      ty_size = 0;
      expr_tys = [];
    }

    let fresh_meta (ctx : context) : Ty.t =
      Ty.Meta (Ty.fresh_meta ctx.ty_size)

    let extend_ty (ctx : context) : context =
      { ctx with ty_size = 1 + ctx.ty_size }

    let extend_expr (ctx : context) (name : string) (ty_params, ty : poly_ty) : context =
      { ctx with expr_tys = (name, (ty_params, ty)) :: ctx.expr_tys }

    let lookup_expr (ctx : context) (name : string) : poly_ty =
      match List.assoc_opt name ctx.expr_tys with
      | Some ty -> ty
      | None -> raise (Error "unbound variable")

    let unify (ctx : context) (ty1 : Ty.t) (ty2 : Ty.t) =
      try Ty.unify ctx.ty_size ty1 ty2 with
      | Ty.Mismatched_types _ -> raise (Error "type mismatch")
      | Ty.Infinite_type -> raise (Error "infinite type")

    (** Convert a polytype to a monotype by replacing the bound type variables
        with fresh metavariables.

        For example, a polytype [∀ a b. a -> b] would be instantiated to a
        monotype [$m1 -> $m2].
    *)
    let instantiate (ctx : context) (ty_params, ty : poly_ty) : Ty.t list * Ty.t =
      let rec go metas ty_params ty =
        match ty_params with
        | ty_id :: ty_params -> go ((ty_id, fresh_meta ctx) :: metas) ty_params ty
        | [] -> List.rev_map snd metas, Ty.subst ty metas
      in
      go [] ty_params ty

    (** Turn a monotype into a polytype by finding all the unsolved metavariables
        that have been introduced after the current level in the type environment,
        and binding them as parameters in a forall.

        For example, [$m1 -> $m2] generalises to [∀ a b. a -> b] if they were both
        introduced {i after} the current level in the type environment. If [$m1]
        was introduced {i before} the current level in the type environment, then
        [$m1 -> $m2] would generalise to [∀ a. $m1 -> a].
    *)
    let generalise (ctx : context) (ty : Ty.t) : Ty.Id.t list =
      let module S = Set.Make (Ty.Id) in
      let rec ty_params ty =
        match ty with
        | Ty.Var _ -> S.empty
        | Ty.Unit -> S.empty
        | Ty.Bool -> S.empty
        | Ty.Fun (param_ty, body_ty) -> S.union (ty_params param_ty) (ty_params body_ty)
        | Ty.Meta { contents = Solved ty } -> ty_params ty
        | Ty.Meta ({ contents = Unsolved { id; level } } as m) when ctx.ty_size < level ->
            m := Solved (Var id);
            S.singleton id
        | Ty.Meta { contents = Unsolved _ } -> S.empty
      in
      S.to_list (ty_params ty)

    let rec infer (ctx : context) (expr : expr) : Expr.t * Ty.t =
      match expr with
      | Var x ->
          let ty_args, ty = instantiate ctx (lookup_expr ctx x) in
          Expr.Var (x, ty_args), ty
      | Let (x, def, body) ->
          let def, def_ty = infer (extend_ty ctx) def in
          let ty_params = generalise ctx def_ty in
          let body, body_ty = infer (extend_expr ctx x (ty_params, def_ty)) body in
          Expr.Let (x, ty_params, def_ty, def, body), body_ty
      | Fun (x, body) ->
          let param_ty = fresh_meta ctx in
          let body, body_ty = infer (extend_expr ctx x ([], param_ty)) body in
          Expr.Fun_lit (x, param_ty, body), Ty.Fun (param_ty, body_ty)
      | App (fn, arg) ->
          let fn, fn_ty = infer ctx fn in
          let arg, arg_ty = infer ctx arg in
          let body_ty = fresh_meta ctx in
          unify ctx fn_ty (Ty.Fun (arg_ty, body_ty));
          Expr.Fun_app (fn, arg), body_ty
      | Unit ->
          Expr.Unit_lit, Ty.Unit
      | Bool b ->
          Expr.Bool_lit b, Ty.Bool
      | If (pred, if_true, if_false) ->
          let pred, pred_ty = infer ctx pred in
          unify ctx Ty.Bool pred_ty;
          let if_true, if_true_ty = infer ctx if_true in
          let if_false, if_false_ty = infer ctx if_false in
          unify ctx if_true_ty if_false_ty;
          Expr.Bool_if (pred, if_true, if_false), if_true_ty

    let infer (expr : expr) : (Ty.Id.t list * Expr.t * Ty.t, string) result =
      try
        let ctx = empty_ctx in
        let expr, ty = infer (extend_ty ctx) expr in
        let ty_params = generalise ctx ty in
        Ok (ty_params, Expr.zonk expr, Ty.zonk ty)
      with
      | Error msg -> Error msg

  end

end

let () = begin

  Printexc.record_backtrace true;

  Printf.printf "Running tests in %s ..." __FILE__;

  let open Surface in

  let ( $ ) f x = App (f, x) in

  (* TODO: More tests *)

  (* Unsolved meta tests *)
  begin

    (* The following example will leave a metavariable unsolved for the of the
       second function. See “No Unification Variable Left Behind”
       https://doi.org/10.4230/LIPIcs.ITP.2023.8 for more information. *)
    let expr =
      Let ("x", Fun ("f", Unit) $ Fun ("y", Var "y"),
        Var "x")
    in
    assert (Elab.infer expr |> Result.get_ok =
      Core.(
        [],
        Expr.Let ("x", [], Ty.Unit,
          Expr.Fun_app (Expr.Fun_lit ("f", Ty.Fun (Ty.Unit, Ty.Unit), Expr.Unit_lit),
            Expr.Fun_lit ("y", Ty.Unit, Expr.Var ("y", []))),
          Expr.Var ("x", [])),
        Ty.Unit
      )
    );

  end;

  Printf.printf " ok!\n";

end
