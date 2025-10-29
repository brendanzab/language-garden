(** An implementation of type inference for a polymorphic functional language
    using in-place, imperative unification (a.k.a. Algorithm J).

    {2 On Milner’s original paper}

    Algorithm J was first described by Robin Milner in 1978 in the context of
    type checking early versions of ML. It was intended to be called “Algorithm
    I”, but  became known as “Algorithm J” by way of a typographic mishap, as
    documented by Nipkow and Prehofer in {{: https://belle.sourceforge.net/doc/jfp95.pdf}
    “Type Reconstruction for Type Classes”}:

    {%html:
      <blockquote>
        Although the typography in (Mil78) is ambiguous, Milner has confirmed
        by email that he intended it to be I, not J: it is an imperative
        implementation of W. Milner’s I is imperative because he maintains a
        single global copy of θ which is updated by side-effects. In a purely
        functional style this requires an additional argument and result.
      </blockquote>
    %}

    I found Milner’s original presentation of the algorithm is a little hard to
    follow, as it’s quite different from the syntax-directed inference rules in
    the presentations I’m familiar with today. For example it introduces a
    notion of “prefixes” whereas today we would use typing contexts.
    Generalisation and instantiation are more obscured and hard to find - I’ve
    been assuming they emerge from how prefixes are handled.

    {2 Implementation notes}

    This implementation was originally inspired by {{: https://github.com/jfecher/algorithm-j/}
    jfecher’s implementation of Milner’s algorithm}. I also folded in some ideas
    I saw in {{: https://gist.github.com/mb64/87ac275c327ea923a8d587df7863d8c7}
    Mark Barbone’s implementation of higher rank polymorphism}. One departure
    I make from jfecher and other ML implementations is that, like Barbone, I’ve
    distinguished type variables bound by foralls from metavariables, which I
    think makes things more clear. I’ve also decided to thread through the size
    of the type environment explicitly rather than using mutable variables.

    {2 Resources}

    - Robin Milner, “A theory of type polymorphism in programming”, 1978 https://doi.org/10.1016/0022-0000(78)90014-4
    - Wikipedia, “Hindley–Milner type system - Algorithm J”, https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system#Algorithm_J
    - Oleg Kiselyov, “Efficient and insightful generalization”, https://okmij.org/ftp/ML/generalization.html
    - Richard Eisenberg, “Type inference in OCaml and GHC using Levels”, https://www.youtube.com/watch?v=iFUrhTQi0-U
*)

(** Untyped lambda expressions *)
module Expr = struct

  type t =
    | Var of string
    | Let of string * t * t
    | Fun_lit of string * t
    | Fun_app of t * t
    | Unit_lit

end

(** Fresh name generation *)
module Fresh = struct

  module type S = sig

    type t

    val fresh : unit -> t
    val compare : t -> t -> int

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

  end

end

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

  (** Mutable representation of metavariables, to be updated in-place during
      unification and typechecking *)
  and meta = meta_state ref

  (** The current state of a metavariable. *)
  and meta_state =
    | Solved of t
    | Unsolved of Id.t * level

  (** Create a fresh, unsolved metavariable at a given level in the type
      environment *)
  let fresh_meta (size : level) : meta =
    ref (Unsolved (Id.fresh (), size))

  (** Replace bound variables in a type *)
  let rec subst (ty : t) (mapping : (Id.t * t) list) : t =
    match ty with
    | Var id ->
        begin match List.assoc_opt id mapping with
        | Some ty -> ty
        | None -> ty
        end
    | Meta ({ contents = Solved ty }) -> subst ty mapping
    | Meta ({ contents = Unsolved _}) -> ty
    | Fun (param_ty, body_ty) -> Fun (subst param_ty mapping, subst body_ty mapping)
    | Unit -> Unit

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
    (* Occurs check and level raising *)
    | Meta m' when m == m' -> raise Infinite_type
    | Meta { contents = Solved ty } -> occurs m level ty
    | Meta ({ contents = Unsolved (id', level') } as m') ->
        if level < level' then
          m' := Unsolved (id', level)

  let rec unify (size : level) (ty1 : t) (ty2 : t) =
    match ty1, ty2 with
    | Var id1, Var id2 when id1 = id2 -> ()
    | Meta m1, Meta m2 when m1 == m2 -> ()
    | Unit, Unit -> ()
    | Fun (param_ty1, body_ty1), Fun (param_ty2, body_ty2) ->
        unify size param_ty1 param_ty2;
        unify size body_ty1 body_ty2

    | Meta ({ contents = Solved ty1 }), ty2 -> unify size ty1 ty2
    | ty1, Meta ({ contents = Solved ty2 }) -> unify size ty1 ty2
    | Meta ({ contents = Unsolved (_, level) } as m), ty
    | ty, Meta ({ contents = Unsolved (_, level) } as m) ->
        occurs m level ty;
        m := Solved ty

    | _, _ -> raise (Mismatched_types (ty1, ty2))

  let rec zonk (ty : t) : t =
    match ty with
    | Var _ | Unit -> ty
    | Meta ({ contents = Solved ty }) -> zonk ty
    | Meta ({ contents = Unsolved _ }) -> ty
    | Fun (param_ty, body_ty) -> Fun (zonk param_ty, zonk body_ty)

end

(** The main type checking algorithm *)
module Check : sig

  val infer : Expr.t -> (Ty.t, string) result

end = struct

  exception Error of string

  (** Types that might introduce a series of type variable bindings. *)
  type poly_ty =
    | Forall of Ty.Id.t list * Ty.t

  type context = {
    metas : Ty.meta Dynarray.t;
    ty_size : Ty.level;
    expr_tys : (string * poly_ty) list;
  }

  let empty_context () = {
    metas = Dynarray.create ();
    ty_size = 0;
    expr_tys = [];
  }

  let fresh_meta (ctx : context) : Ty.t =
    let m = Ty.fresh_meta ctx.ty_size in
    Dynarray.add_last ctx.metas m;
    Ty.Meta m

  let extend_ty (ctx : context) : context =
    { ctx with ty_size = 1 + ctx.ty_size }

  let extend_expr (ctx : context) (name : string) (ty : poly_ty) : context =
    { ctx with expr_tys = (name, ty) :: ctx.expr_tys }

  let lookup_expr (ctx : context) (name : string) : poly_ty =
    match List.assoc_opt name ctx.expr_tys with
    | Some ty -> ty
    | None -> raise (Error "unbound variable")

  let unify (ctx : context) (ty1 : Ty.t) (ty2 : Ty.t) =
    try Ty.unify ctx.ty_size ty1 ty2 with
    | Ty.Mismatched_types _ -> raise (Error "type mismatch")
    | Ty.Infinite_type -> raise (Error "infinite type")

  (** Convert a polytype to a monotype by replacing the bound type variables with
      fresh metavariables. *)
  let instantiate (ctx : context) (Forall (ty_params, ty) : poly_ty) : Ty.t =
    Ty.subst ty (List.map (fun tv -> tv, fresh_meta ctx) ty_params)

  (** Find the unsolved metavariables in a monotype that were introduced within
      the current level of the type environment and return a polytype that binds
      them in a forall. *)
  let generalize (ctx : context) (t : Ty.t) : poly_ty =
    let ty_ids = ref [] in
    let rec go t =
      match t with
      | Ty.Var _ -> ()
      | Ty.Unit -> ()
      | Ty.Fun (param_ty, body_ty) ->
          go param_ty;
          go body_ty
      | Ty.Meta ({ contents = Solved t }) -> go t
      | Ty.Meta ({ contents = Unsolved (id, level) } as m) ->
          if ctx.ty_size < level then begin
            ty_ids := id :: !ty_ids;
            m := Solved (Var id);
          end
    in
    go t;
    let ty_params = List.sort_uniq Ty.Id.compare !ty_ids in
    Forall (ty_params, t)

  let rec infer (ctx : context) (expr : Expr.t) : Ty.t =
    match expr with
    | Expr.Var x ->
        instantiate ctx (lookup_expr ctx x)
    | Expr.Let (x, def, body) ->
        let def_ty = infer (extend_ty ctx) def in
        infer (extend_expr ctx x (generalize ctx def_ty)) body
    | Expr.Fun_lit (x, body) ->
        let param_ty = fresh_meta ctx in
        let body_ty = infer (extend_expr ctx x (Forall ([], param_ty))) body in
        Ty.Fun (param_ty, body_ty)
    | Expr.Fun_app (fn, arg) ->
        let fn_ty = infer ctx fn in
        let arg_ty = infer ctx arg in
        let body_ty = fresh_meta ctx in
        unify ctx fn_ty (Ty.Fun (arg_ty, body_ty));
        body_ty
    | Expr.Unit_lit ->
        Ty.Unit

  let infer (expr : Expr.t) : (Ty.t, string) result =
    try
      let ctx = empty_context () in
      let ty = infer ctx expr in
      ctx.metas |> Dynarray.iter begin function
        | { contents = Ty.Solved _ } -> ()
        | { contents = Ty.Unsolved _ } -> raise (Error "ambiguous type")
      end;
      Ok (Ty.zonk ty)
    with
    | Error msg -> Error msg

end

let () = begin

  print_string "Running tests ...";

  let open Expr in

  let ( $ ) f x = Fun_app (f, x) in

  let expr =
    Let ("id", Fun_lit ("x", Var "x"),
      Var "id" $ Unit_lit)
  in
  assert (Check.infer expr = Ok Ty.Unit);

  let expr =
    Let ("id", Fun_lit ("x", Var "x"),
      Let ("const", Fun_lit ("x", Fun_lit ("y", Var "x")),
        Var "const" $ Var "id" $ Unit_lit $ Unit_lit))
  in
  assert (Check.infer expr = Ok Ty.Unit);

  let expr =
    Fun_lit ("x", Var "x")
  in
  assert (Check.infer expr = Error "ambiguous type");

  print_string " ok!\n";

end
