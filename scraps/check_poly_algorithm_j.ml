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
    val to_int : t -> int

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
    | Meta { contents = Unsolved _} -> ty
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
    | Meta ({ contents = Unsolved other } as m') ->
        if level < other.level then
          m' := Unsolved { other with level }

  let rec unify (size : level) (ty1 : t) (ty2 : t) =
    match ty1, ty2 with
    | Var id1, Var id2 when id1 = id2 -> ()
    | Meta m1, Meta m2 when m1 == m2 -> ()    (* NOTE: using pointer equality for references *)
    | Unit, Unit -> ()
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

  let pp (ty : t) (names : Id.t -> string) (ppf : Format.formatter) : unit =
    let rec pp_ty ty ppf =
      match ty with
      | Meta ({ contents = Solved ty }) -> pp_ty ty ppf
      | Fun (param_ty, body_ty) ->
          Format.fprintf ppf "%t -> %t"
            (pp_atomic_ty param_ty)
            (pp_ty body_ty)
      | ty ->
          pp_atomic_ty ty ppf
    and pp_atomic_ty ty ppf =
      match ty with
      | Var id -> Format.fprintf ppf "%s" (names id)
      | Meta { contents = Solved ty } -> pp_atomic_ty ty ppf
      | Meta { contents = Unsolved { id; _ } } -> Format.fprintf ppf "$%i" (Id.to_int id)
      | Unit -> Format.fprintf ppf "Unit"
      | Fun _ as ty -> Format.fprintf ppf "@[(%t)@]" (pp_ty ty)
    in
    pp_ty ty ppf

end

(** Types that might introduce a series of type variable bindings. *)
module Poly_ty = struct

  type t =
    | Forall of Ty.Id.t * t
    | Ty of Ty.t

  let pp (pty : t) (ppf : Format.formatter) =
    let int_to_name i =
      let c, i = Char.chr (Char.code 'a' + i mod 26), i / 26 in
      if i = 0 then Format.sprintf "%c" c else Format.sprintf "%c%i" c i
    in
    let rec go i names pty =
      match pty with
      | Forall (id, body_ty) ->
          go (i + 1) ((id, int_to_name i) :: names) body_ty
      | Ty ty ->
          let lookup_name id = List.assoc id names in
          if i = 0 then Ty.pp ty lookup_name ppf else
            Format.fprintf ppf "@[<2>@[forall@ @[%a@].@]@ @[%t@]@]"
              (Format.pp_print_list Format.pp_print_string ~pp_sep:Format.pp_print_space)
              (List.rev_map snd names)
              (Ty.pp ty lookup_name)
    in
    go 0 [] pty

end

(** The main type checking algorithm *)
module Check : sig

  val infer : Expr.t -> (Poly_ty.t, string) result

end = struct

  exception Error of string

  type context = {
    ty_size : Ty.level;
    expr_tys : (string * Poly_ty.t) list;
  }

  let empty_ctx = {
    ty_size = 0;
    expr_tys = [];
  }

  let fresh_meta (ctx : context) : Ty.t =
    Ty.Meta (Ty.fresh_meta ctx.ty_size)

  let extend_ty (ctx : context) : context =
    { ctx with ty_size = 1 + ctx.ty_size }

  let extend_expr (ctx : context) (name : string) (ty : Poly_ty.t) : context =
    { ctx with expr_tys = (name, ty) :: ctx.expr_tys }

  let lookup_expr (ctx : context) (name : string) : Poly_ty.t =
    match List.assoc_opt name ctx.expr_tys with
    | Some ty -> ty
    | None -> raise (Error "unbound variable")

  let unify (ctx : context) (ty1 : Ty.t) (ty2 : Ty.t) =
    try Ty.unify ctx.ty_size ty1 ty2 with
    | Ty.Mismatched_types _ -> raise (Error "type mismatch")
    | Ty.Infinite_type -> raise (Error "infinite type")

  (** Convert a polytype to a monotype by replacing the bound type variables with
      fresh metavariables.

      For example, a polytype [∀ a b. a -> b] would be instantiated to a
      monotype [$m1 -> $m2].
  *)
  let instantiate (ctx : context) (pty : Poly_ty.t) : Ty.t =
    let rec go metas pty =
      match pty with
      | Poly_ty.Forall (ty_id, body_ty) -> go ((ty_id, fresh_meta ctx) :: metas) body_ty
      | Poly_ty.Ty ty -> Ty.subst ty metas
    in
    go [] pty

  (** Turn a monotype into a polytype by finding all the unsolved metavariables
      that have been introduced after the current level in the type environment,
      and binding them as parameters in a forall.

      For example, [$m1 -> $m2] would be generalise to [∀ a b. a -> b]. If [$m1]
      was introduced before the current level in the type environment, then
      [$m1 -> $m2] would generalise to [∀ a. $m1 -> a].
  *)
  let generalise (ctx : context) (ty : Ty.t) : Poly_ty.t =
    let module S = Set.Make (Ty.Id) in
    let rec ty_params ty =
      match ty with
      | Ty.Var _ -> S.empty
      | Ty.Unit -> S.empty
      | Ty.Fun (param_ty, body_ty) -> S.union (ty_params param_ty) (ty_params body_ty)
      | Ty.Meta { contents = Solved ty } -> ty_params ty
      | Ty.Meta ({ contents = Unsolved { id; level } } as m) when ctx.ty_size < level ->
          m := Solved (Var id);
          S.singleton id
      | Ty.Meta { contents = Unsolved _ } -> S.empty
    in
    ListLabels.fold_right (S.to_list (ty_params ty))
      ~init:(Poly_ty.Ty ty)
      ~f:(fun ty_id pty -> Poly_ty.Forall (ty_id, pty))

  let rec infer (ctx : context) (expr : Expr.t) : Ty.t =
    match expr with
    | Expr.Var x ->
        instantiate ctx (lookup_expr ctx x)
    | Expr.Let (x, def, body) ->
        let def_ty = infer (extend_ty ctx) def in
        infer (extend_expr ctx x (generalise ctx def_ty)) body
    | Expr.Fun_lit (x, body) ->
        let param_ty = fresh_meta ctx in
        let body_ty = infer (extend_expr ctx x (Poly_ty.Ty param_ty)) body in
        Ty.Fun (param_ty, body_ty)
    | Expr.Fun_app (fn, arg) ->
        let fn_ty = infer ctx fn in
        let arg_ty = infer ctx arg in
        let body_ty = fresh_meta ctx in
        unify ctx fn_ty (Ty.Fun (arg_ty, body_ty));
        body_ty
    | Expr.Unit_lit ->
        Ty.Unit

  let infer (expr : Expr.t) : (Poly_ty.t, string) result =
    try
      let ctx = empty_ctx in
      Ok (generalise ctx (infer (extend_ty ctx) expr))
    with
    | Error msg -> Error msg

end

let () = begin

  print_string "Running tests ...";

  let open Expr in

  let ( $ ) f x = Fun_app (f, x) in

  let run_infer expr =
    let ty = Check.infer expr |> Result.get_ok in
    Format.asprintf "%t" (Poly_ty.pp ty)
  in

  (* Examples *)
  begin

    (* id *)
    let id_expr = Fun_lit ("x", Var "x") in
    assert (run_infer id_expr = "forall a. a -> a");

    (* const *)
    let const_expr = Fun_lit ("x", Fun_lit ("y", Var "x")) in
    assert (run_infer const_expr = "forall a b. a -> b -> a");

    let expr =
      Let ("id", id_expr,
        Var "id" $ Unit_lit)
    in
    assert (run_infer expr = "Unit");

    let expr =
      Let ("id", id_expr,
        Let ("const", const_expr,
          Var "const" $ Var "id" $ Unit_lit $ Unit_lit))
    in
    assert (run_infer expr = "Unit");

  end;

  (* Generalisation tests. See https://okmij.org/ftp/ML/generalization.html#generalization
     for more details *)
  begin

    let expr =
      Fun_lit ("x",
        Let ("y", Fun_lit ("z", Var "z"),
          Var "y")) in
    assert (run_infer expr = "forall a b. a -> b -> b");

    (* Ensure that the unsound type [forall a b. a -> b] is not inferred.  *)
    let expr =
      Fun_lit ("x",
        (* At this point the typing context contains a single entry [x : $m0].
           To prevent [y] being assigned the unsound type [forall a. a] the
           definition should not be generalised over the metavariable [$m0] *)
        Let ("y", Var "x",
          Var "y")) in
    assert (run_infer expr = "forall a. a -> a");

    let expr =
      Fun_lit ("x",
        Let ("y", Fun_lit ("z", Var "x"),
          Var "y")) in
    assert (run_infer expr = "forall a b. a -> b -> a");

  end;

  (* Unsolved meta tests *)
  begin

    (* The following example will leave a metavariable unsolved for the of the
       second function. See “No Unification Variable Left Behind”
       https://doi.org/10.4230/LIPIcs.ITP.2023.8 for more information. *)
    let expr =
      Let ("x", Fun_lit ("f", Unit_lit) $ Fun_lit ("y", Var "y"),
        Var "x")
    in
    assert (run_infer expr = "Unit");

  end;

  print_string " ok!\n";

end
