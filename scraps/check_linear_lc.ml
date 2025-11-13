(** A linear lambda calculus that ensures each binding is used exactly once.

    In order to translate the declarative presentation of linear lambda
    calculus into an algorithm we use input and output contexts in a style
    similar to the one shown by David Walker in “Advanced Topics in Types and
    Programming Languages”.

    We don’t have unrestricted bindings, only linear bindings, which implies
    that every binding is unique as well (see {{: https://doi.org/10.1007/978-3-030-99336-8_13}
    “Linearity and Uniqueness: An Entente Cordiale”} by Marshall et. al.).

    {2 Future ideas}

    It would be nice to try adding rudimentary borrow checking, similar to that
    which is described in {{: https://doi.org/10.1145/3764117} “From Linearity
    to Borrowing”} by Wagner et. al.

    Adding some sort of copying (like in Rust) would be nice, e.g. to allow
    functions that don’t close over values to be used multiple times.

    I’d also be interested in experimenting with QTT-style typechecking - I’m
    curious if this would be cleaner than threading through input and output
    contexts.

    {2 Resources}

    - David Walker. 2002. A Linear Type System. In Benjamin C. Peirce (Ed).
      Advanced Topics in Types and Programming Languages. https://www.cis.upenn.edu/~bcpierce/attapl/
    - Frank Pfenning. 2001. Linear Type Theory.
      https://www.cs.cmu.edu/~fp/courses/15816-f01/handouts/lintt.pdf
*)

module Ty = struct

  type t =
    | Fun of t * t        (* Linear functions [t1 ⊸ t2] *)
    | Pair of t * t       (* Multiplicative conjunction [t1 ⊗ t2] *)
    | Either of t * t     (* Additive disjunction [t1 ⊕ t2]*)
    | Unit                (* Unit type *)
    | Int                 (* Integers *)
    | Ref of t            (* Memory references *)
    [@@warning "-unused-constructor"]

  let pp (ty : t) (ppf : Format.formatter) =
    let rec pp_fun ty ppf =
      match ty with
      | Fun (ty1, ty2) -> Format.fprintf ppf "%t ⊸ %t" (pp_infix ty1) (pp_fun ty2)
      | ty -> pp_infix ty ppf
    and pp_infix ty ppf =
      match ty with
      | Pair (ty1, ty2) -> Format.fprintf ppf "%t ⊗ %t" (pp_app ty1) (pp_app ty2)
      | Either (ty1, ty2) -> Format.fprintf ppf "%t ⊕ %t" (pp_app ty1) (pp_app ty2)
      | ty -> pp_app ty ppf
    and pp_app ty ppf =
      match ty with
      | Ref ty -> Format.fprintf ppf "Ref %t" (pp_atomic ty)
      | ty -> pp_atomic ty ppf
    and pp_atomic ty ppf =
      match ty with
      | Unit -> Format.fprintf ppf "Unit"
      | Int -> Format.fprintf ppf "Int"
      | Fun _ | Pair _ | Either _ | Ref _ as ty ->
          Format.fprintf ppf "(%t)" (pp_fun ty)
    in
    pp_fun ty ppf

end

module Expr = struct

  type t =
    | Var of string
    | Let of string * t * t
    | Ann of t * Ty.t
    | Fun_lit of string * t
    | Fun_app of t * t
    | Pair_lit of t * t
    | Pair_elim of string * string * t * t
    | Either_left of t
    | Either_right of t
    | Either_elim of t * (string * t) * (string * t)
    | Unit_lit
    | Unit_elim of t * t
    | Prim of [ `Alloc | `Free | `Swap ] * t list
    [@@warning "-unused-constructor"]

end

module Check : sig

  val check : Expr.t -> Ty.t -> (unit, string) result
  val infer : Expr.t -> (Ty.t, string) result

end = struct

  (** Error handling*)

  exception Type_error of string

  let type_error (type a b) (f : (b, Format.formatter, unit, a) format4) : b =
    Format.kasprintf (fun msg -> raise (Type_error msg)) f


  (** Typing contexts *)
  module Ctx : sig

    type t

    (** A context with no bindings in it *)
    val empty : t

    (** Add an unused binding to the context *)
    val add : string -> Ty.t -> t -> t

    (** Remove the most recently added binding from the context, raising a type
        error if it has not yet been used *)
    val remove : t -> t

    (** Lookup the type of an unused variable in the context, returning a new
        context with the binding marked as used *)
    val lookup : t -> string -> t * Ty.t

    (** [true] if there are no bindings in the context *)
    val is_empty : t -> bool

    (** [true] if both contexts have the same bindings marked as used *)
    val equate_usages : t -> t -> bool

  end = struct

    (** The number of times a binding has been used. *)
    type uses =
      | Zero    (* Unused *)
      | One     (* Used once *)

    type t = (string * Ty.t * uses) list

    let empty = []

    let is_empty (ctx : t) : bool =
      List.is_empty ctx

    let equate_usages (ctx1 : t) (ctx2 : t) : bool =
      (* TODO: return the variable names that differ to improve error reporting *)
      List.equal (fun (_, _, u1) (_, _, u2) -> u1 = u2) ctx1 ctx2

    let add (x : string) (ty : Ty.t) (ctx : t) : t =
      (x, ty, Zero) :: ctx

    let remove (ctx : t) : t =
      match ctx with
      | (_, _, One) :: ctx -> ctx
      | (x, _, Zero) :: _ -> type_error "unused variable `%s`" x
      | [] -> failwith "bug: no bindings left to remove"

    let lookup (ctx : t) (x : string) : t * Ty.t =
      let rec go acc ctx =
        match ctx with
        | (x', ty, Zero) :: ctx when x = x' -> List.rev_append acc ((x', ty, One) :: ctx), ty
        | (x', _, One) :: _ when x = x' -> type_error "variable `%s` has already been used" x
        | entry :: ctx -> go (entry :: acc) ctx
        | [] -> type_error "unbound variable `%s`" x
      in
      go [] ctx

  end


  (** Bidirectional type checking *)

  let rec check (ctx : Ctx.t) (expr : Expr.t) (ty : Ty.t) : Ctx.t =
    match expr, ty with
    | Expr.Let (x, def, body), body_ty ->
        let ctx, def_ty = infer ctx def in
        let ctx = check (ctx |> Ctx.add x def_ty) body body_ty in
        ctx |> Ctx.remove

    | Expr.Fun_lit (x, body), Ty.Fun (param_ty, body_ty) ->
        let ctx = check (ctx |> Ctx.add x param_ty) body body_ty in
        ctx |> Ctx.remove

    | Expr.Pair_elim (x1, x2, pair, body), body_ty ->
        begin match infer ctx pair with
        | ctx, Ty.Pair (ty1, ty2) ->
            let ctx = check (ctx |> Ctx.add x1 ty1 |> Ctx.add x2 ty2) body body_ty in
            ctx |> Ctx.remove |> Ctx.remove
        | _, ty -> type_error "expected: _ ⊕ _, found: %t" (Ty.pp ty)
        end

    | Expr.Either_left expr, Ty.Either (left_ty, _) ->
        check ctx expr left_ty

    | Expr.Either_right expr, Ty.Either (_, right_ty) ->
        check ctx expr right_ty

    | Expr.Either_elim (either, (x1, body1), (x2, body2)), body_ty ->
        begin match infer ctx either with
        | ctx, Ty.Either (ty1, ty2) ->
            let ctx1 = check (ctx |> Ctx.add x1 ty1) body1 body_ty |> Ctx.remove in
            let ctx2 = check (ctx |> Ctx.add x2 ty2) body2 body_ty |> Ctx.remove in
            if Ctx.equate_usages ctx1 ctx2 then ctx1 else
              type_error "branches did not use the same variables"
        | _, ty -> type_error "expected: _ ⊗ _, found: %t" (Ty.pp ty)
        end

    | Expr.Unit_elim (unit, body), body_ty ->
        let ctx = check ctx unit Ty.Unit in
        check ctx body body_ty

    | expr, ty ->
        let ctx, found_ty = infer ctx expr in
        if ty = found_ty then ctx else
          type_error "expected: %t, found: %t" (Ty.pp ty) (Ty.pp found_ty)

  and infer (ctx : Ctx.t) (expr : Expr.t) : Ctx.t * Ty.t =
    match expr with
    | Expr.Var x ->
        Ctx.lookup ctx x

    | Expr.Let (x, def, body) ->
        let ctx, def_ty = infer ctx def in
        let ctx, body_ty = infer (ctx |> Ctx.add x def_ty) body in
        ctx |> Ctx.remove, body_ty

    | Expr.Ann (expr, ty) ->
        check ctx expr ty, ty

    | Expr.Fun_app (fn, arg) ->
        begin match infer ctx fn with
        | ctx, Ty.Fun (param_ty, body_ty) -> check ctx arg param_ty, body_ty
        | _, _ -> type_error "unexpected argument"
        end

    | Expr.Pair_lit (expr1, expr2) ->
        let ctx, ty1 = infer ctx expr1 in
        let ctx, ty2 = infer ctx expr2 in
        ctx, Ty.Pair (ty1, ty2)

    | Expr.Pair_elim (x1, x2, pair, body) ->
        begin match infer ctx pair with
        | ctx, Ty.Pair (ty1, ty2) ->
            let ctx, body_ty = infer (ctx |> Ctx.add x1 ty1 |> Ctx.add x2 ty2) body in
            ctx |> Ctx.remove |> Ctx.remove, body_ty
        | _, ty -> type_error "expected: _ ⊗ _, found: %t" (Ty.pp ty)
        end

    | Expr.Unit_lit ->
        ctx, Ty.Unit

    | Expr.Unit_elim (unit, body) ->
        let ctx = check ctx unit Ty.Unit in
        infer ctx body

    | Expr.Prim (`Alloc, [expr]) ->
        let ctx, ty = infer ctx expr in
        ctx, Ty.Ref ty

    | Expr.Prim (`Free, [expr]) ->
        begin match infer ctx expr with
        | ctx, Ty.Ref _ -> ctx, Ty.Unit
        | _, ty -> type_error "expected: Ref _, found: %t" (Ty.pp ty)
        end

    | Expr.Prim (`Swap, [dst; src]) ->
        let ctx, dst_ty =
          match infer ctx dst with
          | ctx, Ty.Ref ty -> ctx, ty
          | _, ty -> type_error "expected: Ref _, found: %t" (Ty.pp ty)
        in
        let ctx, src_ty = infer ctx src in
        ctx, Ty.Pair (Ty.Ref src_ty, dst_ty)

    | Expr.Prim _ ->
        type_error "mismatched arity"

    | Expr.Fun_lit _
    | Expr.Either_left _
    | Expr.Either_right _
    | Expr.Either_elim _ ->
        type_error "type annotations needed"


  (** Running the type checker *)

  let run (type a) (prog : unit -> Ctx.t * a) : (a, string) result =
    match prog () with
    | ctx, x when Ctx.is_empty ctx -> Ok x
    | _, _ -> failwith "bug: unused bindings remaining after typechecking"
    | exception Type_error msg -> Error msg


  (** Public API *)

  let check (expr : Expr.t) (ty : Ty.t) : (unit, string) result =
    run (fun () -> check Ctx.empty expr ty, ())

  let infer (expr : Expr.t) : (Ty.t, string) result =
    run (fun () -> infer Ctx.empty expr)

end

let () = begin

  Printexc.record_backtrace true;

  print_string "Running tests ...";

  let ( $ ) f x = Expr.Fun_app (f, x) in

  begin

    (* id *)
    let id_expr = Expr.Fun_lit ("x", Var "x") in
    let id_ty = Ty.Fun (Unit, Unit) in
    assert (Check.check id_expr id_ty = Ok ());

    (* const *)
    let const_expr = Expr.Fun_lit ("x", Fun_lit ("y", Var "x")) in
    let const_ty = Ty.Fun (Unit, Fun (Unit, Unit)) in
    assert (Check.check const_expr const_ty = Error "unused variable `y`");

    let expr = Expr.Let ("id", Ann (id_expr, id_ty), Unit_lit) in
    assert (Check.infer expr = Error "unused variable `id`");

    let expr = Expr.Let ("id", Ann (id_expr, id_ty), Var "id" $ Unit_lit) in
    assert (Check.infer expr = Ok Ty.Unit);

    (* We can only use functions once, haha *)
    let expr =
      Expr.Let ("id", Ann (id_expr, id_ty),
        Pair_lit (Var "id" $ Unit_lit, Var "id" $ Unit_lit))
    in
    assert (Check.infer expr = Error "variable `id` has already been used");

    let expr =
      Expr.Fun_lit ("v",
        Let ("x", Prim (`Alloc, [Var "v"]),
          Prim (`Free, [Var "x"])))
    in
    assert (Check.check expr (Ty.Fun (Int, Unit)) = Ok ());

    let expr =
      Expr.Fun_lit ("v",
        Let ("x", Prim (`Alloc, [Var "v"]),
          Pair_elim ("x", "y", Prim (`Swap, [Var "x"; Unit_lit]),
            Unit_elim (Prim (`Free, [Var "x"]),
              Var "y"))))
    in
    assert (Check.check expr (Ty.Fun (Int, Int)) = Ok ());

    (* TODO: More tests *)

  end;

  print_string " ok!\n";

end
