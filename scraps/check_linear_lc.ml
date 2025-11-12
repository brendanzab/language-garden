(** A linear lambda calculus.

    We don’t have unrestricted bindings, only linear bindings, which implies
    that every binding is unique as well (see {{: https://doi.org/10.1007/978-3-030-99336-8_13}
    “Linearity and Uniqueness: An Entente Cordiale”} by Marshall et. al.).

    {2 Future ideas}

    It would be nice to try adding rudimentary borrow checking, similar to that
    which is described in {{: https://doi.org/10.1145/3764117} “From Linearity
    to Borrowing”} by Wagner et. al.

    I’d also be interested in experimenting with QTT-style typechecking - I’m
    curious if this would be cleaner than threading through input and output
    contexts.

    {2 Resources}

    - David Walker, “Chapter 1: A Linear Type System” in Advanced Topics in
      Types and Programming Languages
    - Frank Pfenning, {{: https://www.cs.cmu.edu/~fp/courses/15816-f01/lectures/lecture19.html}
      “Lecture 19: Linear Type Theory”}
*)

module Ty = struct

  type t =
    | Fun of t * t        (* Linear functions [t1 ⊸ t2] *)
    | Pair of t * t       (* Multiplicative conjunction [t1 ⊗ t2] *)
    | Either of t * t     (* Additive disjunction [t1 ⊕ t2]*)
    | Unit                (* Unit type *)
    [@@warning "-unused-constructor"]

  let pp (ty : t) (ppf : Format.formatter) =
    let rec pp_fun ty ppf =
      match ty with
      | Fun (ty1, ty2) -> Format.fprintf ppf "%t ⊸ %t" (pp_infix ty1) (pp_fun ty2)
      | ty -> pp_infix ty ppf
    and pp_infix ty ppf =
      match ty with
      | Pair (ty1, ty2) -> Format.fprintf ppf "%t ⊗ %t" (pp_atomic ty1) (pp_atomic ty2)
      | Either (ty1, ty2) -> Format.fprintf ppf "%t ⊕ %t" (pp_atomic ty1) (pp_atomic ty2)
      | ty -> pp_atomic ty ppf
    and pp_atomic ty ppf =
      match ty with
      | Unit -> Format.fprintf ppf "Unit"
      | Fun _ | Pair _ | Either _ as ty ->
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
    | Pair_let of string * string * t * t
    | Either_left of t
    | Either_right of t
    | Either_case of t * (string * t) * (string * t)
    | Unit_lit
    [@@warning "-unused-constructor"]

end

module Check : sig

  val check : Expr.t -> Ty.t -> (unit, string) result
  val infer : Expr.t -> (Ty.t, string) result

end = struct

  exception Type_error of string

  let type_error (type a b) (f : (b, Format.formatter, unit, a) format4) : b =
    Format.kasprintf (fun msg -> raise (Type_error msg)) f

  (** The number of times a binding has been used *)
  module Uses = struct

    type t =
      | Zero    (* Unused *)
      | One     (* Used once *)

  end

  type context = (string * Ty.t * Uses.t) list

  (* TODO: return the variable names that differ *)
  let equate_usages (ctx1 : context) (ctx2 : context) : bool =
    List.equal (fun (_, _, u1) (_, _, u2) -> u1 = u2) ctx1 ctx2

  let lookup (ctx : context) (x : string) : context * Ty.t =
    let rec go acc ctx =
      match ctx with
      | (x', ty, Uses.Zero) :: ctx when x = x' -> List.rev_append acc ((x', ty, Uses.One) :: ctx), ty
      | (x', _, Uses.One) :: _ when x = x' -> type_error "variable `%s` has already been used" x
      | entry :: ctx -> go (entry :: acc) ctx
      | [] -> type_error "unbound variable `%s`" x
    in
    go [] ctx

  let add_binding (x : string) (ty : Ty.t) (ctx : context) : context =
    (x, ty, Uses.Zero) :: ctx

  let drop_binding (ctx : context) : context =
    match ctx with
    | (_, _, Uses.One) :: ctx -> ctx
    | (x, _, Uses.Zero) :: _ -> type_error "unused variable `%s`" x
    | [] -> failwith "no bindings"

  let check_binder (ctx : context) (x : string) (ty : Ty.t) (k : context -> context) : context =
    k (ctx |> add_binding x ty) |> drop_binding

  let infer_binder (ctx : context) (x : string) (ty : Ty.t) (k : context -> context * Ty.t) : context * Ty.t =
    let ctx, ty = k (ctx |> add_binding x ty) in
    ctx |> drop_binding, ty

  let rec check (ctx : context) (expr : Expr.t) (ty : Ty.t) : context =
    match expr, ty with
    | Expr.Let (x, def, body), body_ty ->
        let ctx, def_ty = infer ctx def in
        check_binder ctx x def_ty @@ fun ctx ->
          check ctx body body_ty

    | Expr.Fun_lit (x, body), Ty.Fun (param_ty, body_ty) ->
        check_binder ctx x param_ty @@ fun ctx ->
          check ctx body body_ty

    | Expr.Pair_let (x1, x2, pair, body), body_ty ->
        begin match infer ctx pair with
        | ctx, Ty.Pair (ty1, ty2) ->
            check_binder ctx x1 ty1 @@ fun ctx ->
              check_binder ctx x2 ty2 @@ fun ctx ->
                check ctx body body_ty
        | _, ty -> type_error "expected pair, found: %t" (Ty.pp ty)
        end

    | Expr.Either_left expr, Ty.Either (left_ty, _) ->
        check ctx expr left_ty

    | Expr.Either_right expr, Ty.Either (_, right_ty) ->
        check ctx expr right_ty

    | Expr.Either_case (either, (x1, body1), (x2, body2)), body_ty ->
        begin match infer ctx either with
        | ctx, Ty.Either (ty1, ty2) ->
            let ctx1 = check_binder ctx x1 ty1 @@ fun ctx -> check ctx body1 body_ty in
            let ctx2 = check_binder ctx x2 ty2 @@ fun ctx -> check ctx body2 body_ty in
            if equate_usages ctx1 ctx2 then ctx1 else
              type_error "branches did not use the same variables"
        | _, ty -> type_error "expected either, found: %t" (Ty.pp ty)
        end

    | expr, ty ->
        let ctx, found_ty = infer ctx expr in
        if ty = found_ty then ctx else
          type_error "expected: %t, found: %t" (Ty.pp ty) (Ty.pp found_ty)

  and infer (ctx : context) (expr : Expr.t) : context * Ty.t =
    match expr with
    | Expr.Var x ->
        lookup ctx x

    | Expr.Let (x, def, body) ->
        let ctx, def_ty = infer ctx def in
        infer_binder ctx x def_ty @@ fun ctx ->
          infer ctx body

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

    | Expr.Pair_let (x1, x2, pair, body) ->
        begin match infer ctx pair with
        | ctx, Ty.Pair (ty1, ty2) ->
            infer_binder ctx x1 ty1 @@ fun ctx ->
              infer_binder ctx x2 ty2 @@ fun ctx ->
                infer ctx body
        | _, ty -> type_error "expected pair, found: %t" (Ty.pp ty)
        end

    | Expr.Unit_lit ->
        ctx, Ty.Unit

    | Expr.Fun_lit _
    | Expr.Either_left _
    | Expr.Either_right _
    | Expr.Either_case _ ->
        type_error "type annotations needed"

  let run (type a) (prog : unit -> context * a) : (a, string) result =
    match prog () with
    | [], x -> Ok x
    | _ :: _, _ -> failwith "unused bindings"
    | exception Type_error msg -> Error msg

  let check (expr : Expr.t) (ty : Ty.t) : (unit, string) result =
    run (fun () -> check [] expr ty, ())

  let infer (expr : Expr.t) : (Ty.t, string) result =
    run (fun () -> infer [] expr)

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
    let expr = Expr.Let ("id", Ann (id_expr, id_ty), Pair_lit (Var "id" $ Unit_lit, Var "id" $ Unit_lit)) in
    assert (Check.infer expr = Error "variable `id` has already been used");

    (* TODO: More tests *)

  end;

  print_string " ok!\n";

end
