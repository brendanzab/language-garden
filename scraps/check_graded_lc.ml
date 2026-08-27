(** A graded lambda calculus *)

(** Pre-ordered semiring *)
module type Capability = sig

  type t

  val add : t -> t -> t
  val mul : t -> t -> t

  val zero : t
  val one : t

  val lte : t -> t -> bool

  val pp : t -> Format.formatter -> unit

end

(** Unrestricted capability *)
module Unrestricted = struct

  type t = unit

  let add () () = ()
  let mul () () = ()
  let zero = ()
  let one = ()
  let lte () () = true

  let pp () = Format.dprintf "∞"

end

(** Exact usage capability *)
module Exact = struct

  type t = int

  let add = Int.add
  let mul = Int.mul
  let zero = Int.zero
  let one = Int.one
  let lte x y = Int.equal x y

  let pp x = Format.dprintf "%i" x

end

(** Linearity (none-one-tons) capability. Like QTT *)
module Lnl = struct

  type t = Zero | One | Inf

  let compare x y =
    match x, y with
    | Zero, Zero -> 0
    | Zero, (One | Inf) -> -1

    | One, Zero -> 1
    | One, One -> 0
    | One, Inf -> -1

    | Inf, (One | Zero) -> 1
    | Inf, Inf -> 0

  let add x y =
    match x, y with
    | Zero, x | x, Zero -> x
    | _, _ -> Inf

  let mul x y =
    match x, y with
    | Zero, x | x, Zero -> Zero
    | One, x | x, One -> x
    | _, _ -> Inf

  let zero = Zero
  let one = One

  let lte x y = compare x y < 0

  let pp x =
    match x with
    | Zero -> Format.dprintf "0"
    | One -> Format.dprintf "1"
    | Inf -> Format.dprintf "∞"

end

(** Information flow capability *)
module Security = struct

  type t = Low | High

  let join (* \/ *) x y =
    match x, y with
    | High, High -> failwith "TODO"
    | High, Low -> failwith "TODO"
    | Low, Low -> failwith "TODO"
    | Low, High -> failwith "TODO"

  let meet (* /\ *) x y =
    match x, y with
    | High, High -> failwith "TODO"
    | High, Low -> failwith "TODO"
    | Low, Low -> failwith "TODO"
    | Low, High -> failwith "TODO"

  let add x y = meet x y
  let mul x y = join x y

  let zero = High
  let one = Low

  let lte x y =
    match x, y with
    | High, High -> true
    | High, Low -> true
    | Low, Low -> true
    | Low, High -> false

  let pp x =
    match x with
    | Low -> Format.dprintf "L"
    | High -> Format.dprintf "H"

end

module Product (R : Capability) (S : Capability) = struct

  type t = R.t * S.t

  let add (rx, sx) (ry, sy) = R.add rx ry, S.add sx sy
  let mul (rx, sx) (ry, sy) = R.mul rx ry, S.mul sx sy
  let lte (rx, sx) (ry, sy) = R.lte rx ry, S.lte sx sy

  let zero = R.zero, S.zero
  let one = R.one, S.one

  let pp (r, s) =
    Format.dprintf "%t, %t" (R.pp r) (S.pp s)

end

module Ty (R : Capability) = struct

  type t =
    | Fun of t * R.t * t  (* Graded functions [t1 [r] -> t2] *)
    | Pair of t * t       (* Multiplicative conjunction [t1 ⊗ t2] *)
    | Either of t * t     (* Additive disjunction [t1 ⊕ t2]*)
    | Unit                (* Unit type *)
    | Int                 (* Integers *)
    | Ref of t            (* Memory references *)
    [@@warning "-unused-constructor"]

  let pp (ty : t) (ppf : Format.formatter) =
    let rec pp_fun ty ppf =
      match ty with
      | Fun (ty1, r, ty2) -> Format.fprintf ppf "%t [%t] ⊸ %t" (pp_infix ty1) (R.pp r) (pp_fun ty2)
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

module Expr (R : Capability) = struct

  module Ty = Ty (R)

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
    | Int_lit of int
    | Prim of [ `Alloc | `Free | `Swap ] * t list
    [@@warning "-unused-constructor"]

end

module Check (R : Capability) : sig

  module Ty : module type of Ty (R)
  module Expr : module type of Expr (R)

  val check : Expr.t -> Ty.t -> (unit, string) result
  val infer : Expr.t -> (Ty.t, string) result

end = struct

  module Ty = Ty (R)
  module Expr = Expr (R)

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
        let ctx, (ty1, ty2) = expect_pair ctx pair in
        let ctx = check (ctx |> Ctx.add x1 ty1 |> Ctx.add x2 ty2) body body_ty in
        ctx |> Ctx.remove |> Ctx.remove

    | Expr.Either_left expr, Ty.Either (left_ty, _) ->
        check ctx expr left_ty

    | Expr.Either_right expr, Ty.Either (_, right_ty) ->
        check ctx expr right_ty

    | Expr.Either_elim (either, (x1, body1), (x2, body2)), body_ty ->
        let ctx, (ty1, ty2) = expect_either ctx either in
        let ctx1 = check (ctx |> Ctx.add x1 ty1) body1 body_ty |> Ctx.remove in
        let ctx2 = check (ctx |> Ctx.add x2 ty2) body2 body_ty |> Ctx.remove in
        if Ctx.equate_usages ctx1 ctx2 then ctx1 else
          type_error "branches did not use the same variables"

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
        let ctx, (ty1, ty2) = expect_pair ctx pair in
        let ctx, body_ty = infer (ctx |> Ctx.add x1 ty1 |> Ctx.add x2 ty2) body in
        ctx |> Ctx.remove |> Ctx.remove, body_ty

    | Expr.Unit_lit ->
        ctx, Ty.Unit

    | Expr.Unit_elim (unit, body) ->
        let ctx = check ctx unit Ty.Unit in
        infer ctx body

    | Expr.Int_lit _ ->
        ctx, Ty.Int

    | Expr.Prim (`Alloc, [expr]) ->
        let ctx, ty = infer ctx expr in
        ctx, Ty.Ref ty

    | Expr.Prim (`Free, [expr]) ->
        let ctx, _ = expect_ref ctx expr in
        ctx, Ty.Unit

    | Expr.Prim (`Swap, [dst; src]) ->
        let ctx, dst_ty = expect_ref ctx dst in
        let ctx, src_ty = infer ctx src in
        ctx, Ty.Pair (Ty.Ref src_ty, dst_ty)

    | Expr.Prim _ ->
        type_error "mismatched arity"

    | Expr.Fun_lit _
    | Expr.Either_left _
    | Expr.Either_right _
    | Expr.Either_elim _ ->
        type_error "type annotations needed"

  and expect_ref (ctx : Ctx.t) (expr : Expr.t) : Ctx.t * Ty.t =
    match infer ctx expr with
    | ctx, Ty.Ref ty -> ctx, ty
    | _, ty -> type_error "expected: Ref _, found: %t" (Ty.pp ty)

  and expect_pair (ctx : Ctx.t) (expr : Expr.t) : Ctx.t * (Ty.t * Ty.t) =
    match infer ctx expr with
    | ctx, Ty.Pair (ty1, ty2) -> ctx, (ty1, ty2)
    | _, ty -> type_error "expected: _ ⊕ _, found: %t" (Ty.pp ty)

  and expect_either (ctx : Ctx.t) (expr : Expr.t) : Ctx.t * (Ty.t * Ty.t) =
    match infer ctx expr with
    | ctx, Ty.Either (ty1, ty2) -> ctx, (ty1, ty2)
    | _, ty -> type_error "expected: _ ⊗ _, found: %t" (Ty.pp ty)


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

  let run_tests (type a) (prog : (string -> (unit -> unit) -> unit) -> unit) : a =
    let success_count = ref 0 in
    let error_count = ref 0 in

    let run_test (name : string) (prog : unit -> unit) : unit =
      Printf.printf "test %s ... " name;

      match prog () with
      | () ->
          Printf.printf "ok\n";
          incr success_count
      | exception e ->
          Printf.printf "error:\n\n";
          Printf.printf "  %s\n\n" (Printexc.to_string e);
          String.split_on_char '\n' (Printexc.get_backtrace()) |> List.iter begin fun line ->
            Printf.printf "  %s\n" line;
          end;
          incr error_count
    in

    Printf.printf "Running tests in %s:\n\n" __FILE__;
    prog run_test;
    Printf.printf "\n";

    if !error_count <= 0 then begin
      Printf.printf "Ran %i successful tests\n\n" !success_count;
      exit 0
    end else begin
      Printf.printf "Failed %i out of %i tests\n\n" !error_count (!success_count + !error_count);
      exit 1
    end
  in

  let ( $ ) f x = Expr.Fun_app (f, x) in

  begin run_tests @@ fun test ->

    let id_expr = Expr.Fun_lit ("x", Var "x") in
    let id_ty = Ty.Fun (Unit, Unit) in

    begin test "id" @@ fun () ->
      assert (Check.check id_expr id_ty = Ok ());
    end;

    begin test "const" @@ fun () ->
      let const_expr = Expr.Fun_lit ("x", Fun_lit ("y", Var "x")) in
      let const_ty = Ty.Fun (Unit, Fun (Unit, Unit)) in

      assert (Check.check const_expr const_ty = Error "unused variable `y`");
    end;

    begin test "let binding" @@ fun () ->
      let expr = Expr.Let ("id", Ann (id_expr, id_ty), Var "id" $ Unit_lit) in
      assert (Check.infer expr = Ok Ty.Unit);
    end;

    begin test "let binding (unused definition)" @@ fun () ->
      let expr = Expr.Let ("id", Ann (id_expr, id_ty), Unit_lit) in
      assert (Check.infer expr = Error "unused variable `id`");
    end;

    (* We can only use functions once, which somewhat limits the utility of our language *)
    begin test "let binding (reused function definition)" @@ fun () ->
      let expr =
        Expr.Let ("id", Ann (id_expr, id_ty),
          Pair_lit (Var "id" $ Unit_lit, Var "id" $ Unit_lit))
      in
      assert (Check.infer expr = Error "variable `id` has already been used");
    end;

    begin test "alloc/free" @@ fun () ->
      let expr =
        Expr.Fun_lit ("v",
          Let ("x", Prim (`Alloc, [Var "v"]),
            Prim (`Free, [Var "x"])))
      in
      assert (Check.check expr (Ty.Fun (Int, Unit)) = Ok ());
    end;

    begin test "alloc/swap/free" @@ fun () ->
      let expr =
        Expr.Fun_lit ("v",
          Let ("x", Prim (`Alloc, [Var "v"]),
            Pair_elim ("x", "y", Prim (`Swap, [Var "x"; Unit_lit]),
              Unit_elim (Prim (`Free, [Var "x"]),
                Var "y"))))
      in
      assert (Check.check expr (Ty.Fun (Int, Int)) = Ok ());
    end;

    (* TODO: More tests *)

  end;

end
