(** Graded lambda calculus with coeffects for tracking variable usage,
    information flow, etc.

    The bidirectional typechecking algorithm is based on the declarative rules
    for the “Graded Modal Core” language (Liepelt, Marshall and Orchard 2026).

    - Vilem Liepelt, Danielle Marshall, and Dominic Orchard. 2026.
      Same Coeffect, Different Base: Connecting Two Dominant Approaches to Graded Types.
      https://doi.org/10.1145/3828697
    - Tomas Petricek, Coeffects: Context-aware programming languages.
      https://tomasp.net/coeffects/
*)

module List = struct

  include List

  (** Extensions to {!Stdlib.List} *)

  let uncons (type a) (xs : a list) : a * a list =
    match xs with
    | x :: xs -> x, xs
    | [] -> invalid_arg "List.uncons"

  let uncons2 (type a) (xs : a list) : a * a * a list =
    match xs with
    | x :: x' :: xs -> x, x', xs
    | _ :: _ | [] -> invalid_arg "List.uncons2"

end


(** Pre-ordered semiring, used to grade function arrows *)
module Grade = struct

  module type S = sig

    type t

    val add : t -> t -> t
    val mul : t -> t -> t

    val zero : t
    val one : t

    val lte : t -> t -> bool
    val max : t -> t -> t

    (** The [max] operation does not appear in the paper, and is added to allow
        for the approximation of grades in our type inference algorithm. *)

    val pp : t -> Format.formatter -> unit

  end

  module Unrestricted = struct

    type t = unit

    let add () () = ()
    let mul () () = ()
    let zero = ()
    let one = ()
    let lte () () = true
    let max () () = ()

    let pp () = Format.dprintf "ω"

  end

  module Exact = struct

    type t = int

    let add = Int.add
    let mul = Int.mul
    let zero = Int.zero
    let one = Int.one
    let lte x y = Int.equal x y

    let max x y =
      if Int.equal x y then x else
        failwith "TODO" (* FIXME: requires a partial max function *)

    let pp x = Format.dprintf "%i" x

  end

  module Linearity = struct

    type t = Zero | One | Many

    let add x y =
      match x, y with
      | Zero, x | x, Zero -> x
      | _, _ -> Many

    let mul x y =
      match x, y with
      | Zero, x | x, Zero -> Zero
      | One, x | x, One -> x
      | _, _ -> Many

    let zero = Zero
    let one = One

    let lte x y =
      match x, y with
      | Zero, Many -> true
      | One, Many -> true
      | x, y -> x = y

    let max x y =
      match x, y with
      | Zero, One -> Many
      | One, Zero -> Many
      | x, y -> if lte x y then y else x

    let pp x =
      match x with
      | Zero -> Format.dprintf "0"
      | One -> Format.dprintf "1"
      | Many -> Format.dprintf "ω"

  end

  module Security = struct

    type t = High | Low

    let add x y =
      match x, y with
      | High, High -> High
      | _, _ -> Low

    let mul x y =
      match x, y with
      | Low, Low -> Low
      | _, _ -> High

    let zero = High
    let one = Low

    let lte x y =
      match x, y with
      | High, High -> true
      | High, Low -> true
      | Low, High -> false
      | Low, Low -> true

    let max x y =
      match x, y with
      | High, High -> High
      | High, Low -> Low
      | Low, High -> Low
      | Low, Low -> Low

    let pp x =
      match x with
      | High -> Format.dprintf "Hi"
      | Low -> Format.dprintf "Lo"

  end

  (** Interval, from Definition 4.3 of {{: https://doi.org/10.1145/3341714}
      Orchard et. al. 2019}. *)
  module Interval (R : S) = struct

    type t = R.t * R.t

    let add (x1, x2) (y1, y2) = R.add x1 y1, R.add x2 y2
    let mul (x1, x2) (y1, y2) = failwith "TODO"
    let zero = R.zero, R.zero
    let one = R.one, R.one

    let lte (x1, x2) (y1, y2) =
      (* The swapped order of the left part of the conjunction is important! *)
      R.lte y1 x1 && R.lte x2 y2

    let max (x1, x2) (y1, y2) =
      failwith "TODO"

    let pp (x, y) =
      Format.dprintf "(%t..%t)" (R.pp x) (R.pp y)

  end

  module Pair (R : S) (S : S) = struct

    type t = R.t * S.t

    let add (rx, sx) (ry, sy) = R.add rx ry, S.add sx sy
    let mul (rx, sx) (ry, sy) = R.mul rx ry, S.mul sx sy
    let lte (rx, sx) (ry, sy) = R.lte rx ry && S.lte sx sy
    let max (rx, sx) (ry, sy) = R.max rx ry, S.max sx sy

    let zero = R.zero, S.zero
    let one = R.one, S.one

    let pp (r, s) =
      Format.dprintf "%t, %t" (R.pp r) (S.pp s)

  end

  module _ : S = Unrestricted
  module _ : S = Exact
  module _ : S = Linearity
  module _ : S = Security
  module _  (R : S) : S = Interval (R)
  module _  (R : S) (S : S) : S = Pair (R) (S)

end


module Core (R : Grade.S) = struct

  type var = string

  type ty (* t *) =
    | Fun of ty * R.t * ty      (* t % r -> t *)
    | Box of ty * R.t           (* t [r] *)
    | Pair of ty * ty           (* t × t *)
    | Either of ty * ty         (* t + t *)
    | Bool                      (* Bool *)
    | Unit                      (* Unit *)

  let rec pp_ty (t : ty) =
    let rec pp_infix_ty (t : ty) =
      match t with
      | Either (t1, t2) ->
          let rec go (t : ty) =
            match t with
            | Either (t1, t2) -> Format.dprintf "%t + %t" (pp_atom_ty t1) (go t2)
            | t -> pp_atom_ty t
          in
          go t
      | Pair (t1, t2) ->
          let rec go (t : ty) =
            match t with
            | Pair (t1, t2) -> Format.dprintf "%t × %t" (pp_atom_ty t1) (go t2)
            | t -> pp_atom_ty t
          in
          go t
      | t -> pp_atom_ty t
    and pp_atom_ty (t : ty) =
      match t with
      | Box (t, r) -> Format.dprintf "%t [%t]" (pp_atom_ty t) (R.pp r)
      | Bool -> Format.dprintf "Bool"
      | Unit -> Format.dprintf "Unit"
      | t -> Format.dprintf "(%t)" (pp_ty t)
    in
    match t with
    | Fun (t1, r, t2) ->
        Format.dprintf "%t %% %t -> %t" (pp_atom_ty t1) (R.pp r) (pp_ty t2)
    | t -> pp_infix_ty t

  type expr (* e *) =
    | Var of var                                        (* x *)
    | Ann of expr * ty                                  (* e : t *)
    | Let of var * expr * expr                          (* let x := e in e *)
    | Fun_intro of var * expr                           (* λx. e *)
    | Fun_app of expr * expr                            (* e e *)
    | Box_intro of expr                                 (* [e] *)
    | Box_elim of var * expr * expr                     (* let [x] := e in e *)
    | Pair_intro of expr * expr                         (* (e, e) *)
    | Pair_elim of var * var * expr * expr              (* let (x, x) := e in e *)
    | Either_left of expr                               (* inl e *)
    | Either_right of expr                              (* inr e *)
    | Either_elim of expr * (var * expr) * (var * expr) (* case e of inl x -> e | inr x -> e *)
    | Unit_intro                                        (* () *)
    | Unit_elim of expr * expr                          (* let () := e in e *)
    | Bool_true                                         (* true *)
    | Bool_false                                        (* false *)
    | Bool_if of expr * expr * expr                     (* if e then e else e *)


  module Validate : sig

    val check : expr -> ty -> (unit, var) result
    val infer : expr -> (ty, var) result

  end = struct

    (** Type errors *)

    exception Type_error of string

    let type_error (type a b) : (a, Format.formatter, unit, b) format4 -> a =
      fun fmt -> Format.kasprintf (fun msg -> raise (Type_error msg)) fmt


    (** Typing context *)

    type ctx = (var * ty) list


    (** Grade context *)

    type rctx = R.t list

    let add_rctx (rctx1 : rctx) (rctx2 : rctx) : rctx =
      List.map2 R.add rctx1 rctx2

    let scale_rctx (r : R.t) (rctx : rctx) : rctx =
      List.map (R.mul r) rctx

    let max_rctx (rctx1 : rctx) (rctx2 : rctx) : rctx =
      List.map2 R.max rctx1 rctx2


    (** Bidirectional typing *)

    (** Lookup a variable in the context, returning its type along with a weakened
        usage context *)
    let rec lookup (ctx : ctx) (x : string) : ty * rctx =
      (* Return a grade context with 1 grade for the used variable, and 0 grade
        for all the other variables *)
      match ctx with
      | (y, t) :: ctx when x = y -> t, R.one :: List.map (Fun.const R.zero) ctx
      | (_, _) :: ctx -> Pair.map_snd (List.cons R.zero) (lookup ctx x)
      | [] -> type_error "unbound variable %s" x

    let rec check (ctx : ctx) (e : expr) (t : ty) : rctx =
      match e, t with
      | Let (x, e1, e2), t2 ->
          let t1, rctx1 = infer ctx e1 in
          let _, rctx2 = check ((x, t1) :: ctx) e2 t2 |> List.uncons in
          add_rctx rctx1 rctx2

      | Fun_intro (x, e), Fun (t1, r, t2) ->
          let r', rctx = check ((x, t1) :: ctx) e t2 |> List.uncons in
          if R.lte r' r then rctx else
            type_error "grade mismatch: expected %t, found %t" (R.pp r) (R.pp r')

      | Box_intro e, Box (t, r) ->
          let rctx = check ctx e t in
          scale_rctx r rctx

      | Box_elim (x, e1, e2), t2 ->
          let (t1, r), rctx1 = infer_box ctx e1 in
          let r', rctx2 = check ((x, t1) :: ctx) e2 t2 |> List.uncons in
          if R.lte r' r then add_rctx rctx1 rctx2 else
            type_error "grade mismatch: expected %t, found %t" (R.pp r) (R.pp r')

      | Pair_elim (x, y, e1, e2), t3 ->
          let (t1, t2), rctx1 = infer_pair ctx e1 in
          let r2, r1, rctx2 = check ((y, t2) :: (x, t1) :: ctx) e2 t3 |> List.uncons2 in
          add_rctx (scale_rctx (R.max r1 r2) rctx1) rctx2

      | Either_left e, Either (t1, _) ->
          check ctx e t1

      | Either_right e, Either (_, t2) ->
          check ctx e t2

      | Either_elim (e1, (x, e2), (y, e3)), t3 ->
          let (t1, t2), rctx1 = infer_either ctx e1 in
          let r1, rctx2 = check ((x, t1) :: ctx) e2 t3 |> List.uncons in
          let r2, rctx3 = check ((y, t2) :: ctx) e3 t3 |> List.uncons in
          add_rctx (scale_rctx (R.max r1 r2) rctx1) (max_rctx rctx2 rctx3)

      | Bool_if (e1, e2, e3), t ->
          let rctx1 = check ctx e1 Bool in
          let rctx2 = check ctx e2 t in
          let rctx3 = check ctx e3 t in
          add_rctx rctx1 (* FIXME: multiply by r? *) (max_rctx rctx2 rctx3)

      | Unit_elim (e1, e2), t ->
          let rctx1 = check ctx e1 Unit in
          let rctx2 = check ctx e2 t in
          add_rctx rctx1 (* FIXME: multiply by r? *) rctx2

      | e, t ->
          let t', rctx = infer ctx e in
          if t' = t then rctx else
            type_error "type mismatch: expected %t, found %t" (pp_ty t) (pp_ty t')

    and infer (ctx : ctx) (e : expr) : ty * rctx =
      match e with
      | Var x ->
          lookup ctx x

      | Ann (e, t) ->
          t, check ctx e t

      | Let (x, e1, e2) ->
          let t1, rctx1 = infer ctx e1 in
          let t2, (_, rctx2) = infer ((x, t1) :: ctx) e2 |> Pair.map_snd List.uncons in
          t2, add_rctx rctx1 rctx2

      | Fun_app (e1, e2) ->
          let (t1, r, t2), rctx1 = infer_fun ctx e1 in
          let rctx2 = check ctx e2 t1 in
          t2, add_rctx rctx1 (scale_rctx r rctx2)

      | Box_elim (x, e1, e2) ->
          let (t1, r), rctx1 = infer_box ctx e1 in
          let t2, (r', rctx2) = infer ((x, t1) :: ctx) e2 |> Pair.map_snd List.uncons in
          if R.lte r' r then t2, add_rctx rctx1 rctx2 else
            type_error "grade mismatch: expected %t, found %t" (R.pp r) (R.pp r')

      | Pair_intro (e1, e2) ->
          let t1, rctx1 = infer ctx e1 in
          let t2, rctx2 = infer ctx e2 in
          Pair (t1, t2), add_rctx rctx1 rctx2

      | Pair_elim (x, y, e1, e2) ->
          let (t1, t2), rctx1 = infer_pair ctx e1 in
          let t, (r2, r1, rctx2) =
            infer ((y, t2) :: (x, t1) :: ctx) e2 |> Pair.map_snd List.uncons2
          in
          t, add_rctx (scale_rctx (R.max r1 r2) rctx1) rctx2

      | Bool_true | Bool_false ->
          Bool, List.map (Fun.const R.zero) ctx

      | Unit_intro ->
          Unit, List.map (Fun.const R.zero) ctx

      | Unit_elim (e1, e2) ->
          let rctx1 = check ctx e1 Unit in
          let t, rctx2 = infer ctx e2 in
          t, add_rctx rctx1 (* FIXME: multiply by r? *) rctx2

      | Fun_intro _
      | Box_intro _
      | Either_left _
      | Either_right _
      | Either_elim _
      | Bool_if _ ->
          type_error "type annotations needed"

    and infer_fun (ctx : ctx) (e : expr) : (ty * R.t * ty) * rctx =
      match infer ctx e with
      | Fun (t1, r, t2), rctx -> (t1, r, t2), rctx
      | t, _ -> type_error "type mismatch: expected function, found %t" (pp_ty t)

    and infer_box (ctx : ctx) (e : expr) : (ty * R.t) * rctx =
      match infer ctx e with
      | Box (t, r), rctx -> (t, r), rctx
      | t, _ -> type_error "type mismatch: expected box, found %t" (pp_ty t)

    and infer_pair (ctx : ctx) (e : expr) : (ty * ty) * rctx =
      match infer ctx e with
      | Pair (t1, t2), rctx -> (t1, t2), rctx
      | t, _ -> type_error "type mismatch: expected pair, found %t" (pp_ty t)

    and infer_either (ctx : ctx) (e : expr) : (ty * ty) * rctx =
      match infer ctx e with
      | Either (t1, t2), rctx -> (t1, t2), rctx
      | t, _ -> type_error "type mismatch: expected either, found %t" (pp_ty t)


    (** Public API *)

    let check (e : expr) (t : ty) : (unit, string) result =
      match check [] e t with
      | rctx -> assert (List.is_empty rctx); Ok ()
      | exception Type_error msg -> Error msg

    let infer (e : expr) : (ty, string) result =
      match infer [] e with
      | t, rctx -> assert (List.is_empty rctx); Ok t
      | exception Type_error msg -> Error msg

  end

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

  begin run_tests @@ fun test ->

    (* Unrestricted grade *)

    begin

      let module R = Grade.Unrestricted in
      let open Core (Grade.Unrestricted) in

      (* let ( $ ) f x = Fun_app (f, x) in *)


      begin test "unrestricted: print either" @@ fun () ->
        assert (Format.asprintf "%t" (pp_ty (Either (Unit, Bool))) = "Unit + Bool");
      end;

      begin test "unrestricted: print either nested" @@ fun () ->
        assert (Format.asprintf "%t" (pp_ty (Either (Pair (Unit, Bool), Pair (Unit, Bool)))) =
          "(Unit × Bool) + (Unit × Bool)");
      end;

      begin test "unrestricted: print pair" @@ fun () ->
        assert (Format.asprintf "%t" (pp_ty (Pair (Unit, Bool))) = "Unit × Bool");
      end;

      begin test "unrestricted: print pair nested" @@ fun () ->
        assert (Format.asprintf "%t" (pp_ty (Pair (Either (Unit, Bool), Either (Unit, Bool)))) =
          "(Unit + Bool) × (Unit + Bool)");
      end;

      begin test "unrestricted: print box" @@ fun () ->
        assert (Format.asprintf "%t" (pp_ty (Box (Unit, ()))) =
          "Unit [ω]");
      end;

      begin test "unrestricted: print box either" @@ fun () ->
        assert (Format.asprintf "%t" (pp_ty (Box (Either (Unit, Bool), ()))) =
          "(Unit + Bool) [ω]");
      end;

      begin test "unrestricted: print fun" @@ fun () ->
        assert (Format.asprintf "%t" (pp_ty (Fun (Unit, (), Fun (Unit, (), Unit)))) =
          "Unit % ω -> Unit % ω -> Unit");
      end;


      let id_ty = Fun (Unit, (), Unit) in
      let id_expr = Fun_intro ("x", Var "x") in
      let id_expr_ignore = Fun_intro ("x", Unit_intro) in

      begin test "unrestricted: id" @@ fun () ->
        Validate.check id_expr id_ty |> Result.error_to_failure;
      end;

      begin test "unrestricted: id ignore" @@ fun () ->
        Validate.check id_expr_ignore id_ty |> Result.error_to_failure;
      end;

      begin test "unrestricted: either left" @@ fun () ->
        Validate.check (Either_left Unit_intro) (Either (Unit, Bool)) |> Result.error_to_failure;
      end;

      begin test "unrestricted: either right" @@ fun () ->
        Validate.check (Either_right Bool_true) (Either (Unit, Bool)) |> Result.error_to_failure;
      end;

      begin test "unrestricted: either elim" @@ fun () ->
        let expr =
          Either_elim (Ann (Either_right Bool_true, Either (Unit, Bool)),
            ("x", Unit_elim (Var "x", Bool_true)),
            ("x", Var "x"))
        in
        Validate.check expr Bool |> Result.error_to_failure;
      end;

    end;

    (* Linearity grade *)

    begin

      let module R = Grade.Linearity in
      let open Core (Grade.Linearity) in

      (* let ( $ ) f x = Fun_app (f, x) in *)

      let id_ty r = Fun (Unit, r, Unit) in
      let id_expr = Fun_intro ("x", Var "x") in
      let id_expr_ignore = Fun_intro ("x", Unit_intro) in

      begin test "linear: id 0" @@ fun () ->
        assert (Validate.check id_expr (id_ty R.Zero) |> Result.is_error);
      end;

      begin test "linear: id 1" @@ fun () ->
        Validate.check id_expr (id_ty R.One) |> Result.error_to_failure;
      end;

      begin test "linear: id ω" @@ fun () ->
        Validate.check id_expr (id_ty R.Many) |> Result.error_to_failure;
      end;

      begin test "linear: id 0 ignore" @@ fun () ->
        Validate.check id_expr_ignore (id_ty R.Zero) |> Result.error_to_failure;
      end;

      begin test "linear: id 1 ignore" @@ fun () ->
        assert (Validate.check id_expr_ignore (id_ty R.One) |> Result.is_error);
      end;

      begin test "linear: id ω ignore" @@ fun () ->
        Validate.check id_expr_ignore (id_ty R.Many) |> Result.error_to_failure;
      end;

      begin test "linear: if branches" @@ fun () ->

        let ty = Fun (Bool, R.One, Fun (Unit, R.One, Unit)) in
        let expr =
          Fun_intro ("b", Fun_intro ("x",
            Bool_if (Var "b", Var "x", Var "x")))
        in

        Validate.check expr ty |> Result.error_to_failure;

      end;

      begin test "linear: if branches ignore left 1" @@ fun () ->

        let ty = Fun (Bool, R.One, Fun (Unit, R.One, Unit)) in
        let expr =
          Fun_intro ("b", Fun_intro ("x",
            Bool_if (Var "b", Unit_intro, Var "x")))
        in

        assert (Validate.check expr ty |> Result.is_error);

      end;

      begin test "linear: if branches ignore right 1" @@ fun () ->

        let ty = Fun (Bool, R.One, Fun (Unit, R.One, Unit)) in
        let expr =
          Fun_intro ("b", Fun_intro ("x",
            Bool_if (Var "b", Var "x", Unit_intro)))
        in

        assert (Validate.check expr ty |> Result.is_error);

      end;

      begin test "linear: if branches ignore left many" @@ fun () ->

        let ty = Fun (Bool, R.One, Fun (Unit, R.Many, Unit)) in
        let expr =
          Fun_intro ("b", Fun_intro ("x",
            Bool_if (Var "b", Unit_intro, Var "x")))
        in

        Validate.check expr ty |> Result.error_to_failure;

      end;

      begin test "linear: if branches ignore right many" @@ fun () ->

        let ty = Fun (Bool, R.One, Fun (Unit, R.Many, Unit)) in
        let expr =
          Fun_intro ("b", Fun_intro ("x",
            Bool_if (Var "b", Var "x", Unit_intro)))
        in

        Validate.check expr ty |> Result.error_to_failure;

      end;

      let box_ignore = Fun_intro ("x", Box_elim ("y", Var "x", Bool_false)) in
      let box_use = Fun_intro ("x", Box_elim ("y", Var "x", Var "y")) in
      let box_dup = Fun_intro ("x", Box_elim ("y", Var "x", Pair_intro (Var "y", Var "y"))) in

      begin test "linear: box 0 ignore" @@ fun () ->

        let ty = Fun (Box (Bool, R.Zero), R.One, Bool) in
        Validate.check box_ignore ty |> Result.error_to_failure;

      end;

      begin test "linear: box 1 ignore" @@ fun () ->

        let ty = Fun (Box (Bool, R.One), R.One, Bool) in
        assert (Validate.check box_ignore ty |> Result.is_error);

      end;

      begin test "linear: box 0 use" @@ fun () ->

        let ty = Fun (Box (Bool, R.Zero), R.One, Bool) in
        assert (Validate.check box_use ty |> Result.is_error);

      end;

      begin test "linear: box 1 use" @@ fun () ->

        let ty = Fun (Box (Bool, R.One), R.One, Bool) in
        Validate.check box_use ty |> Result.error_to_failure;

      end;

      begin test "linear: box 1 dup" @@ fun () ->

        let ty = Fun (Box (Bool, R.One), R.One, Pair (Bool, Bool)) in
        assert (Validate.check box_dup ty |> Result.is_error);

      end;

      begin test "linear: box ω dup" @@ fun () ->

        let ty = Fun (Box (Bool, R.Many), R.One, Pair (Bool, Bool)) in
        Validate.check box_dup ty |> Result.error_to_failure;

      end;

    end;

    (* Security grade *)

    begin

      let module R = Grade.Security in
      let open Core (Grade.Security) in

      (* let ( $ ) f x = Fun_app (f, x) in *)

      let id_ty r = Fun (Unit, r, Unit) in
      let id_expr = Fun_intro ("x", Var "x") in
      let id_expr_ignore = Fun_intro ("x", Unit_intro) in

      begin test "security: id Hi" @@ fun () ->
        assert (Validate.check id_expr (id_ty R.High) |> Result.is_error);
      end;

      begin test "security: id Lo" @@ fun () ->
        Validate.check id_expr (id_ty R.Low) |> Result.error_to_failure;
      end;

      begin test "security: id Hi ignore" @@ fun () ->
        Validate.check id_expr_ignore (id_ty R.High) |> Result.error_to_failure;
      end;

      begin test "security: id Lo ignore" @@ fun () ->
        Validate.check id_expr_ignore (id_ty R.Low) |> Result.error_to_failure;
      end;

      begin test "linear: if branches Hi" @@ fun () ->

        let ty = Fun (Bool, R.High, Fun (Unit, R.Low, Unit)) in
        let expr =
          Fun_intro ("b", Fun_intro ("x",
            Bool_if (Var "b", Var "x", Var "x")))
        in

        assert (Validate.check expr ty |> Result.is_error);

      end;

      begin test "linear: if branches Low" @@ fun () ->

        let ty = Fun (Bool, R.Low, Fun (Unit, R.Low, Unit)) in
        let expr =
          Fun_intro ("b", Fun_intro ("x",
            Bool_if (Var "b", Var "x", Var "x")))
        in

        Validate.check expr ty |> Result.error_to_failure;

      end;

      begin test "linear: boxed if branches Hi" @@ fun () ->

        let ty = Fun (Bool, R.High, Box (Fun (Unit, R.Low, Unit), R.High)) in
        let expr =
          Fun_intro ("b", Box_intro (Fun_intro ("x",
            Bool_if (Var "b", Var "x", Var "x"))))
        in

        Validate.check expr ty |> Result.error_to_failure;

      end;

    end;

    (* TODO: More tests! *)

  end;

end
