(** Graded lambda calculus.

    This can be used to track variable usage, information flow, etc.

    Extends [check_stlc_inference_rules_bidir.ml].

    - Vilem Liepelt, Danielle Marshall, and Dominic Orchard. 2026.
      Same Coeffect, Different Base: Connecting Two Dominant Approaches to Graded Types.
      https://doi.org/10.1145/3828697
*)

[@@@warning "-unused-constructor"]
[@@@warning "-unused-value-declaration"]

(** Pre-ordered semiring, used to grade function arrows *)
module Grade = struct

  module type S = sig

    type t

    val add : t -> t -> t
    val mul : t -> t -> t

    val zero : t
    val one : t

    val lte : t -> t -> bool

    val pp : t -> Format.formatter -> unit

  end

  let max (type a) (module R : S with type t = a) (x : a) (y : a) : a =
    if R.lte x y then y else x

  let equal (type a) (module R : S with type t = a) (x : a) (y : a) : bool =
    R.lte x y && R.lte y x

  module Unrestricted = struct

    type t = unit

    let add () () = ()
    let mul () () = ()
    let zero = ()
    let one = ()
    let lte () () = true

    let pp () = Format.dprintf "ω"

  end

  module Exact = struct

    type t = int

    let add = Int.add
    let mul = Int.mul
    let zero = Int.zero
    let one = Int.one
    let lte x y = Int.equal x y

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

    let pp x =
      match x with
      | Zero -> Format.dprintf "0"
      | One -> Format.dprintf "1"
      | Many -> Format.dprintf "ω"

  end

  module Security = struct

    type t = Low | High

    let join (* \/ *) x y =
      failwith "TODO"

    let meet (* /\ *) x y =
      failwith "TODO"

    let add x y = meet x y
    let mul x y = join x y

    let zero = High
    let one = Low

    let lte (* ⪰ *) x y =
      failwith "TODO"

    let pp x =
      match x with
      | Low -> Format.dprintf "Lo"
      | High -> Format.dprintf "Hi"

  end

  module Interval (R : S) = struct

    type t = R.t * R.t

    let add = failwith "TODO"
    let mul = failwith "TODO"
    let zero = failwith "TODO"
    let one = failwith "TODO"
    let lte = failwith "TODO"

    let pp (r, s) =
      Format.dprintf "(%t..%t)" (R.pp r) (R.pp s)

  end

  module Product (R : S) (S : S) = struct

    type t = R.t * S.t

    let add (rx, sx) (ry, sy) = R.add rx ry, S.add sx sy
    let mul (rx, sx) (ry, sy) = R.mul rx ry, S.mul sx sy
    let lte (rx, sx) (ry, sy) = R.lte rx ry, S.lte sx sy

    let zero = R.zero, S.zero
    let one = R.one, S.one

    let pp (r, s) =
      Format.dprintf "%t, %t" (R.pp r) (S.pp s)

  end

end

module Make (R : Grade.S) = struct

  type ty (* t *) =
    | Fun of ty * R.t * ty                (* t % r -> t *)
    | Box of ty * R.t                     (* t [r] *)
    (* TODO: | Pair of ty * ty  *)        (* t × t *)
    (* TODO: | Either of ty * ty  *)      (* t + t *)
    | Bool                                (* Bool *)
    | Unit                                (* Unit *)

  type expr (* e *) =
    | Var of string                       (* x *)
    | Ann of expr * ty                    (* e : t *)
    | Fun_intro of string * expr          (* λx. e *)
    | Fun_app of expr * expr              (* e e *)
    | Unit_intro                          (* () *)
    | Unit_elim of string * expr * expr   (* let x = e in e *)
    | Bool_true                           (* true *)
    | Bool_false                          (* false *)
    | Bool_if of expr * expr * expr       (* if e then e else e *)

  (** Typing context *)
  type ctx = (string * ty) list

  (** Grade context *)
  type rctx = R.t list

  let add_rctx (rctx1 : rctx) (rctx2 : rctx) : rctx =
    List.map2 R.add rctx1 rctx2

  let scale_rctx (r : R.t) (rctx : rctx) : rctx =
    List.map (R.mul r) rctx

  let max_rctx (rctx1 : rctx) (rctx2 : rctx) : rctx =
    List.map2 (Grade.max (module R)) rctx1 rctx2

  let equal_rctx (rctx1 : rctx) (rctx2 : rctx) : bool =
    List.for_all2 (Grade.equal (module R)) rctx1 rctx2


  (** Type errors *)

  exception Type_error of string

  let type_error (type a b) : (a, Format.formatter, unit, b) format4 -> a =
    fun fmt -> Format.kasprintf (fun msg -> raise (Type_error msg)) fmt


  (** Bidirectional typing *)

  (** Lookup a variable in the context, returning its type along with a weakened
      usage context *)
  let rec lookup (ctx : ctx) (x : string) : ty * rctx =
    (* Return a grade context with 1 grade for the used variable, and 0 grade
       for all the other variables *)
    match ctx with
    | (y, t) :: ctx when x = y -> t, R.one :: List.map (Fun.const R.zero) ctx
    | _ :: ctx -> Pair.map_snd (List.cons R.zero) (lookup ctx x)
    | [] -> type_error "unbound variable %s" x

  let rec check (ctx : ctx) (e : expr) (t : ty) : rctx =
    match e, t with
    | Fun_intro (x, e), Fun (t1, r, t2) ->
        let rctx = check ((x, t1) :: ctx) e t2 in
        if R.lte (List.hd rctx) r then List.tl rctx else
          type_error "grade mismatch: expected %t, found %t" (R.pp r) (R.pp (List.hd rctx))

    (* BUG: breaks linear usages *)
    | Bool_if (e1, e2, e3), t ->
        let rctx1 = check ctx e1 Bool in
        let rctx2 = check ctx e2 t in
        let rctx3 = check ctx e3 t in
        add_rctx rctx1 (max_rctx rctx2 rctx3)

    (* BUG: breaks unrestricted usages *)
    (*
    | Bool_if (e1, e2, e3), t ->
        let rctx1 = check ctx e1 Bool in
        let rctx2 = check ctx e2 t in
        let rctx3 = check ctx e3 t in
        if equal_rctx rctx2 rctx3 then
          add_rctx rctx1 rctx2
        else
          type_error "mismatched grades" *)

    | Unit_elim _, t ->
        failwith "TODO"

    | e, t ->
        let t', rctx = infer ctx e in
        if t' = t then rctx else
          type_error "type mismatch"

  and infer (ctx : ctx) (e : expr) : ty * rctx =
    match e with
    | Var x ->
        lookup ctx x

    | Ann (e, t) ->
        t, check ctx e t

    | Fun_app (e1, e2) ->
        begin match infer ctx e1, infer ctx e2 with
        | (Fun (t1, r, t2), rctx1), (t3, rctx2) when t1 = t3 ->
            t2, add_rctx rctx1 (scale_rctx r rctx2)
        | _ -> type_error "type mismatch"
        end

    | Bool_true | Bool_false ->
        Bool, List.map (Fun.const R.zero) ctx

    | Unit_intro ->
        Unit, List.map (Fun.const R.zero) ctx

    | Unit_elim _ ->
        failwith "TODO"

    | Fun_intro _ | Bool_if _ ->
        type_error "ambiguous"

  let check (e : expr) (t : ty) : (unit, string) result =
    match check [] e t with
    | uctx -> assert (List.is_empty uctx); Ok ()
    | exception Type_error msg -> Error msg

  let infer (e : expr) : (ty, string) result =
    match infer [] e with
    | t, uctx -> assert (List.is_empty uctx); Ok t
    | exception Type_error msg -> Error msg

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

      let open Make (Grade.Unrestricted) in
      let module R = Grade.Unrestricted in

      (* let ( $ ) f x = Fun_app (f, x) in *)

      let id_ty = Fun (Unit, (), Unit) in
      let id_expr = Fun_intro ("x", Var "x") in
      let id_expr_ignore = Fun_intro ("x", Unit_intro) in

      begin test "unrestricted: id 0" @@ fun () ->
        assert (check id_expr id_ty = Ok ());
      end;

      begin test "unrestricted: id 0 ignore" @@ fun () ->
        assert (check id_expr_ignore id_ty = Ok ());
      end;

    end;

    (* Linearity grade *)

    begin

      let open Make (Grade.Linearity) in
      let module R = Grade.Linearity in

      (* let ( $ ) f x = Fun_app (f, x) in *)

      let id_ty r = Fun (Unit, r, Unit) in
      let id_expr = Fun_intro ("x", Var "x") in
      let id_expr_ignore = Fun_intro ("x", Unit_intro) in

      begin test "linear: id 0" @@ fun () ->
        assert (check id_expr (id_ty R.Zero) |> Result.is_error);
      end;

      begin test "linear: id 1" @@ fun () ->
        assert (check id_expr (id_ty R.One) = Ok ());
      end;

      begin test "linear: id ω" @@ fun () ->
        assert (check id_expr (id_ty R.Many) = Ok ());
      end;

      begin test "linear: id 0 ignore" @@ fun () ->
        assert (check id_expr_ignore (id_ty R.Zero) = Ok ());
      end;

      begin test "linear: id 1 ignore" @@ fun () ->
        assert (check id_expr_ignore (id_ty R.One) |> Result.is_error);
      end;

      begin test "linear: id ω ignore" @@ fun () ->
        assert (check id_expr_ignore (id_ty R.Many) = Ok ());
      end;

      begin test "linear: if branches" @@ fun () ->

        let ty = Fun (Bool, R.One, Fun (Unit, R.One, Unit)) in
        let expr =
          Fun_intro ("b", Fun_intro ("x",
            Bool_if (Var "b", Var "x", Var "x")))
        in

        assert (check expr ty = Ok ());

      end;

      begin test "linear: if branches ignore left 1" @@ fun () ->

        let ty = Fun (Bool, R.One, Fun (Unit, R.One, Unit)) in
        let expr =
          Fun_intro ("b", Fun_intro ("x",
            Bool_if (Var "b", Unit_intro, Var "x")))
        in

        assert (check expr ty |> Result.is_error);

      end;

      (* FIXME: Exception *)
      begin test "linear: if branches ignore right 1" @@ fun () ->

        let ty = Fun (Bool, R.One, Fun (Unit, R.One, Unit)) in
        let expr =
          Fun_intro ("b", Fun_intro ("x",
            Bool_if (Var "b", Var "x", Unit_intro)))
        in

        assert (check expr ty |> Result.is_error);

      end;

      begin test "linear: if branches ignore left many" @@ fun () ->

        let ty = Fun (Bool, R.One, Fun (Unit, R.Many, Unit)) in
        let expr =
          Fun_intro ("b", Fun_intro ("x",
            Bool_if (Var "b", Unit_intro, Var "x")))
        in

        assert (check expr ty = Ok ());

      end;

      begin test "linear: if branches ignore right many" @@ fun () ->

        let ty = Fun (Bool, R.One, Fun (Unit, R.Many, Unit)) in
        let expr =
          Fun_intro ("b", Fun_intro ("x",
            Bool_if (Var "b", Var "x", Unit_intro)))
        in

        assert (check expr ty = Ok ());

      end;

    end;

  end;

end
