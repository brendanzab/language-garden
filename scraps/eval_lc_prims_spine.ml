(** A lambda calculus evaluator that supports the partial application of
    primitive operations.

    Partial applications are collected in an application spine in
    {!Value.Prim_app}, which will reduced once the primitive operation is
    fully-applied during evaluation.
*)

type ident = string

module Prim = struct

  type t =
    | Int_neg
    | Int_add
    | Int_sub

end

module Term = struct

  type t =
    | Var of ident
    | Prim of Prim.t
    | Let of ident * t * t
    | Fun_lit of ident * t
    | Fun_app of t * t
    | Int_lit of int

end

module Value = struct

  type t =
    | Fun_lit of (ident * t) list * ident * Term.t
    | Prim_app of Prim.t * t list   (** Under-applied primitive operations. *)
    | Int_lit of int

end

exception Eval_error of string

let rec eval (env : (ident * Value.t) list) : Term.t -> Value.t =
  function
  | Term.Var x -> List.assoc x env
  | Term.Prim p -> apply_prim p []
  | Term.Let (x, t1, t2) -> let v = eval env t1 in eval ((x, v) :: env) t2
  | Term.Fun_lit (x, t) -> Value.Fun_lit (env, x, t)
  | Term.Fun_app (f, t) -> apply (eval env f) (eval env t)
  | Term.Int_lit n -> Value.Int_lit n

and apply (f : Value.t) (v : Value.t) : Value.t =
  match f with
  | Value.Fun_lit (env, x, t) -> eval ((x, v) :: env) t
  | Value.Prim_app (p, args) -> apply_prim p (v :: args)
  | _ -> raise (Eval_error "invalid application")

and apply_prim (p : Prim.t) (args : Value.t list) : Value.t =
  match (p, args) with
  | Prim.Int_neg, Value.[Int_lit x] -> Value.Int_lit (Int.neg x)
  | Prim.Int_add, Value.[Int_lit y; Int_lit x] -> Value.Int_lit (Int.add x y)
  | Prim.Int_sub, Value.[Int_lit y; Int_lit x] -> Value.Int_lit (Int.sub x y)
  | p, args -> Value.Prim_app (p, args)


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

  let ( $ ) f x = Term.Fun_app (f, x) in

  begin run_tests @@ fun test ->

    begin test "#int-neg 1" @@ fun () ->
      assert (eval [] Term.(Prim Int_neg $ Int_lit 1) = Value.Int_lit (-1));
    end;

    begin test "#int-add" @@ fun () ->
      assert (eval [] Term.(Prim Int_add) = Value.Prim_app (Int_add, []));
    end;

    begin test "#int-add 3" @@ fun () ->
      assert (eval [] Term.(Prim Int_add $ Int_lit 3) = Value.Prim_app (Int_add, [Int_lit 3]));
    end;

    begin test "#int-add 3 4" @@ fun () ->
      assert (eval [] Term.(Prim Int_add $ Int_lit 3 $ Int_lit 4) = Value.Int_lit 7);
    end;

    begin test "#int-sub 3 4" @@ fun () ->
      assert (eval [] Term.(Prim Int_sub $ Int_lit 3 $ Int_lit 4) = Value.Int_lit (-1));
    end;

    begin test "let add-3 := #int-add 3; add-3 4" @@ fun () ->
      let tm = Term.(
        Let ("add-3", Prim Int_add $ Int_lit 3,
        Var "add-3" $ Int_lit 4)
      ) in
      assert (eval [] tm = Value.Int_lit 7);
    end;

    begin test "(fun x => x) (#int-sub 3) 4" @@ fun () ->
      let tm = Term.(Fun_lit ("x", Var "x") $ (Prim Int_sub $ Int_lit 3) $ Int_lit 4) in
      assert (eval [] tm = Value.Int_lit (-1));
    end;

  end;

end
