(** A version of [eval_lc_prims_hoas] with defunctionalised closures. *)

type ident = string

module Prim = struct

  type t =
    | Int_neg
    | Int_add
    | Int_sub

end

module Term = struct

  type t =
    | Var of ident [@warning "-unused-constructor"]
    | Fun_lit of ident * t [@warning "-unused-constructor"]
    | Fun_app of t * t
    | Int of int
    | Prim of Prim.t

end

module Value = struct

  type t =
    | Int of int
    | Var of ident [@warning "-unused-constructor"]
    | Fun_lit of clos

  and clos =
    | Clos of (ident * t) list * ident * Term.t
    | Neg
    | Add
    | Sub
    | Add2 of int
    | Sub2 of int

end

let rec eval env =
  function
  | Term.Var x -> List.assoc x env
  | Term.Fun_lit (x, t) -> Value.Fun_lit (Clos (env, x, t))
  | Term.Fun_app (f, t) -> apply (eval env f) (eval env t)
  | Term.Int n -> Value.Int n
  | Term.Prim Int_neg -> Value.Fun_lit Neg
  | Term.Prim Int_add -> Value.Fun_lit Add
  | Term.Prim Int_sub -> Value.Fun_lit Sub

and apply f v =
  match f, v with
  | Value.Fun_lit (Clos (env, x, t)), v -> eval ((x, v) :: env) t
  | Value.Fun_lit Neg, Int n -> Value.Int (Int.neg n)
  | Value.Fun_lit Add, Int n -> Value.Fun_lit (Add2 n)
  | Value.Fun_lit Sub, Int n -> Value.Fun_lit (Sub2 n)
  | Value.Fun_lit (Add2 n), Int m -> Value.Int (Int.add n m)
  | Value.Fun_lit (Sub2 n), Int m -> Value.Int (Int.sub n m)
  | _ -> failwith "invalid application"

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
      assert (eval [] Term.(Prim Int_neg $ Int 1) = Value.Int (-1));
    end;

    begin test "#int-add 3 4" @@ fun () ->
      assert (eval [] Term.(Prim Int_add $ Int 3 $ Int 4) = Value.Int 7);
    end;

    begin test "#int-sub 3 4" @@ fun () ->
      assert (eval [] Term.(Prim Int_sub $ Int 3 $ Int 4) = Value.Int (-1));
    end;

  end;

end
