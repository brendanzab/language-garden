(** A version of [eval_lc_prims_clos] that uses GADTs to track the arity of
    primitives. *)

type ident = string

module Prim = struct

  type _ t =
    | Int_neg : (int -> int) t
    | Int_add : (int -> int -> int) t
    | Int_sub : (int -> int -> int) t

end

module Term = struct

  type t =
    | Var of ident [@warning "-unused-constructor"]
    | Fun_lit of ident * t [@warning "-unused-constructor"]
    | Fun_app of t * t
    | Int of int
    | Prim : 'a Prim.t -> t

end

module Value = struct

  type t =
    | Int of int
    | Var of ident [@warning "-unused-constructor"]
    | Fun_lit of clos

  and clos =
    | Clos of (ident * t) list * ident * Term.t
    (* NOTE: Unsure if we can generalise these operators *)
    | Prim0 : 'a Prim.t -> clos
    | Prim1 : ('a -> 'b) Prim.t * 'a -> clos

end

let rec eval env =
  function
  | Term.Var x -> List.assoc x env
  | Term.Fun_lit (x, t) -> Value.Fun_lit (Clos (env, x, t))
  | Term.Fun_app (f, t) -> apply (eval env f) (eval env t)
  | Term.Int n -> Value.Int n
  | Term.Prim Int_neg -> Value.Fun_lit (Prim0 Int_neg)
  | Term.Prim Int_add -> Value.Fun_lit (Prim0 Int_add)
  | Term.Prim Int_sub -> Value.Fun_lit (Prim0 Int_sub)

and apply f v =
  match f, v with
  | Value.Fun_lit (Clos (env, x, t)), v -> eval ((x, v) :: env) t
  | Value.Fun_lit (Prim0 Int_neg), Int n -> Value.Int (Int.neg n)
  | Value.Fun_lit (Prim0 Int_add), Int n -> Value.Fun_lit (Prim1 (Int_add, n))
  | Value.Fun_lit (Prim0 Int_sub), Int n -> Value.Fun_lit (Prim1 (Int_sub, n))
  | Value.Fun_lit (Prim1 (Int_add, n)), Int m -> Value.Int (Int.add n m)
  | Value.Fun_lit (Prim1 (Int_sub, n)), Int m -> Value.Int (Int.sub n m)
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
