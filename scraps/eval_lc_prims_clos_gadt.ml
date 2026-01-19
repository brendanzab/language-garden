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
    | Var of ident
    | Prim : 'a Prim.t -> t
    | Let of ident * t * t
    | Fun_lit of ident * t
    | Fun_app of t * t
    | Int_lit of int

end

module Value = struct

  type t =
    | Fun_lit of clos
    | Int_lit of int

  and clos =
    | Clos of (ident * t) list * ident * Term.t
    (* NOTE: Unsure if we can generalise these operators *)
    | Prim0 : 'a Prim.t -> clos
    | Prim1 : ('a -> 'b) Prim.t * 'a -> clos

end

let rec eval env =
  function
  | Term.Var x -> List.assoc x env
  | Term.Let (x, t1, t2) -> let v = eval env t1 in eval ((x, v) :: env) t2
  | Term.Fun_lit (x, t) -> Value.Fun_lit (Clos (env, x, t))
  | Term.Fun_app (f, t) -> apply (eval env f) (eval env t)
  | Term.Int_lit n -> Value.Int_lit n
  | Term.Prim Int_neg -> Value.Fun_lit (Prim0 Int_neg)
  | Term.Prim Int_add -> Value.Fun_lit (Prim0 Int_add)
  | Term.Prim Int_sub -> Value.Fun_lit (Prim0 Int_sub)

and apply f v =
  match f, v with
  | Value.Fun_lit (Clos (env, x, t)), v -> eval ((x, v) :: env) t
  | Value.Fun_lit (Prim0 Int_neg), Int_lit n -> Value.Int_lit (Int.neg n)
  | Value.Fun_lit (Prim0 Int_add), Int_lit n -> Value.Fun_lit (Prim1 (Int_add, n))
  | Value.Fun_lit (Prim0 Int_sub), Int_lit n -> Value.Fun_lit (Prim1 (Int_sub, n))
  | Value.Fun_lit (Prim1 (Int_add, n)), Int_lit m -> Value.Int_lit (Int.add n m)
  | Value.Fun_lit (Prim1 (Int_sub, n)), Int_lit m -> Value.Int_lit (Int.sub n m)
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
      assert (eval [] Term.(Prim Int_neg $ Int_lit 1) = Value.Int_lit (-1));
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
