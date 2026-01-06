(** Compiling untyped lambda calculus expressions into various abstract machines.

    {2 Resources}

    - Xavier Leroy, {{: https://xavierleroy.org/talks/zam-kazam05.pdf}
      From Krivine’s machine to the Caml implementations}
    - Xavier Leroy, {{: https://xavierleroy.org/mpri/2-4/machines.pdf}
      Functional programming languages Part II: abstract machines}
*)

(** Signature of abstract machines *)
module type Machine = sig

  type prog
  type state

  val inject : prog -> state
  val step : state -> state option

end

(** Constants *)
module type Const = sig

  type t

end

(** Untyped lambda calculus expressions *)
module Expr = struct

  module Make (C : Const) = struct

    type t =
      | Var of int
      | Let of string * t * t
      | Fun_intro of string * t
      | Fun_app of t * t
      | Const of C.t

  end

end

(** Landin’s SECD Machine (Call-by-value) *)
module Secd = struct

  module Data (C : Const) = struct

    type instr =
      | Var of int          (* Load a variable from the environment and push it to the stack *)
      | Let_def             (* Pop a value and add it to the environment *)
      | Let_end             (* Discard the first entry in the environment *)
      | Fun_intro of prog   (* Push a closure to the stack *)
      | Fun_app             (* Begin executing a closure with an argument, saving the calling frame *)
      | Fun_end             (* Terminate the function and resume the rest of the program *)
      | Const of C.t        (* Push a constant to the stack *)

    and value =
      | Clos of prog * env
      | Const of C.t

    and frame = prog * env * stack

    and stack = value list        (* Return (S)tack *)
    and env = value list          (* (E)nvironment *)
    and prog = instr list         (* (C)ontrol stack *)
    and frames = frame list       (* (D)ump stack *)

    (** State of the abstract machine *)
    type state = prog * env * stack * frames

  end

  module Make (C : Const) : sig

    include module type of Data (C)

    (** Compile lambda calculus expressions into a list of SECD instructions *)
    val compile_expr : Expr.Make (C).t -> prog

    include Machine
      with type prog := prog
      with type state := state

  end = struct

    include Data (C)

    let compile_expr (expr : Expr.Make (C).t) : prog =
      let module Expr = Expr.Make (C) in
      let ( << ) = Fun.compose in
      let rec compile (expr : Expr.t) : prog -> prog =
        match expr with
        | Expr.Var i -> List.cons (Var i : instr)
        | Expr.Let (_, e1, e2) -> compile e1 << List.cons Let_def << compile e2 << List.cons Let_end
        | Expr.Fun_intro (_, e) -> List.cons (Fun_intro (compile e [Fun_end]))
        | Expr.Fun_app (e1, e2) -> compile e1 << compile e2 << List.cons Fun_app
        | Expr.Const c -> List.cons (Const c : instr)
      in
      compile expr []

    let inject (prog : prog) : state =
      prog, [], [], []

    let step : state -> state option = function
      (* Lookup a variable and push it to the stack *)
      | Var i :: prog, env, stack, frames ->
          List.nth_opt env i |> Option.map @@ fun v ->
            prog, env, v :: stack, frames

      (* Move a definition from the stack to the environment *)
      | Let_def :: prog, env, v :: stack, frames ->
          Some (prog, v :: env, stack, frames)

      (* Exiting a let binding, so pop the definition off the environment *)
      | Let_end :: prog, _ :: env, stack, frames ->
          Some (prog, env, stack, frames)

      (* Push a closure onto the stack *)
      | Fun_intro prog' :: prog, env, stack, frames ->
          Some (prog, env, Clos (prog', env) :: stack, frames)

      (* Suspend the calling frame, and begin executing the body of a closure
         with argument bound in the environment. *)
      | Fun_app :: prog, env, v :: Clos (prog', env') :: stack, frames ->
          Some (prog', v :: env', [], (prog, env, stack) :: frames)

      (* Resume the calling frame, moving the value at the top of the stack
         to the previously suspended stack *)
      | Fun_end :: _, _, v :: _, (prog, env, stack) :: frames ->
          Some (prog, env, v :: stack, frames)

      (* Push a constant to the stack *)
      | Const c :: prog, env, stack, frames ->
          Some (prog, env, Const c :: stack, frames)

      | _ -> None

  end

end

(** Cardelli’s Functional Abstract Machine (Call-by-value) *)
module Fam = struct (* TODO *) end

(** Categorical abstract machine (Call-by-value) *)
module Cam = struct (* TODO *) end

(** Kirvine’s Machine (Call-by-name) *)
module Kirvine = struct (* TODO *) end

(** Zinc abstract machine (Call-by-value)*)
module Zam = struct (* TODO *) end


let () = begin

  Printexc.record_backtrace true;

  let module Const = struct

    type t =
      | Int of int

  end in

  let module Expr = Expr.Make (Const) in
  let module Secd = Secd.Make (Const) in

  let ( $ ) f a = Expr.Fun_app (f, a) in

  let success_count = ref 0 in
  let error_count = ref 0 in

  let test (name : string) (f : unit -> unit) : unit =
    Printf.printf "test %s ... " name;

    match f () with
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

  let test_eval
      ~(expected : Secd.value)
      (name : string)
      (expr : Expr.t) : unit =

    let rec step_many state =
      match Secd.step state with
      | Some state -> step_many state
      | None -> state
    in

    test name @@ fun () ->
      match step_many Secd.(inject (compile_expr expr)), expected with
      | ([], [], [v], []), expected -> assert (v = expected)
      | (_, _, _, _), _ -> failwith "expected return value"
  in

  print_string "Running tests:\n\n";

  test_eval "apply id"
    (* (fun x => x) 42 *)
    Expr.(Fun_intro ("x", Var 0) $ Const (Int 42))
    ~expected:(Const (Int 42));

  test_eval "apply const"
    (* (fun x y => x) 42 3 *)
    Expr.(Fun_intro ("x", Fun_intro ("y", Var 1)) $ Const (Int 42) $ Const (Int 3))
    ~expected:(Const (Int 42));

  test_eval "apply flip const"
    (*
      let const x y := x;
      let flip f x y := f y x;
      flip const 42 3
    *)
    Expr.(
      Let ("const", Fun_intro ("x", Fun_intro ("y", Var 1)),
      Let ("flip", Fun_intro ("f", Fun_intro ("x", Fun_intro ("y", Var 2 $ Var 0 $ Var 1))),
        Var 0 $ Var 1 $ Const (Int 42) $ Const (Int 3)))
    )
    ~expected:(Const (Int 3));

  print_string "\n";

  if !error_count > 0 then begin
    Printf.printf "Failed %i out of %i tests\n" !error_count (!success_count + !error_count);
    exit 1
  end;

  Printf.printf "Ran %i successful tests\n" !success_count;

end
