(** Compiling untyped lambda calculus expressions into various abstract machines.

    {2 Resources}

    - Xavier Leroy. 2005. From Krivine’s machine to the Caml implementations
      https://xavierleroy.org/talks/zam-kazam05.pdf
    - Xavier Leroy. 2015. Functional programming languages Part II: abstract machines
      https://xavierleroy.org/mpri/2-4/machines.pdf
*)

(** Signature of abstract machines *)
module type Machine = sig

  type prog
  type state

  val inject : prog -> state
  val step : state -> state option

end

(** Step an abstract machine until it halts *)
let rec step_many : type state. (module Machine with type state = state) -> state -> state =
  fun (module M) state ->
  match M.step state with
  | Some state -> step_many (module M) state
  | None -> state


(** Constants *)
module Const = struct

  type t =
    | Int of int
    | String of string

end


(** {2 Abstract Machines} *)

(** Untyped lambda calculus expressions *)
module Expr = struct

  (** De-bruijn index *)
  type index = int

  type t =
    | Var of index
    | Let of string * t * t
    | Fun_intro of string * t
    | Fun_app of t * t
    | Const of Const.t

end

(** Landin’s SECD Machine (Call-by-value)

    This was a very early abstract machine (possibly one of the first?) and was
    very influential, introducing {e closures} as a way of implementing
    first-class functions.

    The name is derived from the somewhat names used for the components of the
    machine state in the original paper:

    {@text[
      ⟨ S, E, C, D ⟩
        ▲  ▲  ▲  ▲
        │  │  │  │
        │  │  │  (D)ump
        │  │  │
        │  │  (C)ontrol instruction
        │  │
        │  (E)nvironment
        │
        (S)tack
    ]}

    Let expressions are implemented in the same way as on Xavier Leroy’s slides
    (linked above), by pushing and popping definitions off the environment.

    {2 Resources}

    - https://en.wikipedia.org/wiki/SECD_machine
    - Peter Landin. 1964. The Mechanical Evaluation of Expressions.
    - David Turner. 2012. Some History of Functional Programming Languages.
      https://www.cs.kent.ac.uk/people/staff/dat/tfp12/tfp12.pdf
*)
module Secd = struct

  (** De-bruijn index *)
  type index = int

  (** Machine instructions *)
  type instr =
    | Var of index        (* Load a variable from the environment and push it to the stack *)
    | Let_def             (* Pop a value and add it to the environment *)
    | Let_end             (* Discard the first entry in the environment *)
    | Fun_intro of prog   (* Push a closure to the stack *)
    | Fun_app             (* Begin executing a closure with an argument, saving the calling frame *)
    | Fun_end             (* Terminate the function and resume the rest of the program *)
    | Const of Const.t    (* Push a constant to the stack *)

  and value =
    | Clos of prog * env
    | Const of Const.t

  (** Call-frames, for resuming a computation later *)
  and frame = prog * env * stack

  and stack = value list        (* Return (S)tack *)
  and env = value list          (* (E)nvironment *)
  and prog = instr list         (* (C)ontrol stack *)
  and frames = frame list       (* (D)ump stack *)

  (** State of the abstract machine *)
  type state = prog * env * stack * frames

  (** Compile lambda calculus expressions into a list of SECD instructions *)
  let compile_expr (expr : Expr.t) : prog =
    (* Avoid expensive append operations by composing functions that add
       instructions to the head of the program. *)
    let ( << ) = Fun.compose in
    let rec compile (expr : Expr.t) : prog -> prog =
      match expr with
      | Expr.Var i -> List.cons (Var i : instr)
      | Expr.Let (_, def, body) -> compile def << List.cons Let_def << compile body << List.cons Let_end
      | Expr.Fun_intro (_, body) -> List.cons (Fun_intro (compile body [Fun_end]))
      | Expr.Fun_app (fn, arg) -> compile fn << compile arg << List.cons Fun_app
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

(** Cardelli’s Functional Abstract Machine (Call-by-value) *)
module Fam = struct (* TODO *) end

(** Categorical abstract machine (Call-by-value) *)
module Cam = struct (* TODO *) end

(** Kirvine’s Machine (Call-by-name) *)
module Kirvine = struct (* TODO *) end

(** Zinc abstract machine (Call-by-value)*)
module Zam = struct (* TODO *) end


(* Tests *)

let () = begin

  Printexc.record_backtrace true;

  let module Const = struct

    type t =
      | Int of int

  end in

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

    test name @@ fun () ->
      match step_many (module Secd) Secd.(inject (compile_expr expr)), expected with
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
