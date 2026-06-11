(** An evaluator for imperative control flow (loop, break, continue) implemented
    using continuation passing style. *)

type expr =
  | Var of string                   (* Variable occurrences *)
  | Let of string * expr * expr     (* Let bindings *)
  | Seq of expr * expr              (* Sequence two expressions, one after the other *)

  (* Unit *)
  | Unit

  (* Booleans *)
  | Bool_true
  | Bool_false
  | Bool_elim of expr * expr * expr

  (* Natural numbers *)
  | Nat_zero
  | Nat_succ of expr
  | Nat_elim of expr * expr * (string * expr)

  (* Mutable references *)
  | Ref_new of expr
  | Ref_get of expr
  | Ref_set of expr * expr

  (* Control flow operators *)
  | Loop of expr                    (* Unbounded loop *)
  | Break of expr                   (* Break from a loop with a value *)
  | Continue                        (* Continue the next iteration of the surrounding loop *)

type value =
  | Unit
  | Nat_zero
  | Nat_succ of value
  | Bool_true
  | Bool_false
  | Ref of value ref

(** Additional control flow continuations *)
type 'a ops = {
  continue : unit -> 'a;
  break : value -> 'a;
}

(** A {{: https://ocaml.org/manual/5.4/bindingops.html} binding operator} that
    applies a continuation to an intermediate computation. This allows us to
    replace continuation applications:

    {[
      eval env e1 (fun v1 ->
        eval env e2 (fun v2 ->
          ...))
    ]}

    With the following notation:

    {[
      let@ v1 = eval env e1 in
      let@ v2 = eval env e2 in
      ...
    ]}

    This is equivalent to function application, but we use a more precise type
    for clarity.
*)
let ( let@ ) : type a. ((value -> a) -> a) -> (value -> a) -> a =
  ( @@ )

(** Evaluate an expression. *)
let rec eval : type a. (string * value) list -> expr -> a ops -> (value -> a) -> a =
  fun env expr ops return ->
    match expr with
    | Var x ->
        return (List.assoc x env)

    | Let (x, def, body) ->
        let@ def = eval env def ops in
        eval ((x, def) :: env) body ops return

    | Seq (fst, snd) ->
        let@ _ = eval env fst ops in
        eval env snd ops return

    (* Unit *)

    | Unit -> return Unit

    (* Booleans *)

    | Bool_true -> return Bool_true
    | Bool_false -> return Bool_false

    | Bool_elim (pred, true_body, false_body) ->
        let@ pred = eval env pred ops in
        begin match pred with
        | Bool_true -> eval env true_body ops return
        | Bool_false -> eval env false_body ops return
        | _ -> failwith "expected bool"
        end

    (* Natural numbers *)

    | Nat_zero -> return Nat_zero
    | Nat_succ n ->
        let@ n = eval env n ops in
        return (Nat_succ n)

    | Nat_elim (nat, zero_body, (x, succ_body)) ->
        let@ nat = eval env nat ops in
        begin match nat with
        | Nat_zero -> eval env zero_body ops return
        | Nat_succ n -> eval ((x, n) :: env) succ_body ops return
        | _ -> failwith "expected nat"
        end

    (* Mutable references *)

    | Ref_new init ->
        let@ init = eval env init ops in
        return (Ref (ref init))

    | Ref_get curr ->
        let@ curr = eval env curr ops in
        begin match curr with
        | Ref curr -> return !curr
        | _ -> failwith "expected ref"
        end

    | Ref_set (curr, replacement) ->
        let@ curr = eval env curr ops in
        begin match curr with
        | Ref curr ->
            let@ replacement = eval env replacement ops in
            curr := replacement;
            return Unit
        | _ -> failwith "expected ref"
        end

    (* Control flow operators *)

    | Loop body ->
        let rec loop () =
          (* First evaluate the body of the loop *)
          let@ result = eval env body {
            continue = loop; (* call the loop function recursively when continuing to loop *)
            break = return; (* call the value continuation when breaking out of the loop *)
          } in

          (* Now perform another iteration *)
          match result with
          | Unit -> (loop [@tailcall]) ()
          | _ -> failwith "expected unit"
        in
        loop ()

    | Break ret ->
        (* First evaluate the return expression *)
        let@ ret = eval env ret ops in

        (* Now break from the loop with the value *)
        ops.break ret

    | Continue ->
        (* Continuing is easy, just call the continue continuation (heh) *)
        ops.continue ()


module Tests = struct

  let eval expr =
    let ops = {
      continue = (fun () -> failwith "continue outside loop");
      break = (fun _ -> failwith "break outside loop");
    } in
    eval [] expr ops Fun.id

  (* let x := ref false; !x == false *)
  let () =
    let expr : expr =
      Let ("x", Ref_new Bool_false,
        Ref_get (Var "x"))
    in
    assert (eval expr = Bool_false)

  (* let x := ref false; x := true; !x == true *)
  let () =
    let expr : expr =
      Let ("x", Ref_new Bool_false,
        Seq (Ref_set (Var "x", Bool_true),
        Ref_get (Var "x")))
    in
    assert (eval expr = Bool_true)

  (* continue == <undefined> *)
  let () =
    assert begin match eval Continue with
      | _ -> false
      | exception (Failure _) -> true
    end

  (* break () == <undefined> *)
  let () =
    assert begin match eval (Break Unit) with
      | _ -> false
      | exception (Failure _) -> true
    end

  (* loop (break 2) == 2 *)
  let () =
    let nat_expr : expr = Nat_succ (Nat_succ Nat_zero) in
    let loop_expr : expr = Loop (Break nat_expr) in
    assert (eval loop_expr = eval nat_expr)

  (* loop (break 2; continue) == 2 *)
  let () =
    let nat_expr : expr = Nat_succ (Nat_succ Nat_zero) in
    let loop_expr : expr = Loop (Seq (Break nat_expr, Continue)) in
    assert (eval loop_expr = eval nat_expr)

  (* let x := ref 2; loop (break !x) == 2 *)
  let () =
    let nat_expr : expr = Nat_succ (Nat_succ Nat_zero) in
    let loop_expr : expr = Let ("x", Ref_new nat_expr, Loop (Break (Ref_get (Var "x")))) in
    assert (eval loop_expr = eval nat_expr)

  (*
    let x := ref true;
    loop (if !x then (x := false) else (break ()))
  *)
  let () =
    let expr : expr =
      Let ("x", Ref_new Bool_true,
        Loop (Bool_elim (Ref_get (Var "x"),
          Ref_set (Var "x", Bool_false),
          Break Unit)))
    in
    assert (eval expr = Unit)

  (*
    let x := ref true;
    loop (if !x then (x := false; continue) else (break ()))
  *)
  let () =
    let expr : expr =
      Let ("x", Ref_new Bool_true,
        Loop (Bool_elim (Ref_get (Var "x"),
          Seq (Ref_set (Var "x", Bool_false), Continue),
          Break Unit)))
    in
    assert (eval expr = Unit)

  (*
    let x := ref 2;
    loop (
      match !x with
      | Z -> break !x
      | S y -> (x := y; continue)
    )
  *)
  let () =
    let expr : expr =
      Let ("x", Ref_new (Nat_succ (Nat_succ Nat_zero)),
        Loop (Nat_elim (Ref_get (Var "x"),
            Break (Ref_get (Var "x")),
            ("y",
              Seq (Ref_set (Var "x", Var "y"),
                Continue)))))
    in
    assert (eval expr = Nat_zero)

end
