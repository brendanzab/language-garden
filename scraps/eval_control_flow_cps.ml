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

(** A continuation *)
type 'a k = 'a -> value

(** Additional control flow continuations *)
type ops = {
  continue_k : unit k;
  break_k : value k;
}

(** Apply a continuation in a more “direct style” with a binding operator. *)
let ( let@ ) (type a) : a k k -> a k k = ( @@ )

(** Evaluate an expression. *)
let rec eval_expr (env : (string * value) list) (expr : expr) (ops : ops) (k : value k) : value =
  match expr with
  | Var x ->
      k (List.assoc x env)

  | Let (x, def, body) ->
      let@ def = eval_expr env def ops in
      eval_expr ((x, def) :: env) body ops k

  | Seq (fst, snd) ->
      let@ _ = eval_expr env fst ops in
      eval_expr env snd ops k

  (* Unit *)

  | Unit -> k Unit

  (* Booleans *)

  | Bool_true -> k Bool_true
  | Bool_false -> k Bool_false

  | Bool_elim (pred, true_body, false_body) ->
      let@ pred = eval_expr env pred ops in
      begin match pred with
      | Bool_true -> eval_expr env true_body ops k
      | Bool_false -> eval_expr env false_body ops k
      | _ -> failwith "expected bool"
      end

  (* Natural numbers *)

  | Nat_zero -> k Nat_zero
  | Nat_succ n -> k (Nat_succ (eval_expr env n ops k))

  | Nat_elim (nat, zero_body, (x, succ_body)) ->
      let@ nat = eval_expr env nat ops in
      begin match nat with
      | Nat_zero -> eval_expr env zero_body ops k
      | Nat_succ n -> eval_expr ((x, n) :: env) succ_body ops k
      | _ -> failwith "expected nat"
      end

  (* Mutable references *)

  | Ref_new init ->
      let@ init = eval_expr env init ops in
      k (Ref (ref init))

  | Ref_get curr ->
      let@ curr = eval_expr env curr ops in
      begin match curr with
      | Ref curr -> k !curr
      | _ -> failwith "expected ref"
      end

  | Ref_set (curr, replacement) ->
      let@ curr = eval_expr env curr ops in
      begin match curr with
      | Ref curr ->
          let@ replacement = eval_expr env replacement ops in
          curr := replacement;
          k Unit
      | _ -> failwith "expected ref"
      end

  (* Control flow operators *)

  | Loop body ->
      let rec loop () =
        (* First evaluate the body of the loop *)
        let@ _ = eval_expr env body {
          continue_k = loop; (* call the loop function recursively when continuing to loop *)
          break_k = k; (* call the value continuation when breaking out of the loop *)
        } in

        (* Now perform another iteration *)
        (loop [@tailcall]) ()
      in
      loop ()

  | Break ret ->
      (* First evaluate the return expression *)
      let@ ret = eval_expr env ret ops in

      (* Now break from the loop with the value *)
      ops.break_k ret

  | Continue ->
      (* Continuing is easy, just call the continue continuation (heh) *)
      ops.continue_k ()


module Tests = struct

  let eval_expr expr =
    let ops = {
      continue_k = (fun () -> failwith "continue outside loop");
      break_k = (fun _ -> failwith "break outside loop");
    } in
    eval_expr [] expr ops Fun.id

  (* let x := ref false; !x == false *)
  let () =
    let expr : expr =
      Let ("x", Ref_new Bool_false,
        Ref_get (Var "x"))
    in
    assert (eval_expr expr = Bool_false)

  (* let x := ref false; x := true; !x == true *)
  let () =
    let expr : expr =
      Let ("x", Ref_new Bool_false,
        Seq (Ref_set (Var "x", Bool_true),
        Ref_get (Var "x")))
    in
    assert (eval_expr expr = Bool_true)

  (* continue == <undefined> *)
  let () =
    assert begin match eval_expr Continue with
      | _ -> false
      | exception (Failure _) -> true
    end

  (* break () == <undefined> *)
  let () =
    assert begin match eval_expr (Break Unit) with
      | _ -> false
      | exception (Failure _) -> true
    end

  (* loop (break 2) == 2 *)
  let () =
    let nat_expr : expr = Nat_succ (Nat_succ Nat_zero) in
    let loop_expr : expr = Loop (Break nat_expr) in
    assert (eval_expr loop_expr = eval_expr nat_expr)

  (* loop (break 2; continue) == 2 *)
  let () =
    let nat_expr : expr = Nat_succ (Nat_succ Nat_zero) in
    let loop_expr : expr = Loop (Seq (Break nat_expr, Continue)) in
    assert (eval_expr loop_expr = eval_expr nat_expr)

  (* let x := ref 2; loop (break !x) == 2 *)
  let () =
    let nat_expr : expr = Nat_succ (Nat_succ Nat_zero) in
    let loop_expr : expr = Let ("x", Ref_new nat_expr, Loop (Break (Ref_get (Var "x")))) in
    assert (eval_expr loop_expr = eval_expr nat_expr)

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
    assert (eval_expr expr = Unit)

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
    assert (eval_expr expr = Unit)

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
    assert (eval_expr expr = Nat_zero)

end
