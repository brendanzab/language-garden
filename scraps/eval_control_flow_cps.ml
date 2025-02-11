(** An evaluator for a language with imperative control flow operators,
    implemented with continuation passing style. *)

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

  (* Loops *)
  | Loop of expr                    (* Unbounded loop *)
  | Break of expr                   (* Break with a value *)
  | Continue                        (* Continue the next iteration of the surrounding loop *)

type value =
  | Unit
  | Nat_zero
  | Nat_succ of value
  | Bool_true
  | Bool_false
  | Ref of value ref

type 'a k = 'a -> value

let ( let@ ) = ( @@ )

let rec eval_expr (env : (string * value) list) (expr : expr) (continue_k : unit k) (break_k : value k) (return_k : value k) : value =
  match expr with
  | Var x -> return_k (List.assoc x env)
  | Let (x, def, body) ->
      let@ def = eval_expr env def continue_k break_k in
      eval_expr ((x, def) :: env) body continue_k break_k return_k
  | Seq (fst, snd) ->
      let@ fst = eval_expr env fst continue_k break_k in
      begin match fst with
      | Unit -> eval_expr env snd continue_k break_k return_k
      | _ -> failwith "expected unit"
      end

  | Unit -> return_k Unit

  | Bool_true -> return_k Bool_true
  | Bool_false -> return_k Bool_false
  | Bool_elim (pred, true_body, false_body) ->
      let@ pred = eval_expr env pred continue_k break_k in
      begin match pred with
      | Bool_true -> eval_expr env true_body continue_k break_k return_k
      | Bool_false -> eval_expr env false_body continue_k break_k return_k
      | _ -> failwith "expected bool"
      end

  | Nat_zero -> return_k Nat_zero
  | Nat_succ n -> return_k (Nat_succ (eval_expr env n continue_k break_k return_k))
  | Nat_elim (nat, zero_body, (x, succ_body)) ->
      let@ nat = eval_expr env nat continue_k break_k in
      begin match nat with
      | Nat_zero -> eval_expr env zero_body continue_k break_k return_k
      | Nat_succ n -> eval_expr ((x, n) :: env) succ_body continue_k break_k return_k
      | _ -> failwith "expected nat"
      end

  | Ref_new init ->
      let@ init = eval_expr env init continue_k break_k in
      return_k (Ref (ref init))
  | Ref_get curr ->
      let@ curr = eval_expr env curr continue_k break_k in
      begin match curr with
      | Ref curr -> return_k !curr
      | _ -> failwith "expected ref"
      end
  | Ref_set (curr, replacement) ->
      let@ curr = eval_expr env curr continue_k break_k in
      begin match curr with
      | Ref curr ->
          let@ replacement = eval_expr env replacement continue_k break_k in
          curr := replacement;
          return_k Unit
      | _ -> failwith "expected ref"
      end

  | Loop body ->
      let rec loop () =
        let@ body = eval_expr env body loop return_k in
        begin match body with
        | Unit -> (loop [@tailcall]) ()
        | _ -> failwith "expected unit"
        end
      in
      loop ()

  | Break expr ->
      eval_expr env expr continue_k break_k break_k
  | Continue ->
      continue_k ()


module Tests = struct

  let eval_expr expr =
    eval_expr [] expr
      (fun () -> failwith "continue outside loop")
      (fun _ -> failwith "break outside loop")
      Fun.id

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
