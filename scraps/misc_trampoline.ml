(** Stepped, stack-safe computations with free monads

    - https://www.scala-lang.org/api/current/scala/util/control/TailDefers$.html
    - https://github.com/scala/scala3/blob/3.8.2/library/src/scala/util/control/TailDefers.scala
    - https://blog.higher-order.com/assets/trampolines.pdf
*)

module Trampoline : sig

  type 'a t
  (** A computation that can be stepped through *)

  val defer : (unit -> 'a t) -> 'a t
  (** Defer the next step of the computation *)

  (** {1 Running computations} *)

  val resume : 'a t -> (unit -> 'a t, 'a) Either.t
  (** Advance to the next step of the computation *)

  val result : 'a t -> 'a
  (** Return the result of running all the computations *)

  (** {1 Monadic API} *)

  val pure : 'a -> 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val bind : 'a t -> ('a -> 'b t) -> 'b t

end = struct

  type 'a t =
    | Defer : (unit -> 'a t) -> 'a t
    (* Free monad *)
    | Bind : 'a t * ('a -> 'b t) -> 'b t
    | Pure : 'a -> 'a t

  let defer (type a) (rest : unit -> a t) : a t =
    Defer rest

  let pure (type a) (result : a) : a t =
    Pure result

  let rec bind : type a b. a t -> (a -> b t) -> b t =
    fun mx f ->
      match mx with
      | Pure x -> Defer (fun () -> f x)
      | Defer _ as mx -> Bind (mx, f)
      | Bind (x', f') -> Bind (x', fun x -> bind (f' x) f)

  let map (type a b) (f : a -> b) (mx : a t) : b t =
    bind mx (fun x -> Defer (fun () -> Pure (f x)))

  let rec resume : type a b. a t -> (unit -> a t, a) Either.t =
    function
    | Pure x -> Right x
    | Defer rest -> Left rest
    | Bind (Pure x, f) -> (resume [@tail_call]) (f x)
    | Bind (Defer rest, f) -> Left (fun () -> bind (rest ()) f)
    | Bind (Bind (mx, f'), f) -> (resume [@tail_call]) (bind mx (fun x -> bind (f' x) f))

  let rec result : type a. a t -> a =
    fun mx ->
      (* The call to resume could be inlined for performance, like in the Scala
         standard library. *)
      match resume mx with
      | Right x -> x
      | Left rest -> (result [@tail_call]) (rest ())

end

module _ = struct

  module T = Trampoline

  let ( let* ) = T.bind

  let rec is_even (xs : int list) : bool T.t =
    match xs with
    | [] -> T.pure true
    | _ :: xs -> T.defer (fun () -> is_odd xs)

  and is_odd (xs : int list) : bool T.t =
    match xs with
    | [] -> T.pure false
    | _ :: xs -> T.defer (fun () -> is_even xs)

  let rec fib (n : int) : int T.t =
    if n < 2 then T.pure n else
      let* x = T.defer (fun () -> fib (n - 1)) in
      let* y = T.defer (fun () -> fib (n - 2)) in
      T.pure (x + y)

  let () = begin
    assert (is_even (List.of_seq (Seq.ints 0 |> Seq.take 0)) |> T.result = true);
    assert (is_even (List.of_seq (Seq.ints 0 |> Seq.take 3)) |> T.result = false);
    assert (is_even (List.of_seq (Seq.ints 0 |> Seq.take 1000)) |> T.result = true);
  end

  let () = begin
    assert (is_odd (List.of_seq (Seq.ints 0 |> Seq.take 0)) |> T.result = false);
    assert (is_odd (List.of_seq (Seq.ints 0 |> Seq.take 3)) |> T.result = true);
    assert (is_odd (List.of_seq (Seq.ints 0 |> Seq.take 1000)) |> T.result = false);
  end

  let () = begin
    assert (fib 1 |> T.result = 1);
    assert (fib 2 |> T.result = 1);
    assert (fib 3 |> T.result = 2);
    assert (fib 4 |> T.result = 3);
    assert (fib 5 |> T.result = 5);
    assert (fib 6 |> T.result = 8);
    assert (fib 7 |> T.result = 13);
    (* ... *)
    assert (fib 30 |> T.result = 832040);
  end

end
