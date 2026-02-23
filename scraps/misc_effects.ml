[@@@warning "-unused-value-declaration"]

module Reader = struct

  module type S = sig

    type state

    val get : unit -> state

    val scope : (state -> state) -> (unit -> 'a) -> 'a

    val run : init:state -> (unit -> 'a) -> 'a
    val try_with : ?get:(unit -> state) -> (unit -> 'a) -> 'a

  end

  module Make (A : sig type t end) : S
    with type state = A.t
  = struct

    type state = A.t

    type _ Effect.t +=
      | Get : state Effect.t

    let get () = Effect.perform Get

    let run ~(init : state) prog =
      let open Effect.Deep in
      try prog () with
      | effect Get, k -> continue k init

    let scope f prog =
      run ~init:(f (get ())) prog

    let try_with ?(get = get) prog =
      let open Effect.Deep in
      try prog () with
      | effect Get, k -> continue k (get ())

  end

end

module State = struct

  module type S = sig

    type state

    val set : state -> unit
    val get : unit -> state

    val modify : (state -> state) -> unit

    val run : init:state -> (unit -> 'a) -> 'a
    val try_with : ?set:(state -> unit) -> ?get:(unit -> state) -> (unit -> 'a) -> 'a

  end

  module Make (A : sig type t end) : S
    with type state = A.t
  = struct

    type state = A.t

    type _ Effect.t +=
      | Set : state -> unit Effect.t
      | Get : state Effect.t

    let set x = Effect.perform (Set x)
    let get () = Effect.perform Get

    let run ~(init : state) f =
      let open Effect.Deep in

      let curr = ref init in
      try f () with
      | effect (Set x), k -> curr := x; continue k ()
      | effect Get, k -> continue k !curr

    let modify f = set (f (get ()))

    let try_with ?(set = set) ?(get = get) prog =
      let open Effect.Deep in

      try prog () with
      | effect (Set x), k -> continue k (set x)
      | effect Get, k -> continue k (get ())

  end

end


let ( let@ ) = ( @@ )

let () = begin

  let module R = Reader.Make (String) in

  let@ () = R.run ~init:"hello" in
  assert (R.get () = "hello");
  begin R.scope (fun x -> x ^ " world!") @@ fun () ->
    assert (R.get () = "hello world!");
  end;
  assert (R.get () = "hello");

end

let () = begin

  let module S = State.Make (String) in

  let@ () = S.run ~init:"hello" in
  S.modify (fun x -> x ^ " world!");
  assert (S.get () = "hello world!");

end
