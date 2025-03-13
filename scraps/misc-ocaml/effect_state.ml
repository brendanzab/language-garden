module State (A : sig type t end) : sig

  type t = A.t

  val set : t -> unit
  val get : unit -> t

  val run : t -> (unit -> 'a) -> 'a

end = struct

  type t = A.t

  type _ Effect.t +=
    | Set : t -> unit Effect.t
    | Get : t Effect.t

  let set x = Effect.perform (Set x)
  let get () = Effect.perform Get

  let run (init : t) f =
    let curr = ref init in

    try f () with
    | effect (Set x), k ->
        curr := x;
        Effect.Deep.continue k ()
    | effect Get, k ->
        Effect.Deep.continue k !curr

end

let ( let@ ) = ( @@ )

let () =
  let module S = State (String) in
  let@ () = S.run "hello" in
  S.set (S.get () ^ " world!");
  print_string (S.get ())
