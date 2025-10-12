[@@@warning "-unused-value-declaration"]

module State (A : sig type t end) : sig

  type t = A.t

  val set : t -> unit
  val get : unit -> t

  val modify : (t -> t) -> unit

  val run : init:t -> (unit -> 'a) -> 'a
  val try_with : ?set:(t -> unit) -> ?get:(unit -> t) -> (unit -> 'a) -> 'a

end = struct

  type t = A.t

  type _ Effect.t +=
    | Set : t -> unit Effect.t
    | Get : t Effect.t

  let set x = Effect.perform (Set x)
  let get () = Effect.perform Get

  let modify f = set (f (get ()))

  let run ~(init : t) f =
    let open Effect.Deep in

    let curr = ref init in
    try f () with
    | effect (Set x), k -> curr := x; continue k ()
    | effect Get, k -> continue k !curr

  let try_with ?(set = set) ?(get = get) f =
    let open Effect.Deep in

    try f () with
    | effect (Set x), k -> continue k (set x)
    | effect Get, k -> continue k (get ())

end

let ( let@ ) = ( @@ )

let () =
  let module S = State (String) in
  let@ () = S.run ~init:"hello" in
  S.modify (fun x -> x ^ " world!");
  assert (S.get () = "hello world!")
