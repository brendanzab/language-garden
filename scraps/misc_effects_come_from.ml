(** COMEFROM in OCaml, implemented with algebraic effects and handlers.
    Based on https://effekt-lang.org/examples/comefrom.html
*)

module Come_from () : sig

  val label : unit -> unit (* Label *)
  val try_with : ?label:(unit -> unit (* e *)) -> (unit -> 'a (* Label *)) -> 'a (* e *)

end = struct

  type 'a Effect.t += Label : unit Effect.t

  let label () = Effect.perform Label

  let try_with ?(label = label) f =
    try f () with
    | effect Label, k ->
        Effect.Deep.continue k (label ())

end

module Come_from0 = Come_from ()
module Come_from1 = Come_from ()
module Come_from2 = Come_from ()

let hello_world () : unit (* Come_from0.Label, Come_from1.Label, Come_from2.Label *) =
  Come_from0.label ();
  Printf.printf "Hello\n";
  Come_from1.label ();
  Printf.printf "World\n";
  Come_from2.label ()

let ( let* ) = ( @@ )

let () =
  let* () = Come_from0.try_with ~label:(fun () -> Printf.printf "Came from 0\n") in
  let* () = Come_from1.try_with ~label:(fun () -> Printf.printf "Came from 1\n") in
  let* () = Come_from2.try_with ~label:(fun () -> Printf.printf "Came from 2\n") in
  hello_world ()
