(** COMEFROM in OCaml, implemented with algebraic effects and handlers.
    Based on https://effekt-lang.org/examples/comefrom.html
*)

module Come_from () : sig

  val label : unit -> unit (* Label *)
  val try_with : ?label:(unit -> unit (* E *)) -> ('a -> 'b (* Label *)) -> 'a -> 'b (* E *)

end = struct

  type 'a Effect.t += Label : unit Effect.t

  let label () = Effect.perform Label

  let try_with ?(label=label) f x =
    let open Effect.Deep in
    try_with f x
      { effc = fun (type a) (eff : a Effect.t) ->
          match eff with
          | Label -> Option.some @@ fun (k : (a, _) continuation) ->
            continue k (label ())
          | _ -> None
      }

end

module Come_from0 = Come_from ()
module Come_from1 = Come_from ()
module Come_from2 = Come_from ()

let hello_world () : unit (* Come_from0, Come_from1, Come_from2 *) =
  Come_from0.label ();
  Printf.printf "Hello\n";
  Come_from1.label ();
  Printf.printf "World\n";
  Come_from2.label ()

let () =
  ((fun () -> hello_world ())
    |> Come_from0.try_with ~label:(fun () -> Printf.printf "Came from 0\n")
    |> Come_from1.try_with ~label:(fun () -> Printf.printf "Came from 1\n")
    |> Come_from2.try_with ~label:(fun () -> Printf.printf "Came from 2\n")) ()
