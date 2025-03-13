(** COMEFROM in OCaml, implemented with algebraic effects and handlers.
    Based on https://effekt-lang.org/examples/comefrom.html
*)

module ComeFrom () : sig

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

module ComeFrom0 = ComeFrom ()
module ComeFrom1 = ComeFrom ()
module ComeFrom2 = ComeFrom ()

let hello_world () : unit (* ComeFrom0, ComeFrom1, ComeFrom2 *) =
  ComeFrom0.label ();
  Printf.printf "Hello\n";
  ComeFrom1.label ();
  Printf.printf "World\n";
  ComeFrom2.label ()

let () =
  ((fun () -> hello_world ())
    |> ComeFrom0.try_with ~label:(fun () -> Printf.printf "Came from 0\n")
    |> ComeFrom1.try_with ~label:(fun () -> Printf.printf "Came from 1\n")
    |> ComeFrom2.try_with ~label:(fun () -> Printf.printf "Came from 2\n")) ()
