(** Memoized build system using OCaml 5’s algebraic effects and handlers.
    This is based on an example from the Effekt language documentation, which
    was in turn inspired by “Build systems à la carte” by Mokhov et. al.

    {2 Resources}

    - {{:https://effekt-lang.org/docs/casestudies/buildsystem} Effekt Language Case Studies: Build System}
    - {{:https://doi.org/10.1017/S0956796820000088} Build systems à la carte: Theory and practice}
*)

type key = string
type value = int

type _ Effect.t +=
  | Fetch : key -> value Effect.t
  | Need_input : key -> value Effect.t

let fetch key (* Fetch *) = Effect.perform (Fetch key)
let need_input key (* Need_input *) = Effect.perform (Need_input key)

(** Run the build tasks, recursively fetching dependencies *)
let rec build (target : key) (tasks : key -> value (* Fetch *)) : value =
  try tasks target with
  | effect (Fetch key), k ->
      Effect.Deep.continue k (build key tasks)

(** Reuse previous build results if they are already present in the store *)
let memoize (type a) (prog : unit -> a (* Fetch *)) : a (* Fetch *) =
  let store : (key, value) Hashtbl.t =
    Hashtbl.create 0
  in

  try prog () with
  | effect (Fetch key), k ->
      match Hashtbl.find_opt store key with
      | Some value ->
          Effect.Deep.continue k value
      | None ->
          let value = fetch key in
          Hashtbl.add store key value;
          Effect.Deep.continue k value

(** Run the build system, supplying inputs when requested *)
let supply_input (type a) (inputs : key -> value (* e *)) (prog : unit -> a (* Need_input, e *)) : a (* e *) =
  try prog () with
  | effect (Need_input key), k ->
      Effect.Deep.continue k (inputs key)


module Examples = struct

  (** Spreadsheet example from “Build systems ala Carte” *)
  let spreadsheet1 (key : key) : value (* Fetch, Need_input *) =
    Printf.printf "Fetch: %s\n" key;
    match key with
    | "B1" -> fetch "A1" + fetch "A2"
    | "B2" -> fetch "B1" * 2
    | key -> need_input key

  (** Spreadsheet example that needs the same key twice *)
  let spreadsheet2 (key : key) : value (* Fetch, Need_input *) =
    Printf.printf "Fetch: %s\n" key;
    match key with
    | "B1" -> fetch "A1" + fetch "A2"
    | "B2" -> fetch "B1" * fetch "B1"
    | key -> need_input key

  let ( let@ ) = ( @@ )

  let () =
    let exception Key_not_found of string in

    let inputs key =
      match key with
      | "A1" -> 10
      | "A2" -> 20
      | key -> raise (Key_not_found key)
    in

    try

      Printf.printf "Spreadsheet 1\n\n";
      let result =
        let@ () = supply_input inputs in
        build "B2" spreadsheet1
      in
      Printf.printf "Result: %i\n\n" result;

      Printf.printf "Spreadsheet 2\n\n";
      let result =
        let@ () = supply_input inputs in
        build "B2" spreadsheet2
      in
      Printf.printf "Result: %i\n\n" result;

      Printf.printf "Spreadsheet 2 (Memoized)\n\n";
      let result =
        let@ () = supply_input inputs in
        let@ key = build "B2" in
        let@ () = memoize in
        spreadsheet2 key
      in
      Printf.printf "Result: %i\n\n" result;

    with
    | Key_not_found key ->
        Printf.printf "Key not found: %s\n" key

  (*

    Expected output:

      Example 1

      Fetch: B2
      Fetch: B1
      Fetch: A2
      Fetch: A1
      Result: 60

      Example 2

      Fetch: B2
      Fetch: B1
      Fetch: A2
      Fetch: A1
      Fetch: B1
      Fetch: A2
      Fetch: A1
      Result: 900

      Example 2 (Memoized)

      Fetch: B2
      Fetch: B1
      Fetch: A2
      Fetch: A1
      Result: 900

  *)

end
