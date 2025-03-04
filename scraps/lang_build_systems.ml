(** Memoized build system using OCaml 5’s algebraic effects and handlers.
    This is based on an example from the Effekt language documentation, which
    was in turn inspired by “Build systems à la carte” by Mokhov et. al.

    A similar approach could eventually be used as a basis for query based,
    demand-driven compilers (see {{:https://github.com/ollef/rock} Rock} and
    {{:https://github.com/ollef/rock} Sixty}).

    {2 Resources}

    - {{:https://effekt-lang.org/docs/casestudies/buildsystem} Effekt Language Case Studies: Build System}
    - {{:https://doi.org/10.1017/S0956796820000088} Build systems à la carte: Theory and practice}
    - {{:https://ollef.github.io/blog/posts/query-based-compilers.html} Query-based compiler architectures}
    - {{:https://www.youtube.com/watch?v=3D-ngGIP4fQ} Query-based compiler architectures}
*)

type key = string
type value = int

type _ Effect.t +=
  | Fetch : key -> value Effect.t
  | Need_input : key -> value Effect.t

let fetch key (* Fetch *) = Effect.perform (Fetch key)
let need_input key (* Need_input *) = Effect.perform (Need_input key)

(** Run the build tasks, recursively building dependencies *)
let rec build (target : key) (tasks : key -> value (* Fetch *)) : value =
  try tasks target with
  | effect (Fetch key), k ->
      Effect.Deep.continue k (build key tasks)

(** A build system transformer that reuses previous build results *)
let memoize (tasks : key -> value (* Fetch *)) (key : key) : value (* Fetch *) =
  let store : (key, value) Hashtbl.t =
    Hashtbl.create 0
  in

  try tasks key with
  | effect (Fetch key), k ->
      match Hashtbl.find_opt store key with
      | Some value ->
          Effect.Deep.continue k value
      | None ->
          let value = fetch key in
          Hashtbl.add store key value;
          Effect.Deep.continue k value

(** Run the build system, providing inputs when requested *)
let supply_input (type a) (inputs : key -> value) (prog : unit -> a (* Need_input *)) : a =
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
        build "B2" (memoize spreadsheet2)
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
