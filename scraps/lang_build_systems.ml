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

module Build_system = struct

  module type S = sig

    type key
    type value

    val fetch : key -> value (* Fetch *)
    val need_input : key -> value (* Need_input *)

    (** Run the build tasks, recursively building dependencies *)
    val build : (key -> value (* Fetch *)) -> key -> value

    (** A build system transformer that reuses previous build results *)
    val memoize : (key -> value (* Fetch *)) -> key -> value (* Fetch *)

    (** Run the build system, providing inputs when requested *)
    val supply_input : (key -> value) -> ('a -> 'b (* Need_input *)) -> 'a -> 'b

  end

  module Make (Key : sig type t end) (Value : sig type t end) : S
    with type key = Key.t
    with type value = Value.t
  = struct

    type key = Key.t
    type value = Value.t

    type _ Effect.t +=
      | Fetch : key -> value Effect.t
      | Need_input : key -> value Effect.t

    let fetch key (* Fetch *) = Effect.perform (Fetch key)
    let need_input key (* Need_input *) = Effect.perform (Need_input key)

    let rec build (tasks : key -> value (* Fetch *)) (target : key) : value =
      try tasks target with
      | effect (Fetch target), k ->
          Effect.Deep.continue k (build tasks target)

    let memoize (tasks : key -> value (* Fetch *)) (target : key) : value (* Fetch *) =
      let store : (key, value) Hashtbl.t = Hashtbl.create 0 in

      try tasks target with
      | effect (Fetch target), k ->
          match Hashtbl.find_opt store target with
          | Some value ->
              Effect.Deep.continue k value
          | None ->
              let value = fetch target in
              Hashtbl.add store target value;
              Effect.Deep.continue k value

    let supply_input (type a b) (inputs : key -> value) (prog : a -> b (* Need_input *)) (x : a) : b =
      try prog x with
      | effect (Need_input key), k ->
          Effect.Deep.continue k (inputs key)

  end

end


module Examples = struct

  module B = Build_system.Make (String) (Int)

  (** Spreadsheet example from “Build systems ala Carte” *)
  let spreadsheet1 (target : string) : int (* B.Fetch, B.Need_input *) =
    Printf.printf "Fetch: %s\n" target;
    match target with
    | "B1" -> B.fetch "A1" + B.fetch "A2"
    | "B2" -> B.fetch "B1" * 2
    | target -> B.need_input target

  (** Spreadsheet example that needs the same key twice *)
  let spreadsheet2 (target : string) : int (* B.Fetch, B.Need_input *) =
    Printf.printf "Fetch: %s\n" target;
    match target with
    | "B1" -> B.fetch "A1" + B.fetch "A2"
    | "B2" -> B.fetch "B1" * B.fetch "B1"
    | target -> B.need_input target

  let () =
    let exception Key_not_found of string in

    let inputs target =
      match target with
      | "A1" -> 10
      | "A2" -> 20
      | target -> raise (Key_not_found target)
    in

    try

      Printf.printf "Spreadsheet 1\n\n";
      let result = B.supply_input inputs (B.build spreadsheet1) "B2" in
      Printf.printf "Result: %i\n\n" result;

      Printf.printf "Spreadsheet 2\n\n";
      let result = B.supply_input inputs (B.build spreadsheet2) "B2" in
      Printf.printf "Result: %i\n\n" result;

      Printf.printf "Spreadsheet 2 (Memoized)\n\n";
      let result = B.supply_input inputs (B.build (B.memoize spreadsheet2)) "B2" in
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
