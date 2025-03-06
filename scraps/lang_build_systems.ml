(** Memoized build system using OCaml 5’s algebraic effects and handlers.

    A similar approach could eventually be used as a basis for query based,
    demand-driven compilers (see {{:https://github.com/ollef/rock} Rock} and
    {{:https://github.com/ollef/rock} Sixty}).

    {2 Resources}

    - {{:https://doi.org/10.1017/S0956796820000088} Build systems à la carte: Theory and practice}
    - {{:https://effekt-lang.org/docs/casestudies/buildsystem} Effekt Language Case Studies: Build System}
    - {{:https://ollef.github.io/blog/posts/query-based-compilers.html} Query-based compiler architectures}
    - {{:https://www.youtube.com/watch?v=3D-ngGIP4fQ} Query-based compiler architectures}
*)

module Build_system = struct

  module type S = sig

    type key
    type value

    val fetch : key -> value (* Fetch *)

    (** Run the build system, recursively fetching dependencies *)
    val run : (key -> value (* Fetch *)) -> (unit -> 'a) -> 'a

    (** A build system transformer that reuses previous build results *)
    val memoize : (key -> value (* Fetch *)) -> key -> value (* Fetch *)

  end

  module Make (Key : sig type t end) (Value : sig type t end) : S
    with type key = Key.t
    with type value = Value.t
  = struct

    type key = Key.t
    type value = Value.t

    type _ Effect.t +=
      | Fetch : key -> value Effect.t

    let fetch key (* Fetch *) = Effect.perform (Fetch key)

    let run (type a) (tasks : key -> value (* Fetch *)) (prog : unit -> a) : a =
      let rec fetch (target : key) : value =
        try tasks target with
        | effect (Fetch target), k ->
            Effect.Deep.continue k (fetch target)
      in
      try prog () with
      | effect (Fetch target), k ->
          Effect.Deep.continue k (fetch target)

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

  end

end


module Examples = struct

  let ( let@ ) = ( @@ )

  module B = Build_system.Make (String) (Int)

  (** Spreadsheet example from “Build systems ala Carte”

      {t
        |   | A  | B       |
        | - | -- | ------- |
        | 1 | 10 | A1 + A2 |
        | 2 | 20 | B1 * 2  |
      }
  *)
  let spreadsheet1 (target : string) : int (* B.Fetch, Not_found *) =
    Printf.printf "Fetch: %s\n" target;
    match target with
    | "A1" -> 10
    | "A2" -> 20
    | "B1" -> B.fetch "A1" + B.fetch "A2"
    | "B2" -> B.fetch "B1" * 2
    | _ -> raise Not_found

  let () = begin

    let@ () = B.run spreadsheet1 in
    Printf.printf "Spreadsheet 1\n\n";
    Printf.printf "Result: %i\n\n" (B.fetch "B2");

  end

  (*

    Expected output:

      Example 1

      Fetch: B2
      Fetch: B1
      Fetch: A2
      Fetch: A1
      Result: 60

  *)


  (** Spreadsheet example that fetches the same key twice

      {t
        |   | A  | B       |
        | - | -- | ------- |
        | 1 | 10 | A1 + A2 |
        | 2 | 20 | B1 * B1 |
      }
  *)
  let spreadsheet2 (target : string) : int (* B.Fetch, Not_found *) =
    Printf.printf "Fetch: %s\n" target;
    match target with
    | "A1" -> 10
    | "A2" -> 20
    | "B1" -> B.fetch "A1" + B.fetch "A2"
    | "B2" -> B.fetch "B1" * B.fetch "B1"
    | _ -> raise Not_found

  let () = begin

    let@ () = B.run spreadsheet2 in
    Printf.printf "Spreadsheet 2\n\n";
    Printf.printf "Result: %i\n\n" (B.fetch "B2");

  end

  (*

    Expected output:

      Example 2

      Fetch: B2
      Fetch: B1
      Fetch: A2
      Fetch: A1
      Fetch: B1
      Fetch: A2
      Fetch: A1
      Result: 900

  *)

  let () = begin

    let@ () = B.run (B.memoize spreadsheet2) in
    Printf.printf "Spreadsheet 2 (Memoized)\n\n";
    Printf.printf "Result: %i\n\n" (B.fetch "B2");

  end

  (*

    Expected output:

      Example 2 (Memoized)

      Fetch: B2
      Fetch: B1
      Fetch: A2
      Fetch: A1
      Result: 900

  *)

end
