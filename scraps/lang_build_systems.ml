(** Memoized build system using OCaml 5’s algebraic effects and handlers. This
    was inspired by the paper “Build systems à la carte: Theory and practice”.

    A similar approach could eventually be used as a basis for query based,
    demand-driven compilers (see {{: https://github.com/ollef/rock} Rock} and
    {{: https://github.com/ollef/rock} Sixty}). The hope is to clarify some of
    the essential ideas behind demand-driven language tooling, which currently
    remains somewhat of a of a dark art.

    Using algebraic effects is convenient in OCaml compared to the monadic
    approach use in Build systems à la carte, but it limits how much static
    analysis we can do to the build tasks without first executing them. For
    example, we cannot compute the dependencies of our build tasks without first
    running them. This would require effects and handlers that support
    non-monadic forms of effectful computation (see {{: https://doi.org/10.1145/2633628.2633636}
    “Algebraic effects and effect handlers for idioms and arrows”}).

    {2 Resources}

    - {{: https://doi.org/10.1017/S0956796820000088} Build systems à la carte: Theory and practice}
    - {{: https://effekt-lang.org/docs/casestudies/buildsystem} Effekt Language Case Studies: Build System}
    - {{: https://ollef.github.io/blog/posts/query-based-compilers.html} Query-based compiler architectures}
    - {{: https://www.youtube.com/watch?v=3D-ngGIP4fQ} Query-based compiler architectures}
*)

(*
    Todo list:

    - [x] Basic build system
    - [x] Memoized builds
    - [ ] Decouple inputs from build results
    - [ ] Incremental rebuilds
    - [ ] Cycle detection
*)

module Build_system = struct

  module type S = sig

    type key
    type value

    (** Fetch a build result for the supplied target *)
    val fetch : key -> value (* Fetch *)

    (** Run the build system, recursively fetching dependencies *)
    val run : (key -> value (* Fetch *)) -> (unit -> 'a (* Fetch *)) -> 'a

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

    let memoize (tasks : key -> value (* Fetch *)) : key -> value (* Fetch *) =
      let store : (key, value) Hashtbl.t = Hashtbl.create 0 in

      fun target ->
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

  module Spreadsheet = struct

    module B = Build_system.Make (String) (Int)

    (** Spreadsheet example from section 3.2 of “Build systems à la carte: Theory
        and practice”:

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

    let () =
      let@ () = B.run spreadsheet1 in
      Printf.printf "Spreadsheet 1\n\n";
      Printf.printf "Result: %i\n\n" (B.fetch "B2")


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

    let () =
      let@ () = B.run spreadsheet2 in
      Printf.printf "Spreadsheet 2\n\n";
      Printf.printf "Result: %i\n\n" (B.fetch "B2")

    let () =
      let@ () = B.run (B.memoize spreadsheet2) in
      Printf.printf "Spreadsheet 2 (Memoized)\n\n";
      Printf.printf "Result: %i\n\n" (B.fetch "B2")

  end

  (** Fibonacci example from section 3.8 of “Build systems à la carte: Theory
      and practice” *)
  module Fibonacci = struct

    module B = Build_system.Make (Int) (Int)

    let tasks (n : int) : int (* B.Fetch *) =
      Printf.printf "Fetch: %i\n" n;
      match n with
      | 0 -> 0
      | 1 -> 1
      | n -> B.fetch (n - 1) + B.fetch (n - 2)

    let () =
      let@ () = B.run tasks in
      Printf.printf "Fibonacci\n\n";
      Printf.printf "Result: %i\n\n" (B.fetch 5)

    let () =
      let@ () = B.run (B.memoize tasks) in
      Printf.printf "Fibonacci (Memoized)\n\n";
      Printf.printf "Result: %i\n\n" (B.fetch 5)

  end

end
