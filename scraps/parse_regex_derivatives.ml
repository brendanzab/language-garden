(** Regular expression matching with derivatives.

    This implementation was based on the following paper:

    - Scott Owens, John Reppy, and Aaron Turon. 2009.
      Regular-expression derivatives re-examined.
      https://doi.org/10.1017/S0956796808007090

    I found the biggest challenge was figuring out how to normalise the regular
    expressions based on the equivalences in section 4.1. Initially I naively
    implemented the matching from section 3.2 without this, then realised the
    algorithm could only make progress if the expressions were normalised first.

    Related implementations and links:

    - https://github.com/monaddle/regex-deriv/blob/master/src/main/scala/com/github/dlomsak/regex/deriv/
    - https://github.com/bobatkey/foveran/blob/master/src/Data/FiniteStateMachine/RegexpDerivatives.hs
    - https://matt.might.net/articles/implementation-of-regular-expression-matching-in-scheme-with-derivatives/
    - https://well-typed.com/blog/2020/06/fix-ing-regular-expressions/
      - https://github.com/ulysses4ever/rere/blob/master/rere.m
*)

module type Symbol = sig

  type t

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pp : t -> Format.formatter -> unit

end

module Regex = struct

  module type S = sig

    type t
    (** Regular expressions in normal form *)

    module Symbol : Symbol
    (** An alphabet of symbols *)

    val fail : t
    val empty : t
    val symbol : Symbol.t -> t
    val concat : t -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val repeat : t -> t
    val compl : t -> t

    val compare : t -> t -> int
    (** Compare two regular expressions lexicographically *)

    val pp : t -> Format.formatter -> unit
    (** Pretty print a regular expression *)


    (** Regular expression matching with derivatives *)

    val nullable : t -> t
    (** [nullable r] returns either:

        - [empty] if the expression matches the empty string
        - [fail] if the expression never matches the empty string
    *)

    val derivative : t -> Symbol.t -> t
    (** [derivative r a] returns a regular expression that matches the suffix of
        strings matched by [r], beginning with a leading [a] symbol. *)

    val match_seq : t -> Symbol.t Seq.t -> bool
    (** [match_seq r seq] returns true if the sequence of symbols [seq] is in
        the language described by [r]. *)

    type regex = t

    module Dfa : sig

      type t
      (** Deterministic finite automaton (DFA) *)

      val make : regex -> t
      (** Compile a regular expression to a DFA *)

      val match_seq : t -> Symbol.t Seq.t -> bool
      (** [match_seq dfa seq] returns true if the sequence of symbols is
          accepted by the DFA. *)

      (* TODO: Would be neat if we could output the DFA as Graphviz DOT
         See https://github.com/katef/libfsm for inspiration! *)

    end

  end

  module Make (A : Symbol) : S
    with type Symbol.t = A.t
  = struct

    (** {1 Syntax (Section 2.1)} *)

    module Symbol = A

    type t =
      | Fail                  (* ∅ *)
      | Empty                 (* ε *)
      | Symbol of Symbol.t    (* ɑ *)
      | Concat of t * t       (* r · s *)
      | Repeat of t           (* r* *)
      | Union of t * t        (* r + s *)
      | Inter of t * t        (* r & s *)
      | Compl of t            (* ¬r *)

    (** {1 Pretty printing} *)

    let rec pp (r : t) (ppf : Format.formatter) =
      match r with
      (* | Concat (r, (Concat _ as s)) -> Format.fprintf ppf "%t · %t" (pp_atom r) (pp s)
      | Union (r, (Union _ as s)) -> Format.fprintf ppf "%t + %t" (pp_atom r) (pp s)
      | Inter (r, (Inter _ as s)) -> Format.fprintf ppf "%t & %t" (pp_atom r) (pp s) *)
      | Concat (r, s) -> Format.fprintf ppf "%t · %t" (pp_atom r) (pp_atom s)
      | Union (r, s) -> Format.fprintf ppf "%t + %t" (pp_atom r) (pp_atom s)
      | Inter (r, s) -> Format.fprintf ppf "%t & %t" (pp_atom r) (pp_atom s)
      | Repeat r -> Format.fprintf ppf "%t*" (pp_atom r)
      | Compl r -> Format.fprintf ppf "¬%t" (pp_atom r)
      | r -> pp_atom r ppf

    and pp_atom (r : t) (ppf : Format.formatter) =
      match r with
      | Fail -> Format.fprintf ppf "∅"
      | Empty -> Format.fprintf ppf "ε"
      | Symbol a -> Format.fprintf ppf "%t" (A.pp a)
      | Concat _ | Union _ | Inter _ | Repeat _ | Compl _ as r ->
          Format.fprintf ppf "(%t)" (pp r)


    (** {1 Lexicographic ordering (Section 4.1)} *)

    (* NOTE: we could use ppx_derive if this was a full project *)

    let variant_id r =
      match r with
      | Fail -> 0
      | Empty -> 1
      | Symbol _ -> 2
      | Concat _ -> 3
      | Repeat _ -> 4
      | Union _ -> 5
      | Inter _ -> 6
      | Compl _ -> 7

    let rec compare (r : t) (s : t) : int =
      let compare_pair = Pair.compare compare compare in
      match r, s with
      | Fail, Fail -> 0
      | Empty, Empty -> 0
      | Symbol a, Symbol b -> A.compare a b
      | Concat (r1, s1), Concat (r2, s2)  -> compare_pair (r1, s1) (r2, s2)
      | Repeat r, Repeat s -> compare r s
      | Union (r1, s1), Union (r2, s2) -> compare_pair (r1, s1) (r2, s2)
      | Inter (r1, s1), Inter (r2, s2) -> compare_pair (r1, s1) (r2, s2)
      | Compl r, Compl s -> compare r s
      | r, s -> Int.compare (variant_id r) (variant_id s)


    (** {1 Smart constructors (Section 4.1)} *)

    (** Smart constructors for building regular expressions in normal form.
        These are based on the equivalences in section 4.1. *)

    let fail = Fail
    let empty = Empty
    let symbol s = Symbol s

    let concat r s =
      match r, s with
      | Fail, _ -> Fail                               (* ∅ · r ≈ ∅ *)
      | _, Fail -> Fail                               (* r · ∅ ≈ ∅ *)
      | Empty, r -> r                                 (* ε · r ≈ r *)
      | r, Empty -> r                                 (* r · ε ≈ r *)
      | Concat (r, s), t -> Concat (r, Concat (s, t)) (* (r · s) · t ≈ r · (s · t) *)
      | r, s -> Concat (r, s)

    let rec repeat r =
      match r with
      | Repeat r -> repeat r                          (* ( r* )* ≈ r* *)
      | Empty -> Empty                                (* ε* ≈ ε *)
      | Fail -> Empty                                 (* ∅* ≈ ε *)
      | r -> Repeat r

    let compl r =
      match r with
      | Compl r -> r                                  (* ¬(¬r) ≈ r *)
      | r -> Compl r

    (** [sort_commute ctor r s] will apply [ctor], sorting the arguments
        lexically, and avoiding idempotent calls. *)
    let sort_commute ctor r s =
      let c_rs = compare r s in
      if c_rs = 0 then r
      else if c_rs > 0 then ctor s r
      else ctor r s

    (** [sort_assoc ctor (r, s)] will right-associate nested [ctor]
        applications, assuming [r < s]. The arguments will be sorted lexically,
        and idempotent calls will be avoided. *)
    let sort_assoc ctor (r, s) t =
      let c_rt = compare r t in
      if c_rt = 0 then ctor r s
      else if c_rt > 0 then ctor t (ctor r s)
      else ctor r (sort_commute ctor s t)

    let union r s =
      let union r s = Union (r, s) in
      match r, s with
      | Fail, r | r, Fail -> r                        (* ∅ + r ≈ r *)
      | Compl Fail, r | r, Compl Fail -> Compl Fail   (* ¬∅ + r ≈ ¬∅ *)
      (* r + r ≈ r *)
      (* r + s ≈ s + r *)
      (* (r + s) + t ≈ r + (s + t) *)
      | Union (r, s), t | t, Union (r, s) -> sort_assoc union (r, s) t
      | r, s ->  sort_commute union r s

    let inter r s =
      let inter r s = Inter (r, s) in
      match r, s with
      | Fail, r | r, Fail -> Fail                     (* ∅ & r ≈ ∅ *)
      | Compl Fail, r | r, Compl Fail -> r            (* ¬∅ + r ≈ r *)
      (* r & r ≈ r *)
      (* r & s ≈ s & r *)
      (* (r & s) & t ≈ r & (s & t) *)
      | Inter (r, s), t | t, Inter (r, s) -> sort_assoc inter (r, s) t
      | r, s -> sort_commute inter r s


    (** {1 Derivatives (Section 3.1)} *)

    (** These operations return expressions in normal form (Section 4.1),
        avoiding the need to perform this step during [match_seq]. *)

    (** Instead of returning a regular expression, we could return a boolean
        instead, as demonstrated in {{: https://doi.org/10.1145/2034574.2034801}
        “Parsing with Derivatives: A Functional Pearl”} by Might, Darais, and
        Spiewak. This might skip some of the overhead of normalising
        derivatives. *)
    let rec nullable (r : t) : t =
      match r with
      | Fail -> fail
      | Empty -> empty
      | Symbol _ -> fail
      | Concat (r, s) -> inter (nullable r) (nullable s)
      | Repeat _ -> empty
      | Union (r, s) -> union (nullable r) (nullable s)
      | Inter (r, s) -> inter (nullable r) (nullable s)
      | Compl r ->
          match nullable r with
          | Fail -> empty
          | Empty -> fail
          | _ -> failwith "expected Fail or Empty"

    let rec derivative (r : t) (a : Symbol.t) : t =
      match r with
      | Fail -> fail
      | Empty -> fail
      | Symbol b -> if A.equal a b then empty else fail
      | Concat (r, s) -> union (concat (derivative r a) s) (concat (nullable r) (derivative s a))
      | Repeat r -> concat (derivative r a) (repeat r)
      | Union (r, s) -> union (derivative r a) (derivative s a)
      | Inter (r, s) -> inter (derivative r a) (derivative s a)
      | Compl r -> compl (derivative r a)


    (** {1 Matching (Section 3.2)} *)

    let rec match_seq (r : t) (cs : Symbol.t Seq.t) =
      match Seq.uncons cs with
      | None -> nullable r = Empty
      | Some (c, cs) -> match_seq (derivative r c) cs


    (** {1 DFAs (Section 3.3)} *)

    type regex = t

    module Dfa = struct

      (* NOTE: This currently uses regular expressions directly for states.
         Interning these as integers might help to improve the performance of
         map operations. *)

      module Symbol_set = Set.Make (Symbol)

      module State_set = Set.Make (struct
        type t = regex
        let compare = compare
      end)

      module Trans_map = Map.Make (struct
        type t = regex * Symbol.t
        let compare = Pair.compare compare Symbol.compare
      end)

      type t = {
        states : State_set.t;
        start_state : regex;
        final_states : State_set.t;
        trans : regex Trans_map.t;
      }

      (** {1 DFA Construction (Section 3.3)} *)

      type partial_dfa = {
        used_symbols : Set.Make (Symbol).t;
        states : State_set.t;
        trans : regex Trans_map.t;
      }

      (** Returns a set of the symbols that appear in a regular expression *)
      let used_symbols (r : regex) : Symbol_set.t =
        let rec go r symbols =
          match r with
          | Fail -> symbols
          | Empty -> symbols
          | Symbol a -> Symbol_set.add a symbols
          | Repeat r | Compl r -> go r symbols
          | Concat (r, s) | Union (r, s) | Inter (r, s) -> symbols |> go r |> go s
        in
        go r Symbol_set.empty

      let rec goto (q : regex) (c : Symbol.t) (dfa : partial_dfa) : partial_dfa =
        let qc = derivative q c in
        if State_set.mem qc dfa.states then
          let trans = dfa.trans |> Trans_map.add (q, c) qc in
          { dfa with trans }
        else
          let states = dfa.states |> State_set.add qc in
          let trans = dfa.trans |> Trans_map.add (q, c) qc in
          explore qc { dfa with states; trans }

      and explore (q : regex) (dfa : partial_dfa) : partial_dfa =
        Symbol_set.fold (goto q) dfa.used_symbols dfa

      (** Construct a DFA from a regular expression *)
      let make (r : regex) : t =
        let start_state = r in
        let { states; trans; _ } = explore start_state {
          used_symbols = used_symbols r;
          states = State_set.singleton start_state;
          trans = Trans_map.empty;
        } in
        let final_states = states |> State_set.filter (fun q -> nullable q = Empty) in
        { states; start_state; final_states; trans }

      (** Match a sequence of characters using a DFA (See section 2.2) *)
      let match_seq (dfa : t) (cs : Symbol.t Seq.t) =
        let ( let* ) = Option.bind in

        let rec trans q cs =
          match Seq.uncons cs with
          | None -> Some q
          | Some (c, cs) ->
              let* q' = Trans_map.find_opt (q, c) dfa.trans in
              trans q' cs
        in
        match trans dfa.start_state cs with
        | Some final_state -> State_set.mem final_state dfa.final_states
        | None -> false

    end

  end

  (** Regular expressions over strings of characters *)
  module Char : sig

    include S with type Symbol.t = char

    val char : char -> t
    val string : string -> t

    val match_string : t -> string -> bool

    module Dfa : sig

      include module type of Dfa

      val match_string : t -> string -> bool

    end

  end = struct

    include Make (struct
      include Char
      let pp c ppf = Format.pp_print_char ppf c
    end)

    let char (c : char) : t =
      symbol c

    let string (s : string) : t =
      String.fold_right (fun c r -> concat (char c) r) s empty

    let rec match_string (r : t) (cs : string) =
      match_seq r (String.to_seq cs)

    module Dfa = struct

      include Dfa

      let rec match_string (dfa : t) (cs : string) =
        match_seq dfa (String.to_seq cs)

    end

  end

end


module Test = struct

  Printexc.record_backtrace true

  module R = Regex.Char

  let ( * ) = R.concat
  let ( + ) = R.union
  let ( & ) = R.inter

  let a = R.symbol 'a'
  let b = R.symbol 'b'
  let c = R.symbol 'c'

  (* Test normal forms (Section 4.1) *)
  let () = begin

    (* NOTE: would probably be better to use property based tests here *)

    let failures = ref 0 in

    let expect_equals l r =
      if R.compare l r = 0 then () else begin
        Format.eprintf "FAILED: %t = %t\n" R.(pp l) R.(pp r);
        incr failures
      end
    in

    let expect_not_equals l r =
      if R.compare l r <> 0 then () else begin
        Format.eprintf "FAILED: %t <> %t\n" R.(pp l) R.(pp r);
        incr failures
      end
    in

    (* Intersection *)
    begin

      expect_not_equals (a & b) a;

      (* Idempotence *)
      [a; b; c] |> List.iter begin fun r ->
        expect_equals (r & r) r;
        expect_equals r (r & r);
      end;

      (* Commutativity *)
      [a; b; c] |> List.iter begin fun r ->
        [a; b; c] |> List.iter begin fun s ->
          expect_equals (r & s) (r & s);
          expect_equals (s & r) (r & s);
        end;
      end;

      (* Associativity *)
      [a; b; c] |> List.iter begin fun r ->
        [a; b; c] |> List.iter begin fun s ->
          [a; b; c] |> List.iter begin fun t ->
            expect_equals (r & (s & t)) ((r & s) & t);
            expect_equals ((r & s) & t) (r & (s & t));
          end;
        end;
      end;

      [a; b; c] |> List.iter begin fun r ->
        expect_equals (R.fail & r) R.fail;
        expect_equals (r & R.fail) R.fail;
      end;

      [a; b; c] |> List.iter begin fun r ->
        expect_equals (R.compl R.fail & r) r;
        expect_equals (r & R.compl R.fail) r;
      end;

    end;

    (* Union *)
    begin

      expect_not_equals (a + b) a;

      (* Idempotence *)
      [a; b; c] |> List.iter begin fun r ->
        expect_equals (r + r) r;
        expect_equals r (r + r);
      end;

      (* Commutativity *)
      [a; b; c] |> List.iter begin fun r ->
        [a; b; c] |> List.iter begin fun s ->
          expect_equals (r + s) (r + s);
          expect_equals (s + r) (r + s);
        end;
      end;

      (* Associativity *)
      [a; b; c] |> List.iter begin fun r ->
        [a; b; c] |> List.iter begin fun s ->
          [a; b; c] |> List.iter begin fun t ->
            expect_equals (r + (s + t)) ((r + s) + t);
            expect_equals ((r + s) + t) (r + (s + t));
          end;
        end;
      end;

      [a; b; c] |> List.iter begin fun r ->
        expect_equals (R.fail + r) r;
        expect_equals (r + R.fail) r;
      end;

      [a; b; c] |> List.iter begin fun r ->
        expect_equals (R.compl R.fail + r) (R.compl R.fail);
        expect_equals (r + R.compl R.fail) (R.compl R.fail);
      end;

    end;

    (* Concatenation *)
    begin

      (* Associativity *)
      [a; b; c] |> List.iter begin fun r ->
        [a; b; c] |> List.iter begin fun s ->
          [a; b; c] |> List.iter begin fun t ->
            expect_equals (r * (s * t)) ((r * s) * t);
            expect_equals ((r * s) * t) (r * (s * t));
          end;
        end;
      end;

      (* Annihilation *)
      [a; b; c] |> List.iter begin fun r ->
        expect_equals (r * R.fail) R.fail;
        expect_equals (R.fail * r) R.fail;
      end;

      (* Absorption *)
      [a; b; c] |> List.iter begin fun r ->
        expect_equals (r * R.empty) r;
        expect_equals (R.empty * r) r;
      end;

    end;

    (* Repetition *)
    begin

      expect_equals (R.repeat R.empty) R.empty;
      expect_equals R.empty (R.repeat R.empty);

      [a; b; c] |> List.iter begin fun r ->
        expect_equals (R.repeat (R.repeat r)) (R.repeat r);
      end;

    end;

    (* Complement *)
    [a; b; c] |> List.iter begin fun r ->
      expect_equals (R.compl (R.compl r)) r;
      expect_not_equals (R.compl r) r;
    end;

    if !failures > 0 then
      Printf.ksprintf failwith "failed %i test%s"
        !failures
        (if !failures = 1 then "" else "s");

  end

  (* Matching tests *)
  let () = begin

    let failures = ref 0 in
    let expect_match r s e =
      if R.match_string r s = e then () else begin
        Format.eprintf "FAILED: R.match_string %t ~ \"%s\" = %b\n" R.(pp r) s e;
        incr failures
      end
    and expect_dfa_match r dfa s e =
      if R.Dfa.match_string dfa s = e then () else begin
        Format.eprintf "FAILED: R.Dfa.match_string %t ~ \"%s\" = %b\n" R.(pp r) s e;
        incr failures
      end
    in

    begin
      let r = R.(concat a (repeat b)) in
      let dfa = R.Dfa.make r in

      [expect_match r; expect_dfa_match r dfa] |> List.iter begin fun expect_match ->
        expect_match "abb" true;
        expect_match "aba" false;
      end;
    end;

    begin
      let r = R.(inter b b) in
      let dfa = R.Dfa.make r in

      [expect_match r; expect_dfa_match r dfa] |> List.iter begin fun expect_match ->
        expect_match "b" true;
        expect_match "bb" false;
        expect_match "a" false;
      end;
    end;

    begin
      let r = R.(inter (repeat b) b) in
      let dfa = R.Dfa.make r in

      [expect_match r; expect_dfa_match r dfa] |> List.iter begin fun expect_match ->
        expect_match "b" true;
        expect_match "bb" false;
        expect_match "bbb" false;
        expect_match "a" false;
        expect_match "ba" false;
      end;
    end;

    begin
      let r = R.(inter (union a (repeat b)) (union b c)) in
      let dfa = R.Dfa.make r in

      [expect_match r; expect_dfa_match r dfa] |> List.iter begin fun expect_match ->
        expect_match "b" true;
        expect_match "bb" false;
        expect_match "a" false;
        expect_match "c" false;
      end;
    end;

    begin
      let r = R.(inter (repeat (string "bb")) (repeat b)) in
      let dfa = R.Dfa.make r in

      [expect_match r; expect_dfa_match r dfa] |> List.iter begin fun expect_match ->
        expect_match "bb" true;
        expect_match "bbb" false;
        expect_match "bbbb" true;
        expect_match "bbbbb" false;
        expect_match "aba" false;
      end;
    end;

    if !failures > 0 then
      Printf.ksprintf failwith "failed %i test%s"
        !failures
        (if !failures = 1 then "" else "s");

  end

end
