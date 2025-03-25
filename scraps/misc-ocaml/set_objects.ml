(** Set examples in OCaml from {{:https://www.cs.utexas.edu/~wcook/Drafts/2009/essay.pdf}
    “On Understanding Data Abstraction, Revisited”} by William R. Cook
*)

[@@@warning "-unused-value-declaration"]
[@@@warning "-unused-field"]

(** ADT-style set implementations.

    See Section 2 of the paper.
*)
module Abstract_data_types = struct

  (** A set encoded using a module. This is the most common and idiomatic
      approach used for defining APIs in OCaml. *)
  module Module = struct

    module type S = sig

      type t

      val empty : t
      val insert : t -> int -> t
      val is_empty : t -> bool
      val contains : t -> int -> bool
      val union : t -> t -> t

    end

    module Eq_set : S = struct

      type t =
        | Empty
        | Insert of int * t

      let empty = Empty

      let is_empty s =
        s = Empty

      let rec contains s i =
        match s with
        | Empty -> false
        | Insert (n, _) when i = n -> true
        | Insert (_, r) -> contains r i

      let insert s i =
        if contains s i then s else
          Insert (i, s)

      let rec union s1 s2 =
        match s1 with
        | Empty -> s2
        | Insert (n1, r1) -> insert (union r1 s2) n1

    end

    module Sorted_set : S = struct

      type t =
        | Empty
        | Insert of int * t

      let empty = Empty

      let is_empty s =
        s = Empty

      let rec contains s i =
        match s with
        | Empty -> false
        | Insert (n, _) when i = n -> true
        | Insert (n, _) when i > n -> false
        | Insert (_, r) -> contains r i

      let rec insert s i =
        match s with
        | Empty -> Insert (i, s)
        | Insert (n, _) when i = n -> s
        | Insert (n, _) when i < n -> Insert (i, s)
        | Insert (n, r) ->
            let t = insert r i in
            if r = t then s else Insert (n, t)

      let rec union s1 s2 =
        match s1 with
        | Empty -> s2
        | Insert (n1, r1) ->
            match s2 with
            | Empty -> s1
            | Insert (n2, r2) when n1 = n2 -> insert (union r1 r2) n1
            | Insert (n2, _) when n1 < n2 -> insert (union r1 s2) n1
            | Insert (n2, r2) -> insert (union s1 r2) n2

    end

  end

  (** A set that hides its implementation using an existential type. *)
  module Existential = struct

    type set =
      | Set : 'rep. {
        empty : 'rep;
        insert : 'rep -> int -> 'rep;
        is_empty : 'rep -> bool;
        contains : 'rep -> int -> bool;
        union : 'rep -> 'rep -> 'rep;
      } -> set

    let eq_set : set =
      let empty = [] in
      let is_empty s = s = [] in

      let rec contains s i =
        match s with
        | [] -> false
        | n :: _ when i = n -> true
        | _ :: r -> contains r i
      in

      let insert s i =
        if contains s i then s else i :: s
      in

      let rec union s1 s2 =
        match s1 with
        | [] -> s2
        | n1 :: r1 -> insert (union r1 s2) n1
      in

      Set { empty; insert; is_empty; contains; union }

    let sorted_set : set = Set {
      empty = Module.Sorted_set.empty;
      insert = Module.Sorted_set.insert;
      is_empty = Module.Sorted_set.is_empty;
      contains = Module.Sorted_set.contains;
      union = Module.Sorted_set.union;
    }

  end

end

(** Object-oriented set implementations.

    See Section 3 of the paper.
*)
module Object_oriented = struct

  module Functions = struct

    type set = int -> bool

    let empty : set = fun _ -> false
    let insert (s : set) (n : int) : set = fun i -> i = n || s i
    let union (s1 : set) (s2 : set) : set = fun i -> s1 i || s2 i

  end

  (** Recursive record types *)
  module Records = struct

    type set = {
      is_empty : bool;
      contains : int -> bool;
      insert : int -> set;
      union : set -> set;
    }

    let rec empty () : set =
      let rec self = {
        is_empty = true;
        contains = (fun _ -> false);
        insert = (fun i -> insert self i);
        union = (fun s -> s);
      } in self

    and insert (s : set) (n : int) : set =
      if s.contains n then s else
        let rec self = {
          is_empty = false;
          contains = (fun i -> i = n || s.contains i);
          insert = (fun i -> insert self i);
          union = (fun s -> union self s);
        } in self

    and union (s1 : set) (s2 : set) : set =
      let rec self = {
        is_empty = s1.is_empty && s2.is_empty;
        contains = (fun i -> s1.contains i || s2.contains i);
        insert = (fun i -> insert self i);
        union = (fun s -> union self s);
      } in self

  end

  (** Object types and objects *)
  module Objects = struct

    type set = <
      is_empty : bool;
      contains : int -> bool;
      insert : int -> set;
      union : set -> set;
    >

    let rec empty () : set = object(self)
      method is_empty = true
      method contains _ = false
      method insert i = insert self i
      method union s = s
    end

    and insert (s : set) (n : int) : set =
      if s#contains n then s else
        object(self)
          method is_empty = false
          method contains i = (i = n) || s#contains i
          method insert i = insert self i
          method union s = union self s
        end

    and union (s1 : set) (s2 : set) : set = object(self)
      method is_empty = s1#is_empty && s2#is_empty
      method contains i = s1#contains i || s2#contains i
      method insert i = insert self i
      method union s = union self s
    end

  end

  (** I don't think anyone would usually use first-class modules like this, but
      I’m including it to demonstrate the correspondence between modules and
      object-oriented approaches to programming. *)
  module First_class_modules = struct

    module rec Set : sig

      module type S = sig
        val is_empty : bool
        val contains : int -> bool
        val insert : int -> (module Set.S)
        val union : (module Set.S) -> (module Set.S)
      end

      val empty : unit -> (module Set.S)
      val insert : (module Set.S) -> int -> (module Set.S)
      val union : (module Set.S) -> (module Set.S) -> (module Set.S)

    end = struct

      include Set

      let empty () : (module S) =
        let rec self : (module S) = (module struct
          let is_empty = true
          let contains _ = false
          let insert i = insert self i
          let union s = s
        end) in self

      let insert (module S : S) (n : int) : (module S) =
        if S.contains n then (module S) else
          let rec self : (module S) = (module struct
            let is_empty = false
            let contains i = (i = n) || S.contains i
            let insert i = insert self i
            let union s = union self s
          end) in self

      let union (module S1 : S) (module S2 : S) : (module S) =
        let rec self : (module S) = (module struct
          let is_empty = S1.is_empty && S2.is_empty
          let contains i = S1.contains i || S2.contains i
          let insert i = insert self i
          let union s = union self s
        end) in self

    end

  end

end
