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

    (* Based on Figure 3 from the paper *)
    module type S = sig

      type t

      val empty : t
      val insert : t -> int -> t
      val is_empty : t -> bool
      val contains : t -> int -> bool
      val union : t -> t -> t

    end

    (** A set implementation using equality. It’s not very efficient.
        Based on Figure 2 from the paper. *)
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

    (** A more efficient set implementation that uses a total ordering of the
        elements. Based on Figure 4 from the paper. *)
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

    (** This set type uses OCaml’s GADTs to define an existential type that
        hides the underlying representation. Based on Figure 6 from the paper. *)
    type set =
      | Set : 'rep. {
        empty : 'rep;
        insert : 'rep -> int -> 'rep;
        is_empty : 'rep -> bool;
        contains : 'rep -> int -> bool;
        union : 'rep -> 'rep -> 'rep;
      } -> set

    (** This time we'll implement the set using a list internally. External
        consumers cannot see this implementation detail. *)
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

    (** Implement the sorted set in terms of the abstract datatype defined earlier. *)
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

  (* Sets as a characteristic function *)
  module Functions = struct

    type set = int -> bool

    let empty : set = fun _ -> false
    let insert (s : set) (n : int) : set = fun i -> i = n || s i
    let union (s1 : set) (s2 : set) : set = fun i -> s1 i || s2 i

  end

  (** OCaml’s object types are a good stand-in for the paper’s interfaces *)
  module Objects = struct

    (* Based on Figure 7 from the paper *)
    type set = <
      is_empty : bool;
      contains : int -> bool;
      insert : int -> set;
      union : set -> set;
    >

    (* Core implementations *)

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

    (* Additional implementations *)

    let even : set = object(self)
      method is_empty = false
      method contains i = i mod 2 = 0
      method insert i = insert self i
      method union s = union self s
    end

    let full : set = object(self)
      method is_empty = false
      method contains _ = true
      method insert _ = self
      method union _ = self
    end

    let interval (n : int) (m : int) : set = object(self)
      method is_empty = n > m
      method contains i = n <= i && i <= m
      method insert i = insert self i
      method union s = union self s
    end

  end

  (** We can actually just use recursive record to define objects in OCaml,
      but it’s a little more awkward. *)
  module Records = struct

    type set = {
      is_empty : bool;
      contains : int -> bool;
      insert : int -> set;
      union : set -> set;
    }

    (* Core implementations *)

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

    (* Additional implementations *)

    let even : set =
      let rec self = {
        is_empty = false;
        contains = (fun i -> i mod 2 = 0);
        insert = (fun i -> insert self i);
        union = (fun s -> union self s);
      } in self

    let full : set =
      let rec self = {
        is_empty = false;
        contains = (fun _ -> true);
        insert = (fun _ -> self);
        union = (fun _ -> self);
      } in self

    let interval (n : int) (m : int) : set =
      let rec self = {
        is_empty = n > m;
        contains = (fun i -> n <= i && i <= m);
        insert = (fun i -> insert self i);
        union = (fun s -> union self s);
      } in self

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

    end = Set

    (* Core implementations *)

    let rec empty () : (module Set.S) =
      let rec self : (module Set.S) =
        (module struct
          let is_empty = true
          let contains _ = false
          let insert i = insert self i
          let union s = s
        end)
      in self

    and insert (module S : Set.S) (n : int) : (module Set.S) =
      if S.contains n then (module S) else
        let rec self : (module Set.S) =
          (module struct
            let is_empty = false
            let contains i = (i = n) || S.contains i
            let insert i = insert self i
            let union s = union self s
          end)
        in self

    and union (module S1 : Set.S) (module S2 : Set.S) : (module Set.S) =
      let rec self : (module Set.S) =
        (module struct
          let is_empty = S1.is_empty && S2.is_empty
          let contains i = S1.contains i || S2.contains i
          let insert i = insert self i
          let union s = union self s
        end)
      in self

    (* Additional implementations *)

    let even : (module Set.S) =
      let rec self : (module Set.S) =
        (module struct
          let is_empty = false
          let contains i = i mod 2 = 0
          let insert i = insert self i
          let union s = union self s
        end)
      in self

    let full : (module Set.S) =
      let rec self : (module Set.S) =
        (module struct
          let is_empty = false
          let contains _ = true
          let insert _ = self
          let union _ = self
        end)
      in self

    let interval (n : int) (m : int) : (module Set.S) =
      let rec self : (module Set.S) =
        (module struct
          let is_empty = n > m
          let contains i = n <= i && i <= m
          let insert i = insert self i
          let union s = union self s
        end)
      in self

  end

end

(*

  Section 3.9:

  type 'a f = {
    is_empty : bool;
    contains : int -> bool;
    insert : int -> 'a;
    union : 'a -> 'a;
  }

  (* set_obj is isomorphic to Object_oriented.Records.set *)
  (* set_adt is isomorphic to Abstract_data_types.Existential.set *)

  type set_obj = Set_obj f
  type set_adt = Set_adt : 'rep. 'rep * ('rep -> 'rep f) -> set_adt

*)
