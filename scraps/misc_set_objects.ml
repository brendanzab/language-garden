(** Comparing set ADTs vs. set objects in OCaml.

    This is based on the examples from {{:https://www.cs.utexas.edu/~wcook/Drafts/2009/essay.pdf}
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

    (** The set interface, with the representation exposed as a type parameter *)
    type 'a set = {
      empty : 'a;
      insert : 'a -> int -> 'a;
      is_empty : 'a -> bool;
      contains : 'a -> int -> bool;
      union : 'a -> 'a -> 'a;
    }

    (** This set type uses OCaml’s GADTs to define an existential type that
        hides the underlying representation. Based on Figure 6 from the paper. *)
    type some_set =
      | Set : 'a. 'a set -> some_set

    (** This time we'll implement the set using a list internally. External
        consumers cannot see this implementation detail. *)
    let eq_set : some_set =
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

  end

  (** Isomorphisms between presentation styles *)

  (* Module <=> Existential *)

  let existential_of_module (module S : Module.S) : Existential.some_set =
    Set {
      empty = S.empty;
      insert = S.insert;
      is_empty = S.is_empty;
      contains = S.contains;
      union = S.union;
    }

  let module_of_existential : Existential.some_set -> (module Module.S) =
    fun (Set (type a) (set : a Existential.set)) ->
      (module struct
        type t = a
        let empty = set.empty
        let insert = set.insert
        let is_empty = set.is_empty
        let contains = set.contains
        let union = set.union
      end)

end

(** Object-oriented set implementations.

    See Section 3 of the paper.
*)
module Object_oriented = struct

  (** Sets as a characteristic function *)
  module Function = struct

    type t = int -> bool

    let empty : t = fun _ -> false
    let insert (s : t) (n : int) : t = fun i -> i = n || s i
    let union (s1 : t) (s2 : t) : t = fun i -> s1 i || s2 i

    (* NOTE: This interface effectively only supports the “contains” method.
       This that we cannot check if the set is empty. *)

  end

  (** OCaml’s object types are a good stand-in for the paper’s interfaces *)
  module Object = struct

    (* Based on Figure 7 from the paper *)
    type t = <
      is_empty : bool;
      contains : int -> bool;
      insert : int -> t;
      union : t -> t;
    >

    let rec insert (s : t) (n : int) : t =
      if s#contains n then s else
        object(self)
          method is_empty = false
          method contains i = (i = n) || s#contains i
          method insert i = insert self i
          method union s = union self s
        end

    and union (s1 : t) (s2 : t) : t = object(self)
      method is_empty = s1#is_empty && s2#is_empty
      method contains i = s1#contains i || s2#contains i
      method insert i = insert self i
      method union s = union self s
    end

    let empty : t = object(self)
      method is_empty = true
      method contains _ = false
      method insert i = insert self i
      method union s = s (* NOTE: optimisation for empty sets *)
    end

    (* FIXME: [empty] needs to be defined afterwards, unlike in the [Records]
       example below. This could be a bug in OCaml's vicious circle analysis. *)

  end

  (** We can actually just use recursive record to define objects in OCaml,
      but it’s a little more ugly due to the precedence of functions. *)
  module Record = struct

    type t = {
      is_empty : bool;
      contains : int -> bool;
      insert : int -> t;
      union : t -> t;
    }

    let rec empty : t =
      let rec self = {
        is_empty = true;
        contains = (fun _ -> false);
        insert = (fun i -> insert self i);
        union = (fun s -> s); (* NOTE: optimisation for empty sets *)
      } in self

    and insert (s : t) (n : int) : t =
      if s.contains n then s else
        let rec self = {
          is_empty = false;
          contains = (fun i -> i = n || s.contains i);
          insert = (fun i -> insert self i);
          union = (fun s -> union self s);
        } in self

    and union (s1 : t) (s2 : t) : t =
      let rec self = {
        is_empty = s1.is_empty && s2.is_empty;
        contains = (fun i -> s1.contains i || s2.contains i);
        insert = (fun i -> insert self i);
        union = (fun s -> union self s);
      } in self

  end

  (** Isomorphisms between presentation styles *)

  (* Objects <=> Record *)

  let rec record_of_object (self : Object.t) : Record.t = {
    is_empty = self#is_empty;
    contains = (fun i -> self#contains i);
    insert = (fun i -> record_of_object (self#insert i));
    union = (fun s -> record_of_object (self#union (object_of_record s)));
  }

  and object_of_record (self : Record.t) : Object.t =
    object
      method is_empty = self.is_empty
      method contains i = self.contains i
      method insert i = object_of_record (self.insert i)
      method union s = object_of_record (self.union (record_of_object s))
    end

end

(** Additional object oriented set implementations that use the interfaces
    above, including:

    - [even]: the set of all even integers
    - [full]: the set of all integers
    - [interval]: the set of all integers within some inclusive bounds

    This shows how additional set objects can be defined based on the same
    interface, beyond what was originally envisioned by the library author, all
    while retaining interoperability with existing objects.

    See section 3.4 of the paper.
*)
module Object_oriented_ext = struct

  module Function = struct

    open Object_oriented.Function

    let even : t = fun i -> i mod 2 = 0
    let full : t = fun _ -> true
    let interval (n : int) (m : int) : t = fun i -> n <= i && i <= m

  end

  module Object = struct

    open Object_oriented.Object

    let even : t = object(self)
      method is_empty = false
      method contains i = i mod 2 = 0
      method insert i = insert self i
      method union s = union self s
    end

    let full : t = object(self)
      method is_empty = false
      method contains _ = true
      method insert _ = self
      method union _ = self
    end

    let interval (n : int) (m : int) : t = object(self)
      method is_empty = n > m
      method contains i = n <= i && i <= m
      method insert i = insert self i
      method union s = union self s
    end

  end

  module Record = struct

    open Object_oriented.Record

    let even : t =
      let rec self = {
        is_empty = false;
        contains = (fun i -> i mod 2 = 0);
        insert = (fun i -> insert self i);
        union = (fun s -> union self s);
      } in self

    let full : t =
      let rec self = {
        is_empty = false;
        contains = (fun _ -> true);
        insert = (fun _ -> self);
        union = (fun _ -> self);
      } in self

    let interval (n : int) (m : int) : t =
      let rec self = {
        is_empty = n > m;
        contains = (fun i -> n <= i && i <= m);
        insert = (fun i -> insert self i);
        union = (fun s -> union self s);
      } in self

  end

end

(* Section 3.9: *)

type 'a f = {
  is_empty : bool;
  contains : int -> bool;
  insert : int -> 'a;
  union : 'a -> 'a;
}

(* isomorphic to Object_oriented.Records.set *)
type set_obj =
  | Set_obj of set_obj f

let rec into (Set_obj r : set_obj) : Object_oriented.Record.t = {
  is_empty = r.is_empty;
  contains = (fun i -> r.contains i);
  insert = (fun i -> into (r.insert i));
  union = (fun s -> into (r.union (from s)));
}

and from (r : Object_oriented.Record.t) : set_obj = Set_obj {
  is_empty = r.is_empty;
  contains = (fun i -> r.contains i);
  insert = (fun i -> from (r.insert i));
  union = (fun s -> from (r.union (into s)));
}

(* isomorphic to Abstract_data_types.Existential.set *)
type set_adt =
  | Set_adt : 'rep. 'rep * ('rep -> 'rep f) -> set_adt

(* TODO: ... *)


(* Objects as ADTs. See end of section 4.2 *)

module _ : Abstract_data_types.Module.S = struct

  type t = Object_oriented.Object.t

  let is_empty s = s#is_empty
  let contains s i = s#contains i
  let empty = Object_oriented.Object.empty
  let insert = Object_oriented.Object.insert
  let union = Object_oriented.Object.union

end

module _ : Abstract_data_types.Module.S = struct

  type t = Object_oriented.Record.t

  let is_empty s = Object_oriented.Record.(s.is_empty)
  let contains s i = Object_oriented.Record.(s.contains) i
  let empty = Object_oriented.Record.empty
  let insert = Object_oriented.Record.insert
  let union = Object_oriented.Record.union

end
