(** An implementation of miniAdapton in OCaml.

    MiniAdapton is a library for demand-driven incremental computation that
    trades space and startup time for faster incremental updates. Computation
    graphs can be changed after-the-fact, while still maintaining incremental
    performance.

    Resources:

    - {{: https://arxiv.org/abs/1609.05337} miniAdapton: A Minimal Implementation of Incremental Computation in Scheme}
      - {{: https://github.com/fisherdj/miniAdapton} Scheme implementation}
      - {{: https://github.com/LightAndLight/mini-adapton} Haskell implementation}
    - {{: https://web.archive.org/web/20250507163147/https://adapton.org/} Adapton Homepage}
    - {{: https://https://vimeo.com/122066659/} Incremental Computation with Adapton} by Matthew A Hammer
*)

(** Low-level interface used in the implementation of {!Miniadapton}. The
    Dynamic Computation Graph (DCG) must be managed manually when using this
    interface. *)
module Microadapton : sig

  (** Delayed computations *)

  type 'a thunk

  val create : (unit -> 'a) -> 'a thunk
  val add_dcg_edge : super:'a thunk -> sub:'b thunk -> unit
  val remove_dcg_edge : super:'a thunk -> sub:'b thunk -> unit [@@warning "-unused-value-declaration"]
  val compute : 'a thunk -> 'a
  val dirty : 'a thunk -> unit [@@warning "-unused-value-declaration"]

  (** Mutable references *)

  type 'a thunk_ref = private 'a thunk

  val create_ref : 'a -> 'a thunk_ref
  val set_ref : 'a thunk_ref -> 'a -> unit

end = struct

  (** Core type definitions *)

  module rec Thunk : sig

    type 'a t = {
      id : int;
      compute : unit -> 'a;
      mutable result : 'a option;
      mutable sub : Thunk_set.t;
      mutable super : Thunk_set.t;
      mutable is_clean : bool;
    }

    type some_t =
      | Thunk : 'a Thunk.t -> some_t

  end = Thunk

  and Thunk_set : Set.S with type elt = Thunk.some_t =
    Set.Make (struct
      type t = Thunk.some_t
      let compare (Thunk t1 : t) (Thunk t2 : t) =
        Int.compare t1.id t2.id
    end)


  (** Delayed computations *)

  type 'a thunk = 'a Thunk.t

  let fresh_id : unit -> int =
    let next_id = ref 0 in
    fun () ->
      let id = !next_id in
      incr next_id;
      id

  let create (type a) (f : unit -> a) : a thunk =
    Thunk.{
      id = fresh_id ();
      compute = f;
      result = None;
      sub = Thunk_set.empty;
      super = Thunk_set.empty;
      is_clean = false;
    }

  let add_dcg_edge (type a b) ~super:(t_super : a thunk) ~sub:(t_sub : b thunk) =
    t_super.sub <- Thunk_set.add (Thunk t_sub) t_super.sub;
    t_sub.super <- Thunk_set.add (Thunk t_super) t_sub.super

  let remove_dcg_edge (type a b) ~super:(t_super : a thunk) ~sub:(t_sub : b thunk) =
    t_super.sub <- Thunk_set.remove (Thunk t_sub) t_super.sub;
    t_sub.super <- Thunk_set.remove (Thunk t_super) t_sub.super

  (** Return the result of a thunked computation quickly if it is in a clean
      state, or restart the computation, removing links to any subcomputations
      that may have been previously recorded. *)
  let rec compute : type a. a thunk -> a =
    fun t ->
      if t.is_clean then
        Option.get t.result
      else begin
        t.sub |> Thunk_set.iter (fun (Thunk t_sub) ->
          remove_dcg_edge ~super:t ~sub:t_sub);
        t.is_clean <- true;
        t.result <- Some (t.compute ());
        compute t
      end

  (** Mark a thunk as dirty along with all of its supercomputations *)
  let rec dirty : type a. a thunk -> unit =
    fun t ->
      if t.is_clean then
        t.is_clean <- false;
        t.super |> Thunk_set.iter (fun (Thunk t_super) -> dirty t_super)


  (** Mutable references *)

  type 'a thunk_ref = 'a thunk

  let create_ref (type a) (x : a) : a thunk =
    let rec t = Thunk.{
      id = fresh_id ();
      compute = (fun () -> Option.get t.result);
      result = Some x;
      sub = Thunk_set.empty;
      super = Thunk_set.empty;
      is_clean = false;
    } in
    t

  let set_ref (type a) (t : a thunk) (x : a) : unit =
    t.result <- Some x;
    dirty t

end


(** High-level interface for safely combining mutation and memoization *)
module Miniadapton : sig

  (** Demand-driven, incremental computations *)
  module Thunk : sig

    type 'a t = private 'a Microadapton.thunk

    val create : (unit -> 'a) -> 'a t
    val force : 'a t -> 'a

  end

  (** Mutable references *)
  module Ref : sig

    type 'a t = private 'a Microadapton.thunk_ref

    val create : 'a -> 'a t
    val set : 'a t -> 'a -> unit
    val get : 'a t -> 'a

  end

  (** Memoize a function, returning a thunk that can be forced later *)
  val memo_lazy : ('a -> 'b) -> 'a -> 'b Thunk.t [@@warning "-unused-value-declaration"]

  (** Convenience function that uses {!memo_lazy} and returns the forced the result *)
  val memo : ('a -> 'b) -> 'a -> 'b [@@warning "-unused-value-declaration"]

  (** Delayed computations that can be altered incrementally after the fact *)
  module Var : sig

      type 'a t

      val create : (unit -> 'a) -> 'a t
      val set : 'a t -> (unit -> 'a) -> unit
      val get : 'a t -> 'a

  end

end = struct

  module Thunk = struct

    type 'a t = 'a Microadapton.thunk

    let create = Microadapton.create

    type some_t =
      | Thunk : 'a t -> some_t

    let currently_adapting : some_t option ref =
      ref None

    let force (type a) (t : a t) : a =
      let prev_adapting = !currently_adapting in
      currently_adapting := Some (Thunk t);
      let result = Microadapton.compute t in
      currently_adapting := prev_adapting;
      !currently_adapting |> Option.iter (fun (Thunk t') ->
        Microadapton.add_dcg_edge ~super:t' ~sub:t);
      result

  end

  module Ref = struct

    type 'a t = 'a Microadapton.thunk_ref

    let create = Microadapton.create_ref
    let set = Microadapton.set_ref

    let get (type a) (t : a t) : a =
      Thunk.force (t :> a Microadapton.thunk)

  end

  let memo_lazy (type a b) (f : a -> b) : a -> b Thunk.t =
    let kv = Hashtbl.create 16 in
    fun x ->
      match Hashtbl.find_opt kv x with
      | Some result -> result
      | None ->
          let result = Thunk.create (fun () -> f x) in
          Hashtbl.add kv x result;
          result

  let memo (type a b) (f : a -> b) : a -> b =
    let f' = memo_lazy f in
    fun x -> Thunk.force (f' x)

  module Var = struct

    type 'a t = 'a Thunk.t Ref.t

    let create (type a) (f : unit -> a) : a t =
      Ref.create (Thunk.create f)

    let set (type a) (t : a t) (f : unit -> a) : unit =
      Ref.set t (Thunk.create f)

    let get (type a) (t : a t) : a =
      Thunk.force (Ref.get t)

  end

end


(* Tests *)

let () = begin

  let open Miniadapton in

  let r = Ref.create 5 in
  let a = Thunk.create (fun () -> Ref.get r + 3) in

  assert (Thunk.force a = 8);
  Ref.set r 2;
  assert (Thunk.force a = 5);

end

(* Spreadsheet example *)
let () = begin

  let open Miniadapton in

  let n1 = Var.create (fun () -> 1) in
  let n2 = Var.create (fun () -> 2) in
  let n3 = Var.create (fun () -> 3) in
  let p1 = Var.create (fun () -> Var.get n1 + Var.get n2) in
  let p2 = Var.create (fun () -> Var.get p1 + Var.get n3) in

  assert (Var.get p1 = 3);
  assert (Var.get p2 = 6);

  (* Update variable *)
  Var.set n1 (fun () -> 5);
  assert (Var.get p1 = 7);

  (* Swap arguments *)
  Var.set p2 (fun () -> Var.get n3 + Var.get p1);
  assert (Var.get p2 = 10);

  (* Update variable *)
  Var.set p1 (fun () -> 4);
  assert (Var.get p2 = 7);

  (* Change our mind and revert back *)
  Var.set p1 (fun () -> Var.get n1 + Var.get n2);
  assert (Var.get p2 = 10);

end
