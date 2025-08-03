(** An implementation of miniAdapton in OCaml.

    - {{: https://arxiv.org/abs/1609.05337} miniAdapton: A Minimal Implementation of Incremental Computation in Scheme}
    - https://github.com/fisherdj/miniAdapton
    - https://github.com/LightAndLight/mini-adapton
*)

module Adapton : sig

  module Thunk : sig

    type 'a t

    val create : (unit -> 'a) -> 'a t
    val force : 'a t -> 'a

  end

  module Ref : sig

    type 'a t

    val create : 'a -> 'a t
    val set : 'a t -> 'a -> unit
    val get : 'a t -> 'a

  end

  val memo_lazy : ('a -> 'b) -> 'a -> 'b Thunk.t [@@warning "-unused-value-declaration"]
  val memo : ('a -> 'b) -> 'a -> 'b [@@warning "-unused-value-declaration"]

  module Var : sig

      type 'a t

      val create : (unit -> 'a) -> 'a t
      val set : 'a t -> (unit -> 'a) -> unit
      val get : 'a t -> 'a

  end

end = struct

  module rec Base_thunk : sig

    type 'a t = {
      id : int;
      compute : unit -> 'a;
      mutable result : 'a option;
      mutable sub : Thunk_set.t;
      mutable super : Thunk_set.t;
      mutable is_clean : bool;
    }

    type some_t =
      | Thunk : 'a t -> some_t

  end = Base_thunk

  and Thunk_set : Set.S
    with type elt = Base_thunk.some_t
  = Set.Make (struct

    type t = Base_thunk.some_t

    let compare (Thunk t1 : t) (Thunk t2 : t) : int =
      Int.compare t1.id t2.id

  end)

  module Thunk = struct

    include Base_thunk

    let fresh_id : unit -> int =
      let next_id = ref 0 in
      fun () ->
        let id = !next_id in
        incr next_id;
        id

    let create (type a) (f : unit -> a) : a t = {
      id = fresh_id ();
      compute = f;
      result = None;
      sub = Thunk_set.empty;
      super = Thunk_set.empty;
      is_clean = false;
    }

    let add_dcg_edge (type a b) (t_super : a t) (t_sub : b t) =
      t_super.sub <- Thunk_set.add (Thunk t_sub) t_super.sub;
      t_sub.super <- Thunk_set.add (Thunk t_super) t_sub.super

    let remove_dcg_edge (type a b) (t_super : a t) (t_sub : b t) =
      t_super.sub <- Thunk_set.remove (Thunk t_sub) t_super.sub;
      t_sub.super <- Thunk_set.remove (Thunk t_super) t_sub.super

    let rec compute : type a. a t -> a =
      fun t ->
        if t.is_clean then
          Option.get t.result
        else begin
          t.sub |> Thunk_set.iter (fun (Thunk t_sub) -> remove_dcg_edge t t_sub);
          t.is_clean <- true;
          t.result <- Some (t.compute ());
          compute t
        end

    let rec dirty : type a. a t -> unit =
      fun t ->
        if t.is_clean then
          t.is_clean <- false;
          t.super |> Thunk_set.iter (fun (Thunk t_super) -> dirty t_super)

    let currently_adapting : some_t option ref =
      ref None

    let force (type a) (t : a t) : a =
      let prev_adapting = !currently_adapting in
      currently_adapting := Some (Thunk t);
      let result = compute t in
      currently_adapting := prev_adapting;
      !currently_adapting |> Option.iter (fun (Thunk t') -> add_dcg_edge t' t);
      result

  end

  module Ref = struct

    type 'a t = 'a Thunk.t

    let create (type a) (x : a) : a t =
      let rec t = Thunk.{
        id = Thunk.fresh_id ();
        compute = (fun () -> Option.get t.result);
        result = Some x;
        sub = Thunk_set.empty;
        super = Thunk_set.empty;
        is_clean = false;
      } in
      t

    let set (type a) (t : a t) (x : a) : unit =
      t.result <- Some x;
      Thunk.dirty t

    let get (type a) (t : a t) : a =
      Thunk.force t

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

  let open Adapton in

  let r = Ref.create 5 in
  let a = Thunk.create (fun () -> Ref.get r + 3) in

  assert (Thunk.force a = 8);
  Ref.set r 2;
  assert (Thunk.force a = 5);

end

(* Spreadsheet example *)
let () = begin

  let open Adapton in

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
