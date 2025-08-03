(** An implementation of miniAdapton in OCaml.

    - {{: https://arxiv.org/abs/1609.05337} miniAdapton: A Minimal Implementation of Incremental Computation in Scheme}
    - https://github.com/fisherdj/miniAdapton
    - https://github.com/LightAndLight/mini-adapton
*)

module rec Thunk : sig

  type 'a t

  val create : (unit -> 'a) -> 'a t
  val id : 'a t -> int

  val add_dcg_edge : 'a t -> 'b t -> unit [@@warning "-unused-value-declaration"]
  val remove_dcg_edge : 'a t -> 'b t -> unit [@@warning "-unused-value-declaration"]

  val compute : 'a t -> 'a [@@warning "-unused-value-declaration"]
  val dirty : 'a t -> unit [@@warning "-unused-value-declaration"]
  val force : 'a t -> 'a

  module Ref : sig

    type 'a t

    val create : 'a -> 'a t
    val set : 'a t -> 'a -> unit
    val get : 'a t -> 'a

  end

end = struct

  module Some_thunk = struct

    type t =
      | Some_thunk : 'a Thunk.t -> t

    let compare (Some_thunk t1) (Some_thunk t2) : int =
      Int.compare (Thunk.id t1) (Thunk.id t2)

  end

  module S = Set.Make (Some_thunk)

  type 'a t = {
    id : int;
    compute : unit -> 'a;
    mutable result : 'a option;
    mutable sub : S.t;
    mutable super : S.t;
    mutable is_clean : bool;
  }

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
    sub = S.empty;
    super = S.empty;
    is_clean = false;
  }

  let id (type a) (t : a t) : int = t.id

  let add_dcg_edge (type a b) (t_super : a t) (t_sub : b t) =
    t_super.sub <- S.add (Some_thunk t_sub) t_super.sub;
    t_sub.super <- S.add (Some_thunk t_super) t_sub.super

  let remove_dcg_edge (type a b) (t_super : a t) (t_sub : b t) =
    t_super.sub <- S.remove (Some_thunk t_sub) t_super.sub;
    t_sub.super <- S.remove (Some_thunk t_super) t_sub.super

  let rec compute : type a. a t -> a =
    fun t ->
      if t.is_clean then
        t.result |> Option.get
      else begin
        t.sub |> S.iter (fun (Some_thunk t_sub) -> remove_dcg_edge t t_sub);
        t.is_clean <- true;
        t.result <- Some (t.compute ());
        compute t
      end

  let rec dirty : type a. a t -> unit =
    fun t ->
      if t.is_clean then
        t.is_clean <- false;
        t.super |> S.iter (fun (Some_thunk t_super) -> dirty t_super)

  let currently_adapting : Some_thunk.t option ref =
    ref None

  let force (type a) (t : a t) : a =
    let prev_adapting = !currently_adapting in
    currently_adapting := Some (Some_thunk t);
    let result = compute t in
    currently_adapting := prev_adapting;
    !currently_adapting |> Option.iter (fun (Some_thunk.Some_thunk t') -> add_dcg_edge t' t);
    result

  module Ref = struct

    type 'a t = {
      thunk : 'a Thunk.t;
    }

    let create (type a) (x : a) : a t =
      let rec t = {
        id = fresh_id ();
        compute = (fun () -> t.result |> Option.get);
        result = Some x;
        sub = S.empty;
        super = S.empty;
        is_clean = false;
      } in
      { thunk = t }

    let set (type a) (t : a t) (x : a) : unit =
      t.thunk.result <- Some x;
      dirty t.thunk

    let get (type a) (t : a t) : a =
      force t.thunk

  end

end

module Var : sig

    type 'a t

    val create : (unit -> 'a) -> 'a t
    val set : 'a t -> (unit -> 'a) -> unit
    val get : 'a t -> 'a

end = struct

  type 'a t = {
    ref : 'a Thunk.t Thunk.Ref.t;
  }

  let create (type a) (f : unit -> a) : a t = {
    ref = Thunk.Ref.create (Thunk.create f);
  }

  let set (type a) (t : a t) (f : unit -> a) : unit =
    Thunk.Ref.set t.ref (Thunk.create f)

  let get (type a) (t : a t) : a =
    Thunk.force (Thunk.Ref.get t.ref)

end


(* Tests *)

let () = begin

  let r = Thunk.Ref.create 5 in
  let a = Thunk.create (fun () -> Thunk.Ref.get r + 3) in

  assert (Thunk.force a = 8);
  Thunk.Ref.set r 2;
  assert (Thunk.force a = 5);

end

(* Spreadsheet example *)
let () = begin

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
