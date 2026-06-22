(** Generative functors for producing new namespaces *)

module type S = sig

  type t

  val make : string -> t
  val compare : t -> t -> int
  val to_string : t -> string
  val pp : t -> Format.formatter -> unit

end

(** Create a new namespace of names. *)
module Make () : S = struct

  type t = string

  let make name = name
  let compare = String.compare
  let to_string name = name
  let pp name ppf = Format.pp_print_string ppf name

end

(** A supply of fresh names.

    The following pattern can be used to create a local function that
    generates a fresh name whenever it is called:

    {[
      let module Supply (Id) in
      let fresh_id = Label_supply.(fresh (create ())) in
    ]}
*)
module Supply (A : S) : sig

  type t

  val create : ?names:string list -> unit -> t
  val fresh : t -> string -> A.t

end = struct

  type t = {
    counts : (string, int) Hashtbl.t;
  }

  let create ?(names = []) () : t = {
    counts =
      List.to_seq names
      |> Seq.map (fun n -> n, 1)
      |> Hashtbl.of_seq;
  }

  let make_name (base_name : string) (count : int) =
    if count = 0 then base_name else
      Printf.sprintf "%s_%i" base_name count

  let rec fresh (state : t) (base_name : string) : A.t =
    (* Search for a name that’s not currently in use *)
    let rec go count =
      let name = make_name base_name count in
      if Hashtbl.mem state.counts name then
        go (count + 1)
      else begin
        Hashtbl.add state.counts name 1;
        Hashtbl.replace state.counts base_name count;
        A.make name
      end
    in
    go (Hashtbl.find_opt state.counts base_name |> Option.value ~default:0)

end

module Debruijn = struct

  module type S = sig

    module Index : sig

      type t

    end

    module Env : sig

      type 'a t

      val empty : 'a t
      val extend : 'a -> 'a t -> 'a t
      val lookup : Index.t -> 'a t -> 'a
      val find_index : ('a -> bool) -> 'a t -> Index.t option
      val of_seq : 'a Seq.t -> 'a t

    end

  end

  module Make () : S = struct

    module Index = struct

      type t = int

    end

    module Env = struct

      type 'a t = 'a list

      let empty = []
      let extend x env = x :: env
      let lookup i env = List.nth env i
      let find_index = List.find_index
      let of_seq = List.of_seq

    end

  end

end
