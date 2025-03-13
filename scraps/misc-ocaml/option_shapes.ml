(** Inspired by https://okmij.org/ftp/tagless-final/datatypes.html *)

[@@@warning "-unused-value-declaration"]

module Option = struct

  type 'a shape = [
    | `Some of 'a
    | `None
  ]

  module type S = sig

    type 'a t

    val intro : 'a shape -> 'a t
    val elim : 'a t -> ('a shape -> 'b) -> 'b

  end

  module Impl1 : S = struct

    type 'a t = 'a shape

    let intro opt = opt
    let elim x k = k x

  end

  module Impl2 : S = struct

    type 'a t =
      | Some of 'a
      | None

    let intro = function
      | `Some x -> Some x
      | `None -> None

    let elim = function
      | Some x -> fun k -> k (`Some x)
      | None -> fun k -> k `None

  end

  (* TODO: Impl that uses Stdlib.Obj *)

end
