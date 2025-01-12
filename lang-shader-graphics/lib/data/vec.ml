(** Fixed-size vectors

    Vectors indexed with a statically known size. This is not a very efficient
    representation, but we don’t actually use these for computation so it
    doesn’t really matter.
*)

open Nat

type (_, _) t =
  | [] : ('s, zero) t
  | ( :: ) : 's * ('s, 'n) t -> ('s, 'n succ) t

(** {2 Vectors of a given size} *)

type 's t1 = ('s, n1) t
type 's t2 = ('s, n2) t
type 's t3 = ('s, n3) t
type 's t4 = ('s, n4) t

(** {2 Vector component accessors} *)

(** Named component of a vector *)
type _ component =
  | X : ('n ge1) component
  | Y : ('n ge2) component
  | Z : ('n ge3) component
  | W : ('n ge4) component

(* TODO: Use numeric indexing instead *)

let get (type a n) (c : n component) (v : (a, n) t) : a =
  match c, v with
  | X, (s :: _) -> s
  | Y, (_ :: s :: _) -> s
  | Z, (_ :: _ :: s :: _) -> s
  | W, (_ :: _ :: _ :: s :: _) -> s

let rec swizzle : type a n m. (n component, m) t -> (a, n) t -> (a, m) t =
  fun cv v ->
    match cv with
    | [] -> []
    | c :: cv -> get c v :: swizzle cv v

let get2 (type a n) (c1, c2 : n component * n component) (v : (a, n) t) : a t2 =
  swizzle [c1; c2] v

let get3 (type a n) (c1, c2, c3 : n component * n component * n component) (v : (a, n) t) : a t3 =
  swizzle [c1; c2; c3] v

let get4 (type a n) (c1, c2, c3, c4 : n component * n component * n component * n component) (v : (a, n) t) : a t4 =
  swizzle [c1; c2; c3; c4] v

let set (type a n) (c : n component) (s : a) (v : (a, n) t) : (a, n) t =
  match c, v with
  | X, (_ :: v) -> s :: v
  | Y, (sx :: _ :: v) -> sx :: s :: v
  | Z, (sx :: sy :: _ :: v) -> sx :: sy :: s :: v
  | W, (sx :: sy :: sz :: _ :: v) -> sx :: sy :: sz :: s :: v

(** {2 Vector combinators} *)

let rec map : type a b n. (a -> b) -> (a, n) t -> (b, n) t =
  fun f -> function
    | [] -> []
    | s :: v -> f s :: map f v

let rec fold_left : type a b n. (a -> b -> a) -> a -> (b, n) t -> a =
  fun f acc -> function
    | [] -> acc
    | s :: v -> fold_left f (f acc s) v

let rec zip_with : type a b c n. (a -> b -> c) -> (a, n) t -> (b, n) t -> (c, n) t =
  fun f v1 v2 ->
    match v1, v2 with
    | [], [] -> []
    | (s1 :: v1), (s2 :: v2) ->
        f s1 s2 :: zip_with f v1 v2

let rec zip_with3 : type a b c d n. (a -> b -> c -> d) -> (a, n) t -> (b, n) t -> (c, n) t -> (d, n) t =
  fun f v1 v2 v3 ->
    match v1, v2, v3 with
    | [], [], [] -> []
    | (s1 :: v1), (s2 :: v2), (s3 :: v3) ->
        f s1 s2 s3 :: zip_with3 f v1 v2 v3

module Notation = struct

  let ( .%{ } ) v c = get c v
  let ( .%{ }<- ) v c s = set c s v

end
