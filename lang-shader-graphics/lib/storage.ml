(** {0 Common storage types used in GPU shader languages} *)


(** {1 Type level natural numbers} *)

(** A type that represents the number zero, i.e. [0] *)
type zero = private Z

(** A type that represents the successor of ['n], i.e. ['n + 1] *)
type 'n succ = private Succ of 'n

(** {2 Natural number constants} *)

type n0 = zero
type n1 = n0 succ
type n2 = n1 succ
type n3 = n2 succ
type n4 = n3 succ

(** {2 Greater-than or equal to constants} *)

(** These are useful for expressing the idea that a number must be at least a
    certian natural number *)

type 'n ge1 = 'n succ
type 'n ge2 = 'n succ ge1
type 'n ge3 = 'n succ ge2
type 'n ge4 = 'n succ ge3


(** {1 Fixed-size vectors} *)

(** Vectors indexed with a statically known size. This is not a very efficient
    representation, but we don’t actually use these for computation so it
    doesn’t really matter. *)
module Vec = struct

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

end

type ('s, 'n) vec = ('s, 'n) Vec.t

type 'n vecf = (float, 'n) vec

(** {2 Vectors of a given size} *)

type 's vec1 = ('s, n1) vec
type 's vec2 = ('s, n2) vec
type 's vec3 = ('s, n3) vec
type 's vec4 = ('s, n4) vec

type vec1f = n1 vecf
type vec2f = n2 vecf
type vec3f = n3 vecf
type vec4f = n4 vecf

(** {2 Vectors that are greater than or equal to a given size} *)

type ('s, 'n) vec_ge1 = ('s, 'n ge1) vec
type ('s, 'n) vec_ge2 = ('s, 'n ge2) vec
type ('s, 'n) vec_ge3 = ('s, 'n ge3) vec
type ('s, 'n) vec_ge4 = ('s, 'n ge4) vec

type 'n vec_ge1f = ('n ge1) vecf
type 'n vec_ge2f = ('n ge2) vecf
type 'n vec_ge3f = ('n ge3) vecf
type 'n vec_ge4f = ('n ge4) vecf


(** {1 Fixed-size matrices} *)

type 's mat2 = ('s vec2) vec2
type 's mat3 = ('s vec3) vec3
type 's mat4 = ('s vec4) vec4

type mat2f = float mat2
type mat3f = float mat3
type mat4f = float mat4
