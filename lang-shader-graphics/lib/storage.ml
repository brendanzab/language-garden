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
type (_, _) vec =
  | Nil : ('s, zero) vec
  | Cons : 's * ('s, 'n) vec -> ('s, 'n succ) vec

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

(** {2 Vector constructors} *)

let vec2 s1 s2 = Cons (s1, Cons (s2, Nil))
let vec3 s1 s2 s3 = Cons (s1, Cons (s2, Cons (s3, Nil)))
let vec4 s1 s2 s3 s4 = Cons (s1, Cons (s2, Cons (s3, Cons (s4, Nil))))

(** {2 Vector component accessors} *)

(** Named component of a vector *)
type _ component =
  | X : ('n ge1) component
  | Y : ('n ge2) component
  | Z : ('n ge3) component
  | W : ('n ge4) component

(* TODO: Use numeric indexing instead *)

let get (type a n) (c : n component) (v : (a, n) vec) : a =
  match c, v with
  | X, Cons (s, _) -> s
  | Y, Cons (_, Cons (s, _)) -> s
  | Z, Cons (_, Cons (_, Cons (s, _))) -> s
  | W, Cons (_, Cons (_, Cons (_, Cons (s, _)))) -> s

let rec swizzle : type a n m. (n component, m) vec -> (a, n) vec -> (a, m) vec =
  fun cv v ->
    match cv with
    | Nil -> Nil
    | Cons (c, cv) -> Cons (get c v, swizzle cv v)

let get2 (type a n) (c1, c2 : n component * n component) (v : (a, n) vec) : a vec2 =
  swizzle (vec2 c1 c2) v

let get3 (type a n) (c1, c2, c3 : n component * n component * n component) (v : (a, n) vec) : a vec3 =
  swizzle (vec3 c1 c2 c3) v

let get4 (type a n) (c1, c2, c3, c4 : n component * n component * n component * n component) (v : (a, n) vec) : a vec4 =
  swizzle (vec4 c1 c2 c3 c4) v

let set (type n) (c : n component) (s : float) (v : n vecf) : n vecf =
  match c, v with
  | X, Cons (_, v) -> Cons (s, v)
  | Y, Cons (sx, Cons (_, v)) -> Cons (sx, Cons (s, v))
  | Z, Cons (sx, Cons (sy, Cons (_, v))) -> Cons (sx, Cons (sy, Cons (s, v)))
  | W, Cons (sx, Cons (sy, Cons (sz, Cons (_, v)))) -> Cons (sx, Cons (sy, Cons (sz, Cons (s, v))))

(** {2 Vector combinators} *)

let rec map_vec : type a b n. (a -> b) -> (a, n) vec -> (b, n) vec =
  fun f -> function
    | Nil -> Nil
    | Cons (s, v) -> Cons (f s, map_vec f v)

let rec fold_left_vec : type a b n. (a -> b -> a) -> a -> (b, n) vec -> a =
  fun f acc -> function
    | Nil -> acc
    | Cons (s, v) -> fold_left_vec f (f acc s) v

let rec zip_with_vec : type a b c n. (a -> b -> c) -> (a, n) vec -> (b, n) vec -> (c, n) vec =
  fun f v1 v2 ->
    match v1, v2 with
    | Nil, Nil -> Nil
    | Cons (s1, v1), Cons (s2, v2) ->
        Cons (f s1 s2, zip_with_vec f v1 v2)

let rec zip_with3_vec : type a b c d n. (a -> b -> c -> d) -> (a, n) vec -> (b, n) vec -> (c, n) vec -> (d, n) vec =
  fun f v1 v2 v3 ->
    match v1, v2, v3 with
    | Nil, Nil, Nil -> Nil
    | Cons (s1, v1), Cons (s2, v2), Cons (s3, v3) ->
        Cons (f s1 s2 s3, zip_with3_vec f v1 v2 v3)


(** {1 Fixed-size matrices} *)

type 's mat2 = ('s vec2) vec2
type 's mat3 = ('s vec3) vec3
type 's mat4 = ('s vec4) vec4

type mat2f = float mat2
type mat3f = float mat3
type mat4f = float mat4


module Notation = struct

  let ( .%{ } ) v c = get c v
  let ( .%{ }<- ) v c s = set c s v

end
