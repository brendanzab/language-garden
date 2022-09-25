(** {0 Common types for linear algebra} *)


(** {1 Type level natural numbers} *)

type zero = Z
type 'n succ = Succ of 'n

type n0 = zero
type n1 = n0 succ
type n2 = n1 succ
type n3 = n2 succ
type n4 = n3 succ


(** {1 Fixed-size vectors} *)

(** Vectors indexed with a statically known size. This is not a very efficient
    representation, but we don’t actually use these for computation so it
    doesn’t really matter. *)
type (_, _) vec =
  | Nil : ('s, zero) vec
  | Cons : 's * ('s, 'n) vec -> ('s, 'n succ) vec

type 's vec1 = ('s , n1) vec
type 's vec2 = ('s , n2) vec
type 's vec3 = ('s , n3) vec
type 's vec4 = ('s , n4) vec

type ('s, 'n) vec1n = ('s , 'n succ) vec
type ('s, 'n) vec2n = ('s , 'n succ) vec1n
type ('s, 'n) vec3n = ('s , 'n succ) vec2n
type ('s, 'n) vec4n = ('s , 'n succ) vec3n

type 'n vecf = (float, 'n) vec

type vec1f = float vec1
type vec2f = float vec2
type vec3f = float vec3
type vec4f = float vec4


(** {1 Fixed-size matrices} *)

type 's mat2 = ('s vec2) vec2
type 's mat3 = ('s vec3) vec3
type 's mat4 = ('s vec4) vec4

type mat2f = float mat2
type mat3f = float mat3
type mat4f = float mat4
