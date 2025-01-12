(** {0 Common storage types used in GPU shader languages} *)

module Nat = Nat
module Vec = Vec

(** {1 Fixed-size vectors} *)

type ('s, 'n) vec = ('s, 'n) Vec.t

type 'n vecf = (float, 'n) vec

(** {2 Vectors of a given size} *)

type 's vec1 = ('s, Nat.n1) vec
type 's vec2 = ('s, Nat.n2) vec
type 's vec3 = ('s, Nat.n3) vec
type 's vec4 = ('s, Nat.n4) vec

type vec1f = Nat.n1 vecf
type vec2f = Nat.n2 vecf
type vec3f = Nat.n3 vecf
type vec4f = Nat.n4 vecf

(** {2 Vectors that are greater than or equal to a given size} *)

type ('s, 'n) vec_ge1 = ('s, 'n Nat.ge1) vec
type ('s, 'n) vec_ge2 = ('s, 'n Nat.ge2) vec
type ('s, 'n) vec_ge3 = ('s, 'n Nat.ge3) vec
type ('s, 'n) vec_ge4 = ('s, 'n Nat.ge4) vec

type 'n vec_ge1f = ('n Nat.ge1) vecf
type 'n vec_ge2f = ('n Nat.ge2) vecf
type 'n vec_ge3f = ('n Nat.ge3) vecf
type 'n vec_ge4f = ('n Nat.ge4) vecf


(** {1 Fixed-size matrices} *)

type 's mat2 = ('s vec2) vec2
type 's mat3 = ('s vec3) vec3
type 's mat4 = ('s vec4) vec4

type mat2f = float mat2
type mat3f = float mat3
type mat4f = float mat4
