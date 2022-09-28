(** {0 Language of signed distance functions} *)

open ShaderTypes


(** A language for building signed distance functions. *)
module type S = sig

  (** The underlying representation of expressions in the SDF language *)
  type 'a repr

  (** Signed distance to the boundary of a shape.

      - Negative values are inside the shape
      - Zero values are on the boundary of the shape
      - Positive values are outside the shape
  *)
  type dist = float repr

  (** A signed distanced function (SDF) describes a surface as a signed distance
      coordinate to the a signed distance to boundary of the surface. *)
  type 'n sdf = ('n vecf) repr -> dist

  (** A two-dimensional distance function *)
  type sdf2 = n2 sdf

  (** A three-dimensional distance function *)
  type sdf3 = n3 sdf


  (** {1 Primitive shape functions} *)

  (** {2 2D shapes} *)

  (** Circle of a given radius *)
  val circle : float repr -> sdf2

  (** Square of a given radius *)
  val square : float repr -> sdf2

  (* Rectangle with extents measured from the origin *)
  val rectangle : vec2f repr -> sdf2

  (* Line segment between two points *)
  val line_segment : vec2f repr -> vec2f repr -> sdf2

  (** {2 3D shapes} *)

  (* TODO: 3D shape functions *)


  (** {1 Operators for combining shapes} *)

  (** The union of two surfaces *)
  val union : dist -> dist -> dist

  (** The intersection of two surfaces *)
  val intersection : dist -> dist -> dist

  (** The difference of two surfaces *)
  val difference : dist -> dist -> dist

  (** {2 Chamfered combining operators} *)

  (** Union of two surfaces, meeting at a chamfered edge.
      The edge is a diagonal of a square of the specified size. *)
  val union_chamfer : dist -> dist -> float repr -> dist

  (** Intersection of two surfaces, meeting at a chamfered edge.
      The edge is a diagonal of a square of the specified size. *)
  val intersection_chamfer : dist -> dist -> float repr -> dist

  (** Difference of two surfaces, meeting at a chamfered edge.
      The edge is a diagonal of a square of the specified size. *)
  val difference_chamfer : dist -> dist -> float repr -> dist

  (** {2 Rounded combining operators} *)

  (** Union of two surfaces, meeting at a quarter-circle. *)
  val union_round : dist -> dist -> float repr -> dist

  (** Intersection of two surfaces, meeting at a quarter-circle. *)
  val intersection_round : dist -> dist -> float repr -> dist

  (** Difference of two surfaces, meeting at a quarter-circle. *)
  val difference_round : dist -> dist -> float repr -> dist


  (** {1 Position operations} *)

  (** Move a distance function by the supplied vector *)
  val move : ('n vecf) repr -> 'n sdf -> 'n sdf

  (** Uniformly scale a distance function by an amount *)
  val scale : float repr -> 'n sdf -> 'n sdf

  (* TODO: rotate *)


  (** {1 Reflection operations} *)

  (** Reflect a copy of the distance function in each axis *)
  val reflect : 'n sdf -> 'n sdf

  (** Reflect a copy of the distance function in the x axis *)
  val reflect_x : ('n ge1) sdf -> ('n ge1) sdf

  (** Reflect a copy of the distance function in the y axis *)
  val reflect_y : ('n ge2) sdf -> ('n ge2) sdf

  (** Reflect a copy of the distance function in the z axis *)
  val reflect_z : ('n ge3) sdf -> ('n ge3) sdf


  (** {1 Repetition operations} *)

  (** Repeat a distance function with the given spacing vector. The repetition
      can be optionally limited to a bounding volume. *)
  val repeat : spacing:('n vecf) repr -> ?limit:('n vecf) repr -> 'n sdf -> 'n sdf


  (** {1 Deformations and distortions} *)

  (** Displace an SDF with the supplied function *)
  val displace : (('n vecf) repr -> dist) -> 'n sdf -> 'n sdf


  (** {1 Compositing operations} *)

  (** Overlay a surface on top of a background color, painting it with a
      foreground color. *)
  val overlay : bg:vec3f repr -> fg:vec3f repr -> dist -> vec3f repr

  (* TODO: twist *)
  (* TODO: bend *)


end


(** Create an implementation of SDFs for a given shader language. *)
module Make (S : Shader.S) : S

  with type 'a repr = 'a S.repr

= struct

  open Shader.Notation (S)


  type 'a repr = 'a S.repr

  type dist = float repr
  type 'n sdf = ((float, 'n) vec) repr -> dist
  type sdf2 = n2 sdf
  type sdf3 = n3 sdf


  (** Clamp to the range \[0, 1\] *)
  let saturate e =
      S.clamp e ~min:!!0.0 ~max:!!0.1

  (** 2D origin *)
  let zero2 =
    S.vec2 !!0.0 !!0.0

  (** Maximum component of a 2D vector *)
  let vmax2 v =
    S.max (S.x v) (S.y v)


  (** Based on the equation:

      [ x^2 + y^2 = r^2 ]

      Where:

      - [x] = x-coordinate
      - [y] = y-coordinate
      - [r] = radius
  *)
  let circle radius uv =
    S.length uv - radius

  (** Based on the equation:

      [ max(abs(x), abs(y)) = r ]

      Where:

      - [x] = x-coordinate
      - [y] = y-coordinate
      - [r] = radius
  *)
  let square radius uv =
    vmax2 (S.abs_vec uv) - radius

  (* Distance functions based on https://iquilezles.org/articles/distfunctions2d/ *)
  (* See also https://mercury.sexy/hg_sdf/ *)

  let rectangle extents uv =
    let d = S.abs_vec uv |-| extents in
    S.length (S.max_vec d zero2) + S.min (vmax2 d) !!0.0

  let line_segment p1 p2 uv =
    (* let uv_p1 = uv |-| p1 in
    let p2_p1 = p2 |-| p1 in
    let h = saturate (S.dot uv_p1 p2_p1 / S.dot p2_p1 p2_p1) in
    S.length (uv_p1 |-| (p2_p1 |* h)) *)
	(* vec3 ab = b - a;
	float t = saturate(dot(p - a, ab) / dot(ab, ab));
	return length((ab*t + a) - p); *)
    let p2_p1 = p2 |-| p1 in
    let h = saturate (S.dot (uv |-| p1) p2_p1 / S.dot p2_p1 p2_p1) in
    S.length (((p2_p1 |* h) |+| p1) |-| uv)


  let union = S.min
  let intersection = S.max
  let difference d1 d2 = intersection d1 (S.neg d2)

  (* Distance functions based on https://mercury.sexy/hg_sdf/ *)
  (* See also https://iquilezles.org/articles/smin/ *)

  let union_chamfer d1 d2 r =
    S.min (S.min d1 d2) ((d1 - r + d2) * S.sqrt !!0.5)

  let intersection_chamfer d1 d2 r =
    S.max (S.max d1 d2) ((d1 + r + d2) * S.sqrt !!0.5)

  let difference_chamfer d1 d2 r =
    intersection_chamfer d1 (S.neg d2) r

  let union_round d1 d2 r =
    let u = S.max_vec (S.vec2 (r - d1) (r - d2)) zero2 in
    S.max r (S.min d1 d2) - S.length u

  let intersection_round d1 d2 r =
    let u = S.max_vec (S.vec2 (r + d1) (r + d2)) zero2 in
    S.min (S.neg r) (S.max d1 d2) + S.length u

  let difference_round d1 d2 r =
    intersection_round d1 (S.neg d2) r


  let move v sdf uv =
    sdf (uv |-| v)

  let scale factor sdf uv =
    sdf (uv |/ factor) * factor


  let reflect sdf uv =
    sdf (uv |> S.abs_vec)

  let reflect_x sdf uv =
    sdf (uv |> S.set_x (uv |> S.x |> S.abs))

  let reflect_y sdf uv =
    sdf (uv |> S.set_y (uv |> S.y |> S.abs))

  let reflect_z sdf uv =
    sdf (uv |> S.set_z (uv |> S.z |> S.abs))


  let repeat ~spacing ?limit sdf uv =
    match limit with
    (* Infinite repetitions *)
    | None ->
        let spacing_half = spacing |* !!0.5 in
        sdf ((uv |+| spacing_half) |%| spacing |-| spacing_half)
    (* Limited repetitions *)
    | Some limit ->
        let neg_limit = S.neg_vec limit in
        let offset = spacing |*| S.clamp_vec (S.round_vec (uv |/| spacing)) ~min:neg_limit ~max:limit in
        sdf (uv |-| offset)


  let displace f sdf uv =
    let d1 = sdf uv in
    let d2 = f uv in
    d1 + d2


  let overlay ~bg ~fg shape =
    S.lerp_scalar fg bg (S.step ~edge:!!0.0 shape)

end
