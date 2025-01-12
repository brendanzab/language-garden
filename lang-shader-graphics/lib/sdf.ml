(** {0 Language of signed distance functions} *)

open Data


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
  type sdf2 = Nat.n2 sdf

  (** A three-dimensional distance function *)
  type sdf3 = Nat.n3 sdf


  (** {1 Primitive shape functions} *)

  (** {2 2D shapes} *)

  (** Circle of a given radius *)
  val circle : float repr -> sdf2

  (** Square of a given radius *)
  val square : float repr -> sdf2

  (* Rectangle with extents measured from the origin *)
  val rectangle : vec2f repr -> sdf2

  (* Line segments between two points *)
  val segment : 'n. ?radius:float repr -> ('n vecf) repr -> ('n vecf) repr -> 'n sdf

  (* Line from the origin, up to a length in the y axis *)
  val segment_y : 'n. ?radius:float repr -> float repr -> ('n Nat.ge2) sdf

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
  val move : 'n. ('n vecf) repr -> 'n sdf -> 'n sdf

  (** Uniformly scale a distance function by an amount *)
  val scale : 'n. float repr -> 'n sdf -> 'n sdf

  (* TODO: rotate *)


  (** {1 Reflection operations} *)

  (** Reflect a copy of the distance function in each axis *)
  val reflect : 'n. 'n sdf -> 'n sdf

  (** Reflect a copy of the distance function in the x axis *)
  val reflect_x : 'n. ('n Nat.ge1) sdf -> ('n Nat.ge1) sdf

  (** Reflect a copy of the distance function in the y axis *)
  val reflect_y : 'n. ('n Nat.ge2) sdf -> ('n Nat.ge2) sdf

  (** Reflect a copy of the distance function in the z axis *)
  val reflect_z : ('n Nat.ge3) sdf -> ('n Nat.ge3) sdf


  (** {1 Repetition operations} *)

  (** Repeat a distance function with the given spacing vector. The repetition
      can be optionally limited to a bounding volume. *)
  val repeat : 'n. spacing:('n vecf) repr -> ?limit:('n vecf) repr -> 'n sdf -> 'n sdf


  (** {1 Deformations and distortions} *)

  (** Displace an SDF with the supplied function *)
  val displace : 'n. (('n vecf) repr -> dist) -> 'n sdf -> 'n sdf


  (** {1 Compositing operations} *)

  (** Overlay a shape on top of a background color, painting it with a
      foreground color. *)
  val overlay : shape:dist -> color:vec3f repr -> ?smoothing:float repr -> vec3f repr -> vec3f repr

  (* TODO: twist *)
  (* TODO: bend *)


end


(** Create an implementation of SDFs for a given shader language. *)
module Make (L : Shader.S) : S
  with type 'a repr = 'a L.repr
= struct

  open L.O

  type 'a repr = 'a L.repr

  type dist = float repr
  type 'n sdf = ('n vecf) repr -> dist
  type sdf2 = Nat.n2 sdf
  type sdf3 = Nat.n3 sdf


  (** Based on the equation:

      [ x^2 + y^2 = r^2 ]

      Where:

      - [x] = x-coordinate
      - [y] = y-coordinate
      - [r] = radius
  *)
  let circle radius uv =
    L.length uv - radius

  (** Based on the equation:

      [ max(abs(x), abs(y)) = r ]

      Where:

      - [x] = x-coordinate
      - [y] = y-coordinate
      - [r] = radius
  *)
  let square radius uv =
    L.max_component2 (L.abs_vec uv) - radius

  (* Distance functions based on https://iquilezles.org/articles/distfunctions2d/ *)
  (* See also https://mercury.sexy/hg_sdf/ *)

  let rectangle extents uv =
    let d = L.abs_vec uv |-| extents in
    L.length (L.max_vec d L.zero2) + L.min (L.max_component2 d) !!0.0

  (** See: {{:https://www.youtube.com/watch?v=PMltMdi1Wzg} The SDF of a Line Segment} *)
  let segment ?radius p1 p2 uv =
    let uv_p1 = uv |-| p1 in
    let p2_p1 = p2 |-| p1 in
    let h = L.saturate (L.dot uv_p1 p2_p1 / L.dot p2_p1 p2_p1) in
    match radius with
    | None ->L.length (uv_p1 |-| (p2_p1 |* h))
    | Some r -> L.length (uv_p1 |-| (p2_p1 |* h)) - r

  (** See: {{:https://www.youtube.com/watch?v=PMltMdi1Wzg} The SDF of a Line Segment} *)
  let segment_y ?radius len uv =
    let uv = uv.%{Y} <- uv.%{Y} - L.clamp uv.%{Y} ~min:!!0.0 ~max:len in
    match radius with
    | None -> L.length uv
    | Some r -> L.length uv - r

  let union = L.min
  let intersection = L.max
  let difference d1 d2 = intersection d1 (L.neg d2)

  (* Distance functions based on https://mercury.sexy/hg_sdf/ *)
  (* See also https://iquilezles.org/articles/smin/ *)

  let union_chamfer d1 d2 r =
    L.min (L.min d1 d2) ((d1 - r + d2) * L.sqrt !!0.5)

  let intersection_chamfer d1 d2 r =
    L.max (L.max d1 d2) ((d1 + r + d2) * L.sqrt !!0.5)

  let difference_chamfer d1 d2 r =
    intersection_chamfer d1 (L.neg d2) r

  let union_round d1 d2 r =
    let u = L.max_vec (L.vec2 (r - d1) (r - d2)) L.zero2 in
    L.max r (L.min d1 d2) - L.length u

  let intersection_round d1 d2 r =
    let u = L.max_vec (L.vec2 (r + d1) (r + d2)) L.zero2 in
    L.min (L.neg r) (L.max d1 d2) + L.length u

  let difference_round d1 d2 r =
    intersection_round d1 (L.neg d2) r


  let move v sdf uv =
    sdf (uv |-| v)

  let scale factor sdf uv =
    sdf (uv |/ factor) * factor


  let reflect sdf uv =
    sdf (uv |> L.abs_vec)

  let reflect_x sdf uv =
    sdf (uv.%{X} <- (uv.%{X} |> L.abs))

  let reflect_y sdf uv =
    sdf (uv.%{Y} <- (uv.%{Y} |> L.abs))

  let reflect_z sdf uv =
    sdf (uv.%{Z} <- (uv.%{Z} |> L.abs))


  let repeat ~spacing ?limit sdf uv =
    match limit with
    (* Infinite repetitions *)
    | None ->
        let spacing_half = spacing |* !!0.5 in
        sdf ((uv |+| spacing_half) |%| spacing |-| spacing_half)
    (* Limited repetitions *)
    | Some limit ->
        let neg_limit = L.neg_vec limit in
        let offset = spacing |*| L.clamp_vec (L.round_vec (uv |/| spacing)) ~min:neg_limit ~max:limit in
        sdf (uv |-| offset)


  let displace f sdf uv =
    sdf uv + f uv


  let overlay ~shape ~color ?smoothing background =
    let boundary =
      match smoothing with
      | None -> L.step ~edge:!!0.0 shape
      | Some smoothing -> L.smooth_step ~lower:!!0.0 ~upper:smoothing shape
    in
    L.lerp_scalar color background boundary

end
