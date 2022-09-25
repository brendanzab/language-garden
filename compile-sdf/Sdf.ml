(** {0 Language of signed distance functions} *)

open MathTypes


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
  type 'n sdf = ((float, 'n) vec) repr -> dist

  (** A two-dimensional distance function *)
  type sdf2 = n2 sdf

  (** A three-dimensional distance function *)
  type sdf3 = n3 sdf


  (** {1 Primitive shape functions} *)

  (** Circle of a given radius *)
  val circle : float repr -> sdf2

  (** Square of a given radius *)
  val square : float repr -> sdf2


  (** {1 Operators for combining shapes} *)

  (** Union of two surfaces *)
  val union : dist -> dist -> dist

  (** The intersection of two surfaces *)
  val intersect : dist -> dist -> dist

  (** Substract one surface from another surface *)
  val subtract : dist -> dist -> dist


  (** {1 Operations on the domain of signed distance functions} *)

  (** Move a distance function by the supplied vector *)
  val move : ('n vecf) repr -> 'n sdf -> 'n sdf

  (** Uniformly scale a distance function by an amount *)
  val scale : float repr -> 'n sdf -> 'n sdf

  (** Reflect a copy of the distance function in each axis *)
  val reflect : 'n sdf -> 'n sdf

  (* TODO: Generalise reflect operations over dimension *)

  (** Reflect a copy of the distance function in the x axis *)
  val reflect2_x : sdf2 -> sdf2

  (** Reflect a copy of the distance function in the y axis *)
  val reflect2_y : sdf2 -> sdf2

  (** Reflect a copy of the distance function in the x axis *)
  val reflect3_x : sdf3 -> sdf3

  (** Reflect a copy of the distance function in the y axis *)
  val reflect3_y : sdf3 -> sdf3

  (** Reflect a copy of the distance function in the z axis *)
  val reflect3_z : sdf3 -> sdf3

  (** Repeat a distance function with the given spacing vector. The repetition
      cab be optionally limited to a bounding volume. *)
  val repeat : spacing:('n vecf) repr -> ?limit:('n vecf) repr -> 'n sdf -> 'n sdf


  (** {1 Compositing operations} *)

  (** Overlay a surface on top of a background color, painting it with a
      foreground color. *)
  val overlay : bg:vec3f repr -> fg:vec3f repr -> dist -> vec3f repr

end


(** Create an implementation of the SDF language using the mathematics module. *)
module Make (M : Math.S) : S

  with type 'a repr = 'a M.repr

= struct

  open Math.Notation (M)


  type 'a repr = 'a M.repr

  type dist = float repr
  type 'n sdf = ((float, 'n) vec) repr -> dist
  type sdf2 = n2 sdf
  type sdf3 = n3 sdf


  (** Based on the equation:

      [ x^2 + y^2 = r^2 ]

      Where:

      - [x] = x-coordinate
      - [y] = y-coordinate
      - [r] = radius
  *)
  let circle radius uv =
    M.length uv - radius

  (** Based on the equation:

      [ max(abs(x), abs(y)) = r ]

      Where:

      - [x] = x-coordinate
      - [y] = y-coordinate
      - [r] = radius
  *)
  let square radius uv =
    let x_dist = uv |> M.x |> M.abs in
    let y_dist = uv |> M.y |> M.abs in
    M.max x_dist y_dist - radius


  let union = M.min
  let intersect = M.max
  let subtract d1 d2 = M.min d1 (M.neg d2)


  let move v sdf uv =
    sdf (uv |-| v)

  let scale factor sdf uv =
    sdf (uv |/ factor) * factor

  let reflect sdf uv =
    sdf (uv |> M.abs_vec)

  let reflect2_x sdf uv =
    sdf (M.vec2
      (uv |> M.x |> M.abs)
      (uv |> M.y))

  let reflect2_y sdf uv =
    sdf (M.vec2
      (uv |> M.x)
      (uv |> M.y |> M.abs))

  let reflect3_x sdf uv =
    sdf (M.vec3
      (uv |> M.x |> M.abs)
      (uv |> M.y)
      (uv |> M.z))

  let reflect3_y sdf uv =
    sdf (M.vec3
      (uv |> M.x)
      (uv |> M.y |> M.abs)
      (uv |> M.z))

  let reflect3_z sdf uv =
    sdf (M.vec3
      (uv |> M.x)
      (uv |> M.y)
      (uv |> M.z |> M.abs))

  let repeat ~spacing ?limit sdf uv =
    match limit with
    (* Infinite repetitions *)
    | None ->
        let spacing_half = spacing |* (M.float 0.5) in
        sdf ((uv |+| spacing_half) |%| spacing |-| spacing_half)
    (* Limited repetitions *)
    | Some limit ->
        let neg_limit = M.neg_vec limit in
        let offset = spacing |*| M.clamp_vec (M.round_vec (uv |/| spacing)) ~min:neg_limit ~max:limit in
        sdf (uv |-| offset)


  let overlay ~bg ~fg shape =
    M.lerp_scalar fg bg (M.step (M.float 0.0) shape)

end
