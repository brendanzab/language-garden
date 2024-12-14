(** Model of a branching tree *)

(*

  type tree :=
    | .apex
    | .fork(tree, tree)
    | .branch(tree)

  def axiom :=
    .apex

  def rules :=
    | .apex        -> .branch(.fork(.apex, .apex))
    | .branch(...) -> .branch(.branch(...))

  def apex-diameter := 3px
  def fork-angle := 45deg
  def branch-length := 6px

  def draw :=
    | .apex ->
        circle apex-diameter
    | .fork(tree1..., tree2...) ->
        stack [
          rotate (+fork-angle) (tree2...),
          rotate (-fork-angle) (tree2...),
        ]
    | .branch(...) ->
        stack [
          line (0.0, 0.0) (0.0, -branch-length),
          translate-y (-branch-length) ...,
        ]

*)

module Diagram = Declarative_graphics.Diagram

type t =
  | Apex
  | Fork of t * t
  | Branch of t

let axiom : t =
  Apex

let rules (rules : t -> t) : t -> t =
  function
  | Apex -> Branch (Fork (Apex, Apex))
  | Fork (tree1, tree2) -> Fork (rules tree1, rules tree2)
  (* Boilerplate productions *)
  | Branch tree -> Branch (Branch (rules tree))

let apex_diameter = 3.0
let fork_angle = 45.0 *. Float.pi /. 180.0
let branch_length = 6.0

let draw (type d) (module D : Diagram.S with type t = d) (draw : t -> d) : t -> d =
  function
  | Apex ->
      D.circle ~diameter:apex_diameter
        |> D.fill `solid

  | Fork (tree1, tree2) ->
      D.stack [
        D.rotate ~radians:(+.fork_angle) (draw tree1);
        D.rotate ~radians:(-.fork_angle) (draw tree2);
      ]

  | Branch tree ->
      D.stack [
        D.line (0.0, 0.0) (0.0, -.branch_length)
          |> D.stroke `solid;
        D.translate_y (-.branch_length) (draw tree);
      ]
