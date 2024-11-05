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

type tree =
  | Apex
  | Fork of tree * tree
  | Branch of tree

let axiom : tree =
  Apex

let rules (rules : tree -> tree) : tree -> tree =
  function
  | Apex -> Branch (Fork (Apex, Apex))
  | Fork (tree1, tree2) -> Fork (rules tree1, rules tree2)
  | Branch tree -> Branch (Branch (rules tree))

let rec step tree =
  rules step tree

let grow (iters : int) : tree =
  let rec go iters tree =
    if iters < 0 then tree else
      (go [@tailcall]) (iters - 1) (step tree)
  in
  go iters axiom

let generations axiom =
  Seq.unfold (fun t -> Some (t, step t)) axiom
