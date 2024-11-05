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

  def draw :=
    | .apex -> circle 3.0
    | .fork(tree1..., tree2...) ->
        stack [
          rotate (+angle) tree1...,
          rotate (-angle) tree2...,
        ]
    | .branch(...) ->
        stack [
          line (0.0, 0.0) (0.0, -len),
          translate-y (-len) ...,
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

let rec step f =
  rules step f

let[@warning "-unused-value-declaration"] generate axiom =
  Seq.unfold (fun t -> Some (t, step t)) axiom
