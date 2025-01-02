(** Declarative diagramming API *)

include Diagram_intf

module Make (X : Core) : S
  with type t = X.t
= struct

  include X

  let stack dias =
    List.fold_left over empty dias

  let translate_x dx dia =
    translate (dx, 0.0) dia

  let translate_y dy dia =
    translate (0.0, dy) dia

end
