include module type of Diagram_intf

module Make (X : Core) : S
  with type t = X.t
