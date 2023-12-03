(* TODO: entrypoint *)

(* NOTE: Tempory re-exports to ensure modules are typechecked *)

module Lexer = Lexer
module Parser = Parser

module type Lang = sig

  type expr

  val to_named : expr -> Named.expr
  val of_named : Named.expr -> expr
  val alpha_equiv : expr -> expr -> bool
  val normalise : expr -> expr

end
[@@warning "-unused-value-declaration"]


module Named : Lang
  with type expr = Named.expr
= struct
  include Named
  let to_named = Fun.id
  let of_named = Fun.id
end

module NamedClosures : Lang = struct
  include NamedClosures
  let to_named = Fun.id
  let of_named = Fun.id
  let normalise = normalise []
end

module NamedHoas : Lang = struct
  include NamedHoas
  let to_named = Fun.id
  let of_named = Fun.id
  let normalise = normalise []
end


module Nameless : Lang = Nameless

module NamelessClosures : Lang = struct
  include NamelessClosures
  let normalise = normalise []
end

module NamelessHoas : Lang = struct
  include NamelessHoas
  let normalise = normalise []
end


module Unique : Lang = Unique

module UniqueClosures : Lang = struct
  include UniqueClosures
  let normalise = normalise Id.Map.empty
end

module UniqueHoas : Lang = struct
  include UniqueHoas
  let normalise = normalise Id.Map.empty
end
