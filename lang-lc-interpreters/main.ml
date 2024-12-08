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

module NamedNbeClosures : Lang = struct
  include NamedNbeClosures
  let to_named = Fun.id
  let of_named = Fun.id
  let normalise = normalise []
end

module NamedNbeHoas : Lang = struct
  include NamedNbeHoas
  let to_named = Fun.id
  let of_named = Fun.id
  let normalise = normalise []
end


module Nameless : Lang = Nameless

module NamelessNbeClosures : Lang = struct
  include NamelessNbeClosures
  let normalise = normalise []
end

module NamelessNbeHoas : Lang = struct
  include NamelessNbeHoas
  let normalise = normalise []
end


module Unique : Lang = Unique

module UniqueNbeClosures : Lang = struct
  include UniqueNbeClosures
  let normalise = normalise Id.Map.empty
end

module UniqueNbeHoas : Lang = struct
  include UniqueNbeHoas
  let normalise = normalise Id.Map.empty
end
