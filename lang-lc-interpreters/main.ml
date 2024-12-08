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

module Named_nbe_closures : Lang = struct
  include Named_nbe_closures
  let to_named = Fun.id
  let of_named = Fun.id
  let normalise = normalise []
end

module Named_nbe_hoas : Lang = struct
  include Named_nbe_hoas
  let to_named = Fun.id
  let of_named = Fun.id
  let normalise = normalise []
end


module Nameless : Lang = Nameless

module Nameless_nbe_closures : Lang = struct
  include Nameless_nbe_closures
  let normalise = normalise []
end

module Nameless_nbe_hoas : Lang = struct
  include Nameless_nbe_hoas
  let normalise = normalise []
end


module Unique : Lang = Unique

module Unique_nbe_closures : Lang = struct
  include Unique_nbe_closures
  let normalise = normalise Id.Map.empty
end

module Unique_nbe_hoas : Lang = struct
  include Unique_nbe_hoas
  let normalise = normalise Id.Map.empty
end
