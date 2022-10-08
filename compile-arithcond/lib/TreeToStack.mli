(** Translation pass between the {!TreeLang} and {!StackLang} *)

include Translation.S
  with type source = TreeLang.expr
  with type target = StackLang.code
