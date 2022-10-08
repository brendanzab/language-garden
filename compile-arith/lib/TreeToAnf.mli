(** Translation pass between the {!TreeLang} and {!AnfLang} *)

include Translation.S
  with type source = TreeLang.expr
  with type target = AnfLang.expr
