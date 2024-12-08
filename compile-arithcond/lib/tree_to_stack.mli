(** Translation pass between the {!Tree_lang} and {!Stack_lang} *)

include Translation.S
  with type source = Tree_lang.expr
  with type target = Stack_lang.code
