(** A translation pass between two languages *)
module type Translation = sig
  (** The source language *)
  type source

  (** The target language *)
  type target

  (** The translation pass between the {!source} and {!target} languages *)
  val translate : source -> target
end

(** Translation pass between the {!TreeLang} and {!StackLang} *)
module TreeToStack : Translation

  with type source = TreeLang.term
  with type target = StackLang.program

= struct

  type source = TreeLang.term
  type target = StackLang.program

  let rec translate : TreeLang.term -> StackLang.program =
    function
    | TreeLang.Num n -> [StackLang.Num n]
    | TreeLang.Add (n1, n2) -> translate n1 @ translate n2 @ [StackLang.Add]
    | TreeLang.Sub (n1, n2) -> translate n1 @ translate n2 @ [StackLang.Sub]

end
