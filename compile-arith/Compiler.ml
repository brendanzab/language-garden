(** A tree based language for describing arithmetic expressions *)
module TreeLang = struct

  type term =
    | Num of int
    | Neg of term
    | Add of term * term
    | Sub of term * term
    | Mul of term * term
    | Div of term * term

  let rec pp_term fmt term =
    pp_add_term fmt term
  and pp_add_term fmt = function
    | Add (t1, t2) -> Format.fprintf fmt "%a@ +@ %a" pp_mul_term t1 pp_add_term t2
    | Sub (t1, t2) -> Format.fprintf fmt "%a@ -@ %a" pp_mul_term t1 pp_add_term t2
    | t -> pp_mul_term fmt t
  and pp_mul_term fmt = function
    | Mul (t1, t2) -> Format.fprintf fmt "%a@ *@ %a" pp_atomic_term t1 pp_mul_term t2
    | Div (t1, t2) -> Format.fprintf fmt "%a@ /@ %a" pp_atomic_term t1 pp_mul_term t2
    | t -> pp_atomic_term fmt t
  and pp_atomic_term fmt = function
    | Num n -> Format.fprintf fmt "%d" n
    | Neg t -> Format.fprintf fmt "-%a" pp_atomic_term t
    | t -> Format.fprintf fmt "@[<1>(%a)@]" pp_term t


  module Semantics = struct

    type value = int

    let rec eval : term -> value =
      function
      | Num n -> n
      | Neg n -> -(eval n)
      | Add (n1, n2) -> eval n1 + eval n2
      | Sub (n1, n2) -> eval n1 - eval n2
      | Mul (n1, n2) -> eval n1 * eval n2
      | Div (n1, n2) -> eval n1 / eval n2

  end

end


(** A stack based language for describing arithmetic expressions *)
module StackLang = struct
  type instruction =
    | Num of int    (** [       -- n     ] *)
    | Neg           (** [ n     -- -n    ] *)
    | Add           (** [ n1 n2 -- n1+n2 ] *)
    | Sub           (** [ n1 n2 -- n1-n2 ] *)
    | Mul           (** [ n1 n2 -- n1*n2 ] *)
    | Div           (** [ n1 n2 -- n1/n2 ] *)

  type program =
    instruction list

  let pp_instruction fmt = function
    | Num n -> Format.fprintf fmt "num %d" n
    | Neg -> Format.fprintf fmt "neg"
    | Add -> Format.fprintf fmt "add"
    | Sub -> Format.fprintf fmt "sub"
    | Mul -> Format.fprintf fmt "mul"
    | Div -> Format.fprintf fmt "div"

  let rec pp_program fmt = function
    | instruction :: program ->
        Format.fprintf fmt "%a@.%a"
          pp_instruction instruction
          pp_program program
    | [] -> ()


  module Semantics = struct

    type value = int
    type stack = value list

    type state = {
      program : program;
      stack : stack;
    }

    let step : state -> state = function
      | { program = Num n :: program; stack } -> { program; stack = n :: stack }
      | { program = Neg :: program; stack = n :: stack } -> { program; stack = -n :: stack }
      | { program = Add :: program; stack = n2 :: n1 :: stack } -> { program; stack = n1 + n2 :: stack }
      | { program = Sub :: program; stack = n2 :: n1 :: stack } -> { program; stack = n1 - n2 :: stack }
      | { program = Mul :: program; stack = n2 :: n1 :: stack } -> { program; stack = n1 * n2 :: stack }
      | { program = Div :: program; stack = n2 :: n1 :: stack } -> { program; stack = n1 / n2 :: stack }
      | { program = _; stack = _ } -> failwith "invalid program"

  end

end


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
    | TreeLang.Neg n -> translate n @ [StackLang.Neg]
    | TreeLang.Add (n1, n2) -> translate n1 @ translate n2 @ [StackLang.Add]
    | TreeLang.Sub (n1, n2) -> translate n1 @ translate n2 @ [StackLang.Sub]
    | TreeLang.Mul (n1, n2) -> translate n1 @ translate n2 @ [StackLang.Mul]
    | TreeLang.Div (n1, n2) -> translate n1 @ translate n2 @ [StackLang.Div]

end
