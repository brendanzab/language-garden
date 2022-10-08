(** {0 A stack machine for arithmetic expressions}

    This represents arithmetic expressions as a
    {{:https://en.wikipedia.org/wiki/Stack_machine} stack machine}.
*)


(** {1 Stack machine syntax} *)

(** Stack machine instructions *)
type inst =
  | Int of int     (** [         -- i     ] *)
  | Bool of bool   (** [         -- b     ] *)
  | Code of code   (** [         -- c     ] *)
  | Neg            (** [ i       -- -n    ] *)
  | Add            (** [ i1 i2   -- i1+i2 ] *)
  | Sub            (** [ i1 i2   -- i1-i2 ] *)
  | Mul            (** [ i1 i2   -- i1*i2 ] *)
  | Div            (** [ i1 i2   -- i1/i2 ] *)
  | Eq             (** [ v1 v2   -- v1=v2 ] *)
  | IfThenElse     (** [ b c1 c2 -- v     ] *)

and code =
  inst list


  (** {1 Pretty printing} *)

let rec pp_inst fmt = function
  | Int i -> Format.fprintf fmt "int %d" i
  | Bool true -> Format.fprintf fmt "true"
  | Bool false -> Format.fprintf fmt "false"
  | Code [] -> Format.fprintf fmt "[]"
  | Code c -> Format.fprintf fmt "code @[[@ %a@ ]@]" pp_code c
  | Neg -> Format.fprintf fmt "neg"
  | Add -> Format.fprintf fmt "add"
  | Sub -> Format.fprintf fmt "sub"
  | Mul -> Format.fprintf fmt "mul"
  | Div -> Format.fprintf fmt "div"
  | Eq -> Format.fprintf fmt "eq"
  | IfThenElse -> Format.fprintf fmt "if"
and pp_code fmt = function
  | [] -> ()
  | inst :: [] -> Format.fprintf fmt "%a;" pp_inst inst
  | inst :: code -> Format.fprintf fmt "%a;@ %a" pp_inst inst pp_code code


(** Semantics of arithmetic expressions *)
module Semantics = struct

  type value =
    | Int of int
    | Bool of bool
    | Code of code

  type stack =
    value list

  let step : code * stack -> code * stack = function
    | Int i       :: code,                                stack -> code, Int i          :: stack
    | Bool b      :: code,                                stack -> code, Bool b         :: stack
    | Code c      :: code,                                stack -> code, Code c         :: stack
    | Neg         :: code, Int i ::                       stack -> code, Int (-i)       :: stack
    | Add         :: code, Int i2 :: Int i1 ::            stack -> code, Int (i1 + i2)  :: stack
    | Sub         :: code, Int i2 :: Int i1 ::            stack -> code, Int (i1 - i2)  :: stack
    | Mul         :: code, Int i2 :: Int i1 ::            stack -> code, Int (i1 * i2)  :: stack
    | Div         :: code, Int i2 :: Int i1 ::            stack -> code, Int (i1 / i2)  :: stack
    | Eq          :: code, v2 :: v1 ::                    stack -> code, Bool (v1 = v2) :: stack
    | IfThenElse  :: code, _ :: Code c1 :: Bool true  ::  stack -> c1 @ code,              stack
    | IfThenElse  :: code, Code c2 :: _ :: Bool false ::  stack -> c2 @ code,              stack
    | _, _ -> failwith "invalid code"

  let rec eval ?(stack = []) = function
    | [] -> stack
    | code ->
        let (code, stack) = step (code, stack) in
        eval code ~stack

  let rec quote : stack -> code = function
    | [] -> []
    | Int i :: stack -> Int i :: quote stack
    | Bool b :: stack -> Bool b :: quote stack
    | Code c :: stack -> Code c :: quote stack

  let normalise t =
    quote (eval t)

end
