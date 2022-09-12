(** A stack based language of arithmetic expressions *)


(** Syntax of arithmetic expressions *)

type inst =
  | Num of int     (** [         -- n     ] *)
  | Bool of bool   (** [         -- b     ] *)
  | Code of code   (** [         -- c     ] *)
  | Neg            (** [ n       -- -n    ] *)
  | Add            (** [ n1 n2   -- n1+n2 ] *)
  | Sub            (** [ n1 n2   -- n1-n2 ] *)
  | Mul            (** [ n1 n2   -- n1*n2 ] *)
  | Div            (** [ n1 n2   -- n1/n2 ] *)
  | Eq             (** [ v1 v2   -- v1=v2 ] *)
  | IfThenElse     (** [ b c1 c2 -- v     ] *)
and code =
  inst list


(** Pretty printing *)

let rec pp_inst fmt = function
  | Num n -> Format.fprintf fmt "%d" n
  | Bool true -> Format.fprintf fmt "true"
  | Bool false -> Format.fprintf fmt "false"
  | Code [] -> Format.fprintf fmt "[]"
  | Code c -> Format.fprintf fmt "@[[@ %a@ ]@]" pp_code c
  | Neg -> Format.fprintf fmt "neg"
  | Add -> Format.fprintf fmt "add"
  | Sub -> Format.fprintf fmt "sub"
  | Mul -> Format.fprintf fmt "mul"
  | Div -> Format.fprintf fmt "div"
  | Eq -> Format.fprintf fmt "eq"
  | IfThenElse -> Format.fprintf fmt "if"
and pp_code fmt = function
  | [] -> ()
  | inst :: [] -> Format.fprintf fmt "%a" pp_inst inst
  | inst :: code -> Format.fprintf fmt "%a@ %a" pp_inst inst pp_code code


(** Semantics of arithmetic expressions *)
module Semantics = struct

  type value =
    | Num of int
    | Bool of bool
    | Code of code

  type stack =
    value list

  let step : code * stack -> code * stack = function
    | Num n       :: code,                                stack -> code, Num n          :: stack
    | Bool b      :: code,                                stack -> code, Bool b         :: stack
    | Code c      :: code,                                stack -> code, Code c         :: stack
    | Neg         :: code, Num n ::                       stack -> code, Num (-n)       :: stack
    | Add         :: code, Num n2 :: Num n1 ::            stack -> code, Num (n1 + n2)  :: stack
    | Sub         :: code, Num n2 :: Num n1 ::            stack -> code, Num (n1 - n2)  :: stack
    | Mul         :: code, Num n2 :: Num n1 ::            stack -> code, Num (n1 * n2)  :: stack
    | Div         :: code, Num n2 :: Num n1 ::            stack -> code, Num (n1 / n2)  :: stack
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
    | Num n :: stack -> Num n :: quote stack
    | Bool b :: stack -> Bool b :: quote stack
    | Code c :: stack -> Code c :: quote stack

  let normalise t =
    quote (eval t)

end
