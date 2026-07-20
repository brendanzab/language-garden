(** {0 A stack machine for arithmetic expressions}

    This represents arithmetic expressions as a
    {{: https://en.wikipedia.org/wiki/Stack_machine} stack machine}.
*)


(** {1 Stack machine syntax} *)

(** Stack machine instructions *)
type inst =
  (* Environment related instructions from the SECD machine. For additional
     details see: https://xavierleroy.org/mpri/2-4/machines.pdf. *)
  | Access of int  (** Push the nth value in the environment onto the stack *)
  | Begin_let      (** Pop a value off the stack and push it onto the environment *)
  | End_let        (** Discard the first entry of the environment *)

  (* Literals *)
  | Int of int     (** [         -- i     ] *)
  | Bool of bool   (** [         -- b     ] *)
  | Code of code   (** [         -- c     ] *)

  (* Operators *)
  | Neg            (** [ i       -- -n    ] *)
  | Add            (** [ i1 i2   -- i1+i2 ] *)
  | Sub            (** [ i1 i2   -- i1-i2 ] *)
  | Mul            (** [ i1 i2   -- i1*i2 ] *)
  | Div            (** [ i1 i2   -- i1/i2 ] *)
  | Eq             (** [ v1 v2   -- v1=v2 ] *)
  | If_then_else   (** [ b c1 c2 -- v     ] *)

(** Instruction sequences *)
and code =
  inst list


(** {1 Pretty printing} *)

let rec pp_inst inst =
  match inst with
  | Int i -> Format.dprintf "int %i" i
  | Bool true -> Format.dprintf "true"
  | Bool false -> Format.dprintf "false"
  | Code [] -> Format.dprintf "[]"
  | Code c -> Format.dprintf "code @[[@ %t@ ]@]" (pp_code c)
  | Neg -> Format.dprintf "neg"
  | Add -> Format.dprintf "add"
  | Sub -> Format.dprintf "sub"
  | Mul -> Format.dprintf "mul"
  | Div -> Format.dprintf "div"
  | Eq -> Format.dprintf "eq"
  | If_then_else -> Format.dprintf "if"
  | Access n -> Format.dprintf "access %i" n
  | Begin_let -> Format.dprintf "begin-let"
  | End_let -> Format.dprintf "end-let"
and pp_code code =
  match code with
  | [] -> Format.dprintf ""
  | inst :: [] -> Format.dprintf "%t;" (pp_inst inst)
  | inst :: code -> Format.dprintf "%t;@ %t" (pp_inst inst) (pp_code code)


(** Semantics of arithmetic expressions *)
module Semantics = struct

  type value =
    | Int of int
    | Bool of bool
    | Code of code

  (** Environment of variable bindings *)
  type env = value list

  (** A stack of values *)
  type stack = value list

  (* The state of the stack machine *)
  type state = code * env * stack

  let step : state -> state = function
    | Access n :: code,     env,      stack                                -> code,       env,      List.nth env n :: stack
    | Begin_let :: code,    env,      v :: stack                           -> code,       v :: env, stack
    | End_let :: code,      _ :: env, stack                                -> code,       env,      stack

    | Int i :: code,        env,      stack                                -> code,       env,      Int i :: stack
    | Bool b :: code,       env,      stack                                -> code,       env,      Bool b :: stack
    | Code c :: code,       env,      stack                                -> code,       env,      Code c :: stack

    | Neg :: code,          env,      Int i :: stack                       -> code,       env,      Int (-i) :: stack
    | Add :: code,          env,      Int i2 :: Int i1 :: stack            -> code,       env,      Int (i1 + i2) :: stack
    | Sub :: code,          env,      Int i2 :: Int i1 :: stack            -> code,       env,      Int (i1 - i2) :: stack
    | Mul :: code,          env,      Int i2 :: Int i1 :: stack            -> code,       env,      Int (i1 * i2) :: stack
    | Div :: code,          env,      Int i2 :: Int i1 :: stack            -> code,       env,      Int (i1 / i2) :: stack
    | Eq :: code,           env,      v2 :: v1 :: stack                    -> code,       env,      Bool (v1 = v2) :: stack
    | If_then_else :: code, env,      _ :: Code c1 :: Bool true  ::  stack -> c1 @ code,  env,      stack
    | If_then_else :: code, env,      Code c2 :: _ :: Bool false ::  stack -> c2 @ code,  env,      stack

    | _, _, _ -> failwith "invalid code"

  let rec eval : state -> stack = function
    | [], _, stack -> stack
    | code, env, stack ->
        (eval [@tailcall]) (step (code, env, stack))

  let [@tail_mod_cons] rec quote : stack -> code = function
    | [] -> []
    | Int i :: stack -> Int i :: quote stack
    | Bool b :: stack -> Bool b :: quote stack
    | Code c :: stack -> Code c :: quote stack

  let normalise t =
    quote (eval t)

end
