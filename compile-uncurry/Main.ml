(** De Bruijn index, counting variables from the most recently bound to the
    least recently bound *)
type index = int

(** De Bruijn level, counting variables from the least recently bound to the
    most recently bound. *)
type level = int

(** Convert a De Bruijn level to a De Bruijn index using the number of bindings
    the environment where the index will be used. *)
let level_to_index size level =
  size - level - 1


(** A core language where all functions take a single argument *)
module Curried = struct

  type expr =
    | Var of index
    | Let of string * expr * expr
    | FunLit of string * expr
    | FunApp of expr * expr

  (** Return the number of parameters of an expression *)
  let rec arity : expr -> int =
    function
    | FunLit (_, body) -> 1 + arity body
    | _ -> 0

  (** Return the list of parameters that an expression expects, along with the
      body of that expression. *)
  let rec uncurry : expr -> string list * expr =
    function
    | FunLit (x, body) ->
        let params, body = uncurry body in
        x :: params, body
    | body -> [], body

  (** Return the arguments that and expression has been applied to. *)
  let rec args : expr -> expr * expr list =
    function
    | FunApp (head, arg) ->
        let head, args = args head in
        head, args @ [arg]
    | head -> head, []

end


(** A language that supports parameter lists *)
module Uncurried = struct

  type expr =
    | Var of index * int
    (*       ^^^^^   ^^^
             |       |
             |       index into the scope’s parameter list
             |
             scope index
    *)
    | Let of string * expr * expr
    | FunLit of string list * expr
    | FunApp of expr * expr list

  (* Variables are representated with a De Bruijn index pointing to the scope
     where the variable was bound and the position of the binder in the scope's
     parameter list. For example:

     fun (a, b, c) :=
        let foo := c;
                   ^ 1, 2
        foo
        ^^^ 0, 0
  *)

end


module CurriedToUncurried = struct

  type binding = {
    var : level * int;  (** The scope level and parameter index of this binding *)
    arity : int;        (** Number of known parameters that this binding accepts *)
                        (* TODO: list of arities *)
  }

  (** An environment maps variables in the core language to bindings in the
      uncurried language, recording known arities. *)
  type env = {
    scopes : level;
    bindings : binding list;
  }

  let lookup_var env x : index * int =
    let { var = scope, param; _ } = (List.nth env.bindings x) in
    (* Convert the scope level to a scope index *)
    let scope = level_to_index env.scopes scope in
    scope, param

  let lookup_arity env : Curried.expr -> int = (* TODO: list of arities *)
    function
    | Var x -> (List.nth env.bindings x).arity
    | FunLit (_, _) as expr -> Curried.arity expr
    | _ -> 0

  (** Add a new scope that binds a single definition with a given arity *)
  let bind_def env arity = {
    scopes = env.scopes + 1;
    bindings = { var = env.scopes, 0; arity } :: env.bindings;
  }

  (** Add a new scope that binds a sequence of parameters *)
  let bind_params env params =
    let rec go param = function
      | [] -> env.bindings
      | _ :: bindings ->
          let bindings = go (param + 1) bindings in
          let var = env.scopes, param in
          { var; arity = 0 } :: bindings
          (*             ^ We might be able to pull the arity from the type
                           of the parameter, if we had types? This would
                           fail in the case of polymorphic types, however.
          *)
    in
    {
      scopes = env.scopes + 1;
      bindings = go 0 params;
    }

  let rec translate env : Curried.expr -> Uncurried.expr =
    function
    | Var x ->
        let scope, param = lookup_var env x in
        Var (scope, param)
    | Let (x, def, body) ->
        let def_arity = lookup_arity env def in
        let def = translate env def in
        let body = translate (bind_def env def_arity) body in
        Let (x, def, body)
    | FunLit (_, _) as expr ->
        let params, body = Curried.uncurry expr in
        let env = bind_params env params in
        FunLit (params, translate env body)
    | FunApp _ as expr ->
        let head, args = Curried.args expr in
        let num_args = List.length args in
        let arity = lookup_arity env head in
        (* TODO: A list of arities would allow us to translate more
           applications, eg: foo(a, b)(c, d, e) *)
        if arity = num_args then
          FunApp (translate env head, List.map (translate env) args)
        else
          failwith (Printf.sprintf "arity mismatch: expected %i arguments, found %i" arity num_args)

end


let () =
  ()
