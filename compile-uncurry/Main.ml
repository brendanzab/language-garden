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

  (** Return the list of parameters of a series of nested function literals,
      along with the body of that expression. *)
  let rec fun_lits : expr -> string list * expr =
    function
    | FunLit (name, body) ->
        let params, body = fun_lits body in
        name :: params, body
    | body -> [], body

  (** Return the head of a series of function applications and a list of the
      arguments that it was applied to. *)
  let rec fun_apps : expr -> expr * expr list =
    function
    | FunApp (head, arg) ->
        let head, args = fun_apps head in
        head, args @ [arg]
    | head -> head, []


  (** Pretty print an expression *)
  let rec pp_expr names fmt =
    let pp_parens ?(wrap = false) names fmt = function
      | (Let _ | FunLit _ | FunApp _) as expr when wrap ->
          Format.fprintf fmt "@[(%a)@]" (pp_expr names) expr
      | expr -> pp_expr names fmt expr
    in
    function
    | Var x -> Format.pp_print_string fmt (List.nth names x)
    | Let (name, def, body) ->
        let pp_name_def names fmt (name, def) =
          Format.fprintf fmt "@[let@ %s@ :=@]@ %a;" name (pp_expr names) def
        and pp_lets names fmt = function
          | Let (_, _, _) as expr -> pp_expr names fmt expr
          | expr -> Format.fprintf fmt "@[%a@]" (pp_expr names) expr
        in
        Format.fprintf fmt "@[<2>%a@]@ %a"
          (pp_name_def names) (name, def)
          (pp_lets (name :: names)) body
    | FunLit (_, _) as expr ->
        let pp_sep fmt () = Format.fprintf fmt "@ " in
        let params, body = fun_lits expr in
        Format.fprintf fmt "@[<2>@[<4>fun@ %a@ :=@]@ @[%a@]@]"
          (Format.pp_print_list ~pp_sep Format.pp_print_string) params
          (pp_expr (List.rev params @ names)) body
    | FunApp (_, _) as expr ->
        let pp_sep fmt () = Format.fprintf fmt "@ " in
        let head, args = fun_apps expr in
        Format.fprintf fmt "@[<2>%a@ %a@]"
          (pp_parens ~wrap:true names) head
          (Format.pp_print_list ~pp_sep (pp_parens ~wrap:true names)) args

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
                   ^ 0, 2
        foo
        ^^^ 0, 0
  *)


  (** Pretty print an expression *)
  let rec pp_expr names fmt = function
    | Var (scope, param) ->
        let param_names = List.nth names scope in
        Format.pp_print_string fmt (List.nth param_names param)
    | Let (name, def, body) ->
        let pp_name_def names fmt (name, def) =
          Format.fprintf fmt "@[let@ %s@ :=@]@ %a;" name (pp_expr names) def
        and pp_lets names fmt = function
          | Let (_, _, _) as expr -> pp_expr names fmt expr
          | expr -> Format.fprintf fmt "@[%a@]" (pp_expr names) expr
        in
        Format.fprintf fmt "@[<2>%a@]@ %a"
          (pp_name_def names) (name, def)
          (pp_lets ([name] :: names)) body
    | FunLit (params, body) ->
        let pp_sep fmt () = Format.fprintf fmt ",@ " in
        Format.fprintf fmt "@[<2>@[fun@ @[(%a)@]@ :=@]@ %a@]"
          (Format.pp_print_list ~pp_sep Format.pp_print_string) params
          (pp_expr (params :: names)) body
    | FunApp (head, args) ->
        let pp_sep fmt () = Format.fprintf fmt ",@ " in
        Format.fprintf fmt "%a(%a)"
          (pp_expr names) head
          (Format.pp_print_list ~pp_sep (pp_expr names)) args

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
        let params, body = Curried.fun_lits expr in
        let env = bind_params env params in
        FunLit (params, translate env body)
    | FunApp _ as expr ->
        let head, args = Curried.fun_apps expr in
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
