(** Split the list at a given index  *)
let split_at (type a) (n : int) (xs : a list) : (a list * a list) option =
  if n < 0 then invalid_arg "split_at" else
  let rec go n xs =
    match n, xs with
    | 0, xs -> Some ([], xs)
    | n, x :: xs ->
        go (n - 1) xs |> Option.map (fun (xs, ys) -> x :: xs, ys)
    | _, [] -> None
  in
  go n xs


(** These names are used as hints for pretty printing binders and variables. *)
type name = string

(** De Bruijn index, counting variables from the most recently bound to the
    least recently bound. *)
type index = int

(** De Bruijn level, counting variables from the least recently bound to the
    most recently bound. *)
type level = int

(** Convert a De Bruijn level to a De Bruijn index using the number of bindings
    the environment where the index will be used. *)
let level_to_index size level =
  size - level - 1


(** A language where all functions take a single argument *)
module Curried = struct

  type expr =
    | Var of index
    | Let of name * expr * expr
    | Fun_lit of name * expr
    | Fun_app of expr * expr


  (** Return the list of parameters of a series of nested function literals,
      along with the body of that expression. *)
  let rec fun_lits : expr -> name list * expr =
    function
    | Fun_lit (name, body) ->
        let params, body = fun_lits body in
        name :: params, body
    | body -> [], body

  (** Return the head of a series of function applications and a list of the
      arguments that it was applied to. *)
  let rec fun_apps : expr -> expr * expr list =
    function
    | Fun_app (head, arg) ->
        let head, args = fun_apps head in
        head, args @ [arg]
    | head -> head, []


  (** Pretty print an expression *)
  let rec pp_expr names fmt =
    let pp_parens ?(wrap = false) names fmt = function
      | (Let _ | Fun_lit _ | Fun_app _) as expr when wrap ->
          Format.fprintf fmt "@[(%a)@]" (pp_expr names) expr
      | expr -> pp_expr names fmt expr
    in
    function
    | Var index ->
        Format.pp_print_string fmt (List.nth names index)
    | Let (name, def, body) ->
        let pp_name_def names fmt (name, def) =
          Format.fprintf fmt "@[let@ %s@ :=@]@ @[%a@];" name (pp_expr names) def
        and pp_lets names fmt = function
          | Let (_, _, _) as expr -> pp_expr names fmt expr
          | expr -> Format.fprintf fmt "@[%a@]" (pp_expr names) expr
        in
        Format.fprintf fmt "@[<2>%a@]@ %a"
          (pp_name_def names) (name, def)
          (pp_lets (name :: names)) body
    | Fun_lit (_, _) as expr ->
        let pp_sep fmt () = Format.fprintf fmt "@ " in
        let params, body = fun_lits expr in
        Format.fprintf fmt "@[<2>@[<4>fun@ %a@ :=@]@ @[%a@]@]"
          (Format.pp_print_list ~pp_sep Format.pp_print_string) params
          (pp_expr (List.rev params @ names)) body
    | Fun_app (_, _) as expr ->
        let pp_sep fmt () = Format.fprintf fmt "@ " in
        let head, args = fun_apps expr in
        Format.fprintf fmt "@[<2>%a@ %a@]"
          (pp_parens ~wrap:true names) head
          (Format.pp_print_list ~pp_sep (pp_parens ~wrap:true names)) args


  (** Tree-walking interpreter, implemented using normalisation-by-evaluation *)
  module Semantics = struct

    (** Partially evaluated expressions *)
    type value =
      | Neu of neu
      | Fun_lit of name * (value -> value)
    and neu =
      | Var of level
      | Fun_app of neu * value

    type env = value list


    (** Compute a function application *)
    let app head arg =
      match head with
      | Neu neu -> Neu (Fun_app (neu, arg))
      | Fun_lit (_, body) -> body arg

    (** Evaluate a expression from the syntax into its semantic interpretation *)
    let rec eval env = function
      | Let (_, def, body) -> eval (eval env def :: env) body
      | Var index -> List.nth env index
      | Fun_lit (name, body) -> Fun_lit (name, fun x -> eval (x :: env) body)
      | Fun_app (head, arg) -> app (eval env head) (eval env arg)


    (** Quote a value back to the syntax, evaluating under binders. *)
    let rec quote size : value -> expr =
      function
      | Neu neu -> quote_neu size neu
      | Fun_lit (name, body) ->
          let var = Neu (Var size) in
          Fun_lit (name, quote (size + 1) (body var))
    and quote_neu size : neu -> expr =
      function
      | Var level -> Var (level_to_index size level)
      | Fun_app (neu, arg) -> Fun_app (quote_neu size neu, quote size arg)


    (** Reduce an expression as much as possible in the current environment. *)
    let normalise size env expr : expr =
      quote size (eval env expr)

  end

end


(** A language with multi-parameter functions *)
module Uncurried = struct

  type expr =
    | Var of index * int
    (*       ^^^^^   ^^^
             |       |
             |       index into the scope’s parameter list
             |
             scope index
    *)
    | Let of name * expr * expr
    | Fun_lit of name list * expr
    | Fun_app of expr * expr list

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
    | Var (index, param) ->
        Format.pp_print_string fmt (List.nth (List.nth names index) param)
    | Let (name, def, body) ->
        let pp_name_def names fmt (name, def) =
          Format.fprintf fmt "@[let@ %s@ :=@]@ @[%a@];" name (pp_expr names) def
        and pp_lets names fmt = function
          | Let (_, _, _) as expr -> pp_expr names fmt expr
          | expr -> Format.fprintf fmt "@[%a@]" (pp_expr names) expr
        in
        Format.fprintf fmt "@[<2>%a@]@ %a"
          (pp_name_def names) (name, def)
          (pp_lets ([name] :: names)) body
    | Fun_lit (params, body) ->
        let pp_sep fmt () = Format.fprintf fmt ",@ " in
        Format.fprintf fmt "@[<2>@[fun@ @[(%a)@]@ :=@]@ @[%a@]@]"
          (Format.pp_print_list ~pp_sep Format.pp_print_string) params
          (pp_expr (params :: names)) body
    | Fun_app (head, args) ->
        let pp_sep fmt () = Format.fprintf fmt ",@ " in
        Format.fprintf fmt "%a(%a)"
          (pp_expr names) head
          (Format.pp_print_list ~pp_sep (pp_expr names)) args


  (** Tree-walking interpreter, implemented using normalisation-by-evaluation *)
  module Semantics = struct

    (** Partially evaluated expressions *)
    type value =
      | Neu of neu
      | Fun_lit of name list * (value list -> value)
    and neu =
      | Var of level * int
      | Fun_app of neu * value list

    type env = (value list) list


    (** Compute a function application *)
    let app head args =
      match head with
      | Neu neu -> Neu (Fun_app (neu, args))
      | Fun_lit (_, body) -> body args

    (** Evaluate a expression from the syntax into its semantic interpretation *)
    let rec eval env : expr -> value =
      function
      | Let (_, def, body) -> eval ([eval env def] :: env) body
      | Var (index, param) -> List.nth (List.nth env index) param
      | Fun_lit (names, body) -> Fun_lit (names, fun params -> eval (params :: env) body)
      | Fun_app (head, args) -> app (eval env head) (List.map (eval env) args)


    (** Quote a value back to the syntax, evaluating under binders. *)
    let rec quote size : value -> expr =
      function
      | Neu neu -> quote_neu size neu
      | Fun_lit (names, body) ->
          let vars = List.mapi (fun param _ -> Neu (Var (size, param))) names in
          Fun_lit (names, quote (size + 1) (body vars))
    and quote_neu size : neu -> expr =
      function
      | Var (level, param) -> Var (level_to_index size level, param)
      | Fun_app (neu, args) -> Fun_app (quote_neu size neu, List.map (quote size) args)


    (** Reduce an expression as much as possible in the current environment. *)
    let normalise size env expr : expr =
      quote size (eval env expr)

  end

end


module Curried_to_uncurried = struct

  type binding = {
    var : level * int;    (** The scope level and parameter index of this binding *)
    arities : int list;   (** A list of arities this binding accepts *)
  }

  (* NOTE: We might be able to replace the lists of arities with uncurried
     types, which might end up being more precise. *)

  (** An environment that maps variable indexes in the core language to
      variables in the uncurried language. *)
  type env = {
    size : level;               (** The number of scopes that have been bound *)
    bindings : binding list;
  }

  (** The size field lets us convert the scope levels to scope indicies when
      translating variables.

      Note that the size of the environment does not neccessarily match the
      number of bindings in the envrionment, as multiple bindings can be
      introduced per scope in the uncurried language.
  *)

  (** An empty environment with no bindings *)
  let empty_env = {
    size = 0;
    bindings = [];
  }

  (** Add a new scope that binds a single definition with a given arity *)
  let bind_def env arities = {
    size = env.size + 1;
    bindings = { var = env.size, 0; arities } :: env.bindings;
  }

  (** Add a new scope that binds a sequence of parameters *)
  let bind_params env params =
    (* Add the bindings the environment, mapping each parameter to positions in
       a single parameter list. *)
    let rec go param bindings = function
      | [] -> bindings
      | _ :: params ->
          let var = env.size, param in
          go (param + 1) ({ var; arities = [] } :: bindings) params
          (*                               ^^ We might be able to pull the arity from the type
                                              of the parameter, if we had types? This would
                                              fail in the case of polymorphic types, however.
          *)
    in
    {
      (* We’re only adding a single scope, so we only need to increment the size
         of the environment once. *)
      size = env.size + 1;
      bindings = go 0 env.bindings params;
    }

  (** Translate a curried expression into an uncurried expression *)
  let rec translate env : Curried.expr -> Uncurried.expr * int list =
    function
    | Curried.Var index ->
        let { var = level, param; arities } = List.nth env.bindings index in
        let index = level_to_index env.size level in
        Uncurried.Var (index, param), arities

    | Curried.Let (name, def, body) ->
        let def, def_arities = translate env def in
        let body, body_arities = translate (bind_def env def_arities) body in
        Uncurried.Let (name, def, body), body_arities

    | Curried.Fun_lit (_, _) as expr ->
        let params, body = Curried.fun_lits expr in
        let body, body_arities = translate (bind_params env params) body in
        Uncurried.Fun_lit (params, body), List.length params :: body_arities

    (* Translate function applications to multiple argument lists,
        eg. [f a b c d e] to [f(a, b)(c)(d, e)]. *)
    | Curried.Fun_app _ as expr ->
        let rec go (head, arities) args =
          match arities, args with
          | arities, [] ->
              (* FIXME: we can lose track of arities here, eg. [(id always) id]
                 fails as an over-application because [id always] ‘forgets’ the
                 arity of [always]. Types could possibly help here? *)
              head, arities
          | [], _ ->
              (* I think eval/apply would introduce a call to [apply_n] here? *)
              (* See: https://www.cse.chalmers.se/edu/year/2011/course/CompFun/lecture2.pdf#page=29 *)
              failwith "error: over-application"
          | arity :: arities, args ->
              begin match split_at arity args with
              | Some ([], args) -> go (head, arities) args
              | Some (args, args') ->
                  (* NOTE: throwing out the arities of the arguments here! *)
                  let args = List.map (fun arg -> fst (translate env arg)) args in
                  go (Uncurried.Fun_app (head, args), arities) args'
              (* Could we wrap with a function literal here? *)
              (* I think eval/apply would introduce a closure here? *)
              | None -> failwith "error: under-application"
              end
        in
        let head, args = Curried.fun_apps expr in
        go (translate env head) args

end


let () =
  Printexc.record_backtrace true;

  (* TODO: Parser and proper tests *)

  let term = Curried.(
    Fun_lit ("a", Fun_lit ("b", Fun_lit ("c",
      Let ("a'", Var 2,
      Let ("b'", Var 2,
      Let ("c'", Var 2,
      Let ("foo", Var 3,
      Var 3)))))))
  ) in

  Format.printf "@[%a@]\n" (Curried.pp_expr []) term;

  let term, _ = Curried_to_uncurried.(translate empty_env term) in

  Format.printf "@[%a@]\n" (Uncurried.pp_expr []) term;
  Format.printf "\n";

  let term = Curried.(
    Let ("id", Fun_lit ("a", Var 0),
    Let ("always", Fun_lit ("a", Fun_lit ("b", Var 1)),
    (* Fun_app (Fun_app (Fun_app (Fun_app (Var 1, Var 0), Var 1), Var 0), Var 1))) *)
    (* Fun_app (Fun_app (Fun_app (Var 1, Var 0), Var 1), Var 0))) *)
    Fun_app (Fun_app (Var 0, Var 1), Var 0)))
    (* Fun_app (Fun_app (Fun_app (Var 0, Var 1), Var 0), Var 1))) *)
    (* Fun_app (Var 1, Var 0))) *)

  ) in

  Format.printf "@[%a@]\n" (Curried.pp_expr []) term;

  let term, _ = Curried_to_uncurried.(translate empty_env term) in

  Format.printf "@[%a@]\n" (Uncurried.pp_expr []) term;

  ()
