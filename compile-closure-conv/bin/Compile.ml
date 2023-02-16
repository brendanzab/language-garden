(** {0 Closure conversion} *)

(** Compilation from {!FunLang} to {!ClosLang}.

    This translation converts functions into closures, separating the code of
    functions from the data implicitly captured from the surronding environment.

    This translation is made somewhat fiddly due to the decision to use de
    Bruijn indices in the source and target languages. We might want to try
    alpha-renaming variables to make this easier.
*)


(** {1 Values} *)

(** Values used when substituting source terms for target terms. We only need to
    worry about variables and projections on variables, as those are the only
    values that will be stored in the environment during compilation.

    Variables are represented as de Bruijn levels which allows us to freely
    weaken the target environment (add new bindings) without needing to worry
    about shifting de Bruijn indices.
*)
type vtm =
  | Var of int
  | TupleProj of int * int

(** Quote a closure converted value back into a target environment of a given size. *)
let quote size' : vtm -> ClosLang.tm =
  function
  | Var level -> Var (size' - level - 1)
  | TupleProj (level, label) -> TupleProj (Var (size' - level - 1), label)


(** {1 Helper functions} *)

(** Lookup a source variable in the environment *)
let lookup env index : vtm * ClosLang.ty =
  Option.get (List.nth env index)

(** Return a bitmask of the free variables that occur in a term *)
let fvs size (tm : FunLang.tm) : bool list =
  (* Traverse a term, recording any free variables in the supplied bitmask. *)
  let rec go bs offset : FunLang.tm -> unit  =
    function
    | Var index when index < offset -> ()              (* bound *)
    | Var index -> Array.set bs (index - offset) true  (* free *)
    | Let (_, _, def, body) -> go bs offset def; go bs (offset + 1) body
    | BoolLit _ -> ()
    | IntLit _ -> ()
    | PrimApp (_, args) -> List.iter (go bs offset) args
    | FunLit (_, _, body) -> go bs (offset + 1) body
    | FunApp (head, arg) -> go bs offset head; go bs offset arg
  in

  (* Initialise an array to serve as a bitmask over the environment *)
  let bs = Array.make size false in
  (* Update the bitmask with the free variables *)
  go bs 0 tm;
  (* TODO: avoid needing to convert to a list? *)
  Array.to_list bs


(** {1 Translation} *)

(** Translation to closure converted types *)
let rec translate_ty : FunLang.ty -> ClosLang.ty =
  function
  | BoolType -> BoolType
  | IntType -> IntType
  | FunType (param_ty, body_ty) ->
      ClosType (translate_ty param_ty, translate_ty body_ty)

(** Translation to closure converted terms.

    Two size parameters are supplied:

    - [size]: the size of the source environment, used for computing free variables
    - [size']: the size of the target environment, used for quoting values into
      closure converted terms
*)
let rec translate env size size' : FunLang.tm -> ClosLang.tm =
  function
  | Var index ->
      let vtm, _ = lookup env index in
      quote size' vtm

  | Let (name, def_ty, def, body) ->
      let def_ty = translate_ty def_ty in
      let def = translate env size size' def in

      (* Compile the body of the let expression, adding a binding to the source
        and target environments respectively. *)
      let body_env = Some (Var size', def_ty) :: env in
      let body = translate body_env (size + 1) (size' + 1) body in

      ClosLang.Let (name, def_ty, def, body)

  | BoolLit b -> ClosLang.BoolLit b
  | IntLit i -> ClosLang.IntLit i

  | PrimApp (prim, args) ->
      let args = List.map (translate env size size') args in
      PrimApp (prim, args)

  | FunLit (name, param_ty, body) ->
      let param_ty = translate_ty param_ty in

      (* There are only two variables bound in the environment of the compiled
         code literal: one for the environment parameter, and another for the
         original parameter of the function. *)
      let env_level = 0 in
      let param_level = 1 in

      (* Returns:

        - a mapping from source variables to projections on the environment parameter
        - a list of terms (in reverse order) to be used when constructing the environment
        - a list of types (in reverse order) to be used in the type of the environment
      *)
      let rec make_env bs index =
        match bs with
        | [] -> [], [], []
        | false :: bs ->
            let proj_env, env_tms, env_tys = make_env bs (index + 1) in
            (* This variable is not mentioned in the body of the function, so
              encountering it in the body of the closure is a bug. *)
            None :: proj_env, env_tms, env_tys
        | true :: bs ->
            let proj_env, env_tms, env_tys = make_env bs (index + 1) in
            let env_vtm, env_ty = lookup env index in
            (* This variable is mentioned in the body of the function, so add it
              to the environment tuple and map any occurrences in the body of
              the closure to projections off the environment parameter. *)
            Some (TupleProj (env_level, List.length env_tms), env_ty) :: proj_env,
            quote size' env_vtm :: env_tms,
            env_ty :: env_tys
      in

      let body_fvs = List.tl (fvs (size + 1) body) in
      let proj_env, env_tms, env_tys = make_env body_fvs 0 in

      (* Translate the body of the function, assuming only the environment
         parameter and original parameter in the target environment. *)
      let body_env = Some (Var param_level, param_ty) :: proj_env in
      let body = translate body_env (size + 1) 2 body in

      ClosLang.ClosLit
        (CodeLit (TupleType (List.rev env_tys), (name, param_ty), body),
          TupleLit (List.rev env_tms))

  | FunApp (head, arg) ->
      let head = translate env size size' head in
      let arg = translate env size size' arg in
      ClosLang.ClosApp (head, arg)
