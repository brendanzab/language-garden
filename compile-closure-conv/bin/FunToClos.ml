(** {0 Closure conversion}

    Compilation from {!FunLang} to {!ClosLang}.

    This translation converts functions into closures, separating the code of
    functions from the data implicitly captured from the surronding environment.
    It’s a modified version of the algorithm described in {{:https://doi.org/10.1145/1291201.1291212}
    “A type-preserving closure conversion in haskell”}, using de Bruijn levels
    to simplify the handling of variable bindings.

    The implementation is somewhat fiddly due to the decision to use de Bruijn
    indices in the source and target languages. We might want to try
    alpha-renaming variables to make this easier.
*)


(** {1 Values} *)

(** Values used when substituting source terms for closure converted target
    terms. We only need to worry about variables and projections on variables,
    as those are the only values that will be stored in the environment during
    compilation.

    Variables are represented using de Bruijn levels, allowing us to freely
    weaken the target environment (i.e. add new bindings) without needing to
    worry about shifting de Bruijn indices.
*)
type vtm =
  | Var of int
  | TupleProj of int * int

(** Quote a value back into a target environment. *)
let quote size' : vtm -> ClosLang.tm =
  function
  | Var level -> Var (size' - level - 1)
  | TupleProj (level, label) -> TupleProj (Var (size' - level - 1), label)


(** {1 Helper functions} *)

(** Maps a source variable to a closure converted value in the given
    environment. Raises an exception if no substition was defined. *)
let lookup env index : vtm * ClosLang.ty =
  Option.get (List.nth env index)

(** Return a bitmask of the part of an environment that is used in a term *)
let fvs size (tm : FunLang.tm) : bool array =
  (* Traverse a term, recording any free variables in the supplied bitmask. *)
  let rec go mask offset : FunLang.tm -> unit  =
    function
    | Var index when index < offset -> ()
    | Var index -> Array.set mask (size + offset - index - 1) true
    | Let (_, _, def, body) -> go mask offset def; go mask (offset + 1) body
    | BoolLit _ -> ()
    | IntLit _ -> ()
    | PrimApp (_, args) -> List.iter (go mask offset) args
    | FunLit (_, _, body) -> go mask (offset + 1) body
    | FunApp (head, arg) -> go mask offset head; go mask offset arg
  in

  (* Initialise an array to serve as a bitmask over the environment *)
  let mask = Array.make size false in
  (* Update the bitmask with the free variables *)
  go mask 0 tm;

  mask


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

      Let (name, def_ty, def, body)

  | BoolLit b -> BoolLit b
  | IntLit i -> IntLit i

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
      let rec make_env env mask index =
        match env with
        | [] -> [], [], []
        | binding :: env ->
            let proj_env, env_tms, env_tys = make_env env mask (index + 1) in
            (* Check if the current binding was used *)
            if mask.(size - index - 1) then
              (* This binding was used in the body of the function, so add
                it to the environment tuple and map any occurrences in the body
                of the closure to projections off the environment parameter. *)
              let env_vtm, env_ty = Option.get binding in
              Some (TupleProj (env_level, List.length env_tms), env_ty) :: proj_env,
              quote size' env_vtm :: env_tms,
              env_ty :: env_tys
            else
              (* This binding was not used in the body of the function, so
                encountering it in the body of the closure is a bug. *)
              None :: proj_env, env_tms, env_tys
      in

      let mask = fvs (size + 1) body in
      let proj_env, env_tms, env_tys = make_env env mask 0 in

      (* Translate the body of the function, assuming only the environment
         parameter and original parameter in the target environment. *)
      let body_env = Some (Var param_level, param_ty) :: proj_env in
      let body = translate body_env (size + 1) 2 body in

      ClosLit
        (CodeLit (TupleType (List.rev env_tys), (name, param_ty), body),
          TupleLit (List.rev env_tms))

  | FunApp (head, arg) ->
      let head = translate env size size' head in
      let arg = translate env size size' arg in
      ClosApp (head, arg)
