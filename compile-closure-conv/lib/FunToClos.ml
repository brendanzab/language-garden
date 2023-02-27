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


(** {1 Environment} *)

(** Compilation environment that maps source variables to closure converted
    values and their types. *)
type env = (vtm * ClosLang.ty) option list


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
      let vtm, _ = Option.get (List.nth env index) in
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

      (* Create a mask over the environment that records the free variables used
         in the body of the function. *)
      let body_fvs = FunLang.fvs (size + 1) body in

      (* Prunes the current environment with the bitmask, returning:

        - an environmnent that substitutes source variables that were used in
          the body of the function to projections off the environment parameter
        - a list of terms (in reverse order) to be used when constructing the environment
        - a list of types (in reverse order) to be used in the type of the environment
      *)
      let rec make_env env index =
        match env with
        | [] -> [], [], []
        | binding :: env ->
            let env', tms, tys = make_env env (index + 1) in
            (* Check if the current binding was used in the body *)
            if body_fvs.(size - index - 1) then
              (* This binding was used in the body of the function, so add
                it to the environment tuple and map any occurrences in the body
                of the closure to projections off the environment parameter. *)
              let vtm, ty = Option.get binding in
              Some (TupleProj (env_level, List.length tms), ty) :: env',
              quote size' vtm :: tms,
              ty :: tys
            else
              (* This binding was not used in the body of the function, so
                encountering it in the body of the closure is a bug. *)
              None :: env', tms, tys
      in

      (* Construct the environments *)
      let proj_env, env_tms, env_tys = make_env env 0 in

      (* Translate the body of the function *)
      let body_env = Some (Var param_level, param_ty) :: proj_env in
      let body = translate body_env (size + 1) 2 body in

      ClosLit
        (CodeLit (TupleType (List.rev env_tys), (name, param_ty), body),
          TupleLit (List.rev env_tms))

  | FunApp (head, arg) ->
      let head = translate env size size' head in
      let arg = translate env size size' arg in
      ClosApp (head, arg)
