(** {0 Closure conversion} *)

(** Compilation of the {!Core} language to the {!CoreClos} language.

    This transformation converts functions into closures, separating the “code”
    of functions from the captured data. The translation is somewhat fiddly due
    to the decision to use de Bruijn indices in the source and target languages.
    We might want to alpha-rename variables to make this easier.
*)

(* TODO: Straighten out environment/substitution terminology *)


(** {1 Values} *)

(** Simplified values to be used when substituting source terms for target
    terms. We only need to worry about variables and projections on variables,
    as those are the only values that will be stored in the environment during
    compilation.

    Variables are represented as de Bruijn levels which allows us to
    freely add new scopes (weaken the environment) without needing to worry
    about shifting de Bruijn indices.
*)
type vtm =
  | Var of int
  | TupleProj of int * int

(** Quote a closure converted value back into a target environmnet of a given size. *)
let quote size' : vtm -> CoreClos.tm =
  function
  | Var level -> Var (size' - level - 1)
  | TupleProj (level, label) -> TupleProj (Var (size' - level - 1), label)


(** {1 Helper functions} *)

(** Lookup a source variable in the substitutions *)
let lookup substs index =
  Option.get (List.nth substs index)

(** Return a bit set of the of the free variables that occur in a term for a
    given environment size *)
let fvs size (tm : Core.tm) : bool list =
  (* Set the free variables in a term, beginning at an initial binding depth *)
  let rec go bs depth : Core.tm -> unit  =
    function
    | Var index when index < depth -> ()              (* bound *)
    | Var index -> Array.set bs (index - depth) true  (* free *)
    | Let (_, _, def, body) -> go bs depth def; go bs (depth + 1) body
    | BoolLit _ -> ()
    | IntLit _ -> ()
    | PrimApp (_, args) -> List.iter (go bs depth) args
    | FunLit (_, _, body) -> go bs (depth + 1) body
    | FunApp (head, arg) -> go bs depth head; go bs depth arg
  in

  (* Initialise an array to serve as a bitmap over the environment *)
  let bs = Array.make size false in
  (* Update the entries of the environmnet with the free variables in [tm] *)
  go bs 0 tm;
  (* TODO: avoid needing to convert to a list? *)
  Array.to_list bs


(** {1 Translation} *)

(** Translation to closure converted types *)
let rec translate_ty : Core.ty -> CoreClos.ty =
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
let rec translate substs size size' : Core.tm -> CoreClos.tm =
  function
  | Var index ->
      let vtm, _ = lookup substs index in
      quote size' vtm

  | Let (name, def_ty, def, body) ->
      let def_ty = translate_ty def_ty in
      let def = translate substs size size' def in

      let body_substs = Some (Var size', def_ty) :: substs in
      let body = translate body_substs (size + 1) (size' + 1) body in

      CoreClos.Let (name, def_ty, def, body)

  | BoolLit b -> CoreClos.BoolLit b
  | IntLit i -> CoreClos.IntLit i

  | PrimApp (prim, args) ->
      let args = List.map (translate substs size size') args in
      PrimApp (prim, args)

  | FunLit (name, param_ty, body) ->
      let param_ty = translate_ty param_ty in

      (* [fun env x => ...] *)
      let env_level = size' in
      let arg_level = size' + 1 in

      (* Returns:

        - a mapping from variables to projections on the environment parameter
        - a list of terms (in reverse order) to be used when constructing the environment
        - a list of types (in reverse order) to be used in the type of the environment
      *)
      let rec make_env bs index =
        match bs with
        | [] -> [], [], []
        | false :: bs ->
            let env_substs, env_tms, env_tys = make_env bs (index + 1) in
            None :: env_substs, env_tms, env_tys
        | true :: bs ->
            let env_substs, env_tms, env_tys = make_env bs (index + 1) in
            let env_vtm, env_ty = lookup substs index in
            Some (TupleProj (env_level, List.length env_tms), env_ty) :: env_substs,
            quote size' env_vtm :: env_tms, env_ty :: env_tys
      in

      let body_fvs = List.tl (fvs (size + 1) body) in
      let env_substs, env_tms, env_tys = make_env body_fvs 0 in

      (* Translate the body of the function. Note that two variables are being
         added in the target environment: one for the environment, and another
         for the original argument of the function. *)
      let body_substs = Some (Var arg_level, param_ty) :: env_substs in
      let body = translate body_substs (size + 1) (size' + 2) body in

      CoreClos.ClosLit
        (CodeLit (TupleType (List.rev env_tys), (name, param_ty), body),
          TupleLit (List.rev env_tms))

  | FunApp (head, arg) ->
      let head = translate substs size size' head in
      let arg = translate substs size size' arg in
      CoreClos.ClosApp (head, arg)
