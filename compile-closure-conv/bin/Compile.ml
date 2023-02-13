(** {0 Closure conversion} *)

(** Compilation of the {!Core} language to the {!CoreClos} language.

    This transformation converts functions into closures, separating the “code”
    of functions from the captured data. The translation is somewhat fiddly due
    to the decision to use de Bruijn indices in the source and target languages.
    We might want to alpha-rename variables to make this easier.
*)


(** {1 Helper functions} *)

(** Return a bit set of the of the free variables that occur in a term for a
    given environment size *)
let fvs size (tm : Core.tm) : bool list =
  (* Set the free variables in a term, beginning at an initial binding depth *)
  let rec go bs depth : Core.tm -> unit  =
    function
    | Var x when x < depth -> ()              (* bound *)
    | Var x -> Array.set bs (x - depth) true  (* free *)
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

let shift : CoreClos.tm -> CoreClos.tm =
  function
  | Var x -> Var (x + 1)
  | TupleProj (Var x, i) -> TupleProj (Var (x + 1), i)
  | _ -> raise (Invalid_argument "cannot shift term")

let shift_substs =
  List.map (Option.map (fun (tm, ty) -> shift tm, ty))

let lookup substs n =
  Option.get (List.nth substs n)


(** {1 Translation} *)

(** Translation to closure converted types *)
let rec translate_ty : Core.ty -> CoreClos.ty =
  function
  | BoolType -> BoolType
  | IntType -> IntType
  | FunType (param_ty, body_ty) ->
      ClosType (translate_ty param_ty, translate_ty body_ty)

(** Translation to closure converted terms *)
let rec translate substs size : Core.tm -> CoreClos.tm =
  function
  | Var x -> fst (lookup substs x)

  | Let (name, def_ty, def, body) ->
      let def_ty = translate_ty def_ty in
      let def = translate substs size def in
      (* TODO: use levels in substitutions to avoid shifting *)
      let body = translate (Some (Var 0, def_ty) :: shift_substs substs) (size + 1) body in
      CoreClos.Let (name, def_ty, def, body)

  | BoolLit b -> CoreClos.BoolLit b
  | IntLit i -> CoreClos.IntLit i

  | PrimApp (prim, args) ->
      let args = List.map (translate substs size) args in
      PrimApp (prim, args)

  | FunLit (name, param_ty, body) ->
      let param_ty = translate_ty param_ty in

      let arg_var = CoreClos.Var 0 in
      let env_var = CoreClos.Var 1 in

      (* Returns:

        - a mapping from variables to projections on the environment parameter
        - a lists of terms (in reverse order) to be used when constructing the environment
        - a lists of types (in reverse order) to be used in the type of the environment
      *)
      let rec make_env bs x =
        match bs with
        | [] -> [], [], []
        | false :: bs ->
            let env_substs, env_tms, env_tys = make_env bs (x + 1) in
            None :: env_substs, env_tms, env_tys
        | true :: bs ->
            let env_substs, env_tms, env_tys = make_env bs (x + 1) in
            let env_proj = CoreClos.TupleProj (env_var, List.length env_tms) in
            let env_tm, env_ty = lookup substs x in
            Some (env_proj, env_ty) :: env_substs,
            env_tm :: env_tms,
            env_ty :: env_tys
      in

      let body_fvs = List.tl (fvs (size + 1) body) in
      let env_substs, env_tms, env_tys = make_env body_fvs 0 in
      let body = translate (Some (arg_var, param_ty) :: env_substs) (size + 1) body in

      CoreClos.ClosLit
        (CodeLit (TupleType (List.rev env_tys), (name, param_ty), body),
          TupleLit (List.rev env_tms))

  | FunApp (head, arg) ->
      let head = translate substs size head in
      let arg = translate substs size arg in
      CoreClos.ClosApp (head, arg)
