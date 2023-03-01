(** {0 Typed lambda lifting on alpha renamed terms}

    Translation from {!Lang.FunA} to {!Lang.LiftedA}.

    This translation lifts the code of function literals to top-level
    definitions, calling the code immediately if possible. This cuts down on
    the number of closures being constructed, compared to {!Translation.FunAToClosA}.
    We could further reduce the number of closures even more by uncurrying
    functions during this translation.
*)

module FunA = Lang.FunA
module LiftedA = Lang.LiftedA

module VarMap = FunA.VarMap
module VarSet = FunA.VarSet


(** {1 Translation} *)

(** Translation to closure converted types *)
let rec translate_ty : FunA.ty -> LiftedA.ty =
  function
  | BoolType -> BoolType
  | IntType -> IntType
  | FunType (param_ty, body_ty) ->
      ClosType (translate_ty param_ty, translate_ty body_ty)

(** Translation to lambda lifted terms. The [name] parameter is used when
    naming global definitions, to aid with debugging translated terms. *)
let rec translate globals locals ?name : FunA.tm -> LiftedA.globals * LiftedA.tm =
  function
  | Var var ->
      globals, fst (VarMap.find var locals)

  | Let (def_var, def_ty, def, body) ->
      let def_name = FunA.Var.name def_var in
      let def_ty = translate_ty def_ty in

      (* Function literals will be lifted to the top level, so there’s no need to
        compile them to intermediate let bindings. *)
      begin match translate globals locals ~name:def_name def with
      | globals, (ClosLit (_, _) as def) ->
          let body_env = VarMap.add def_var (def, def_ty) locals in
          translate globals body_env body
      | globals, def ->
          let def_var' = LiftedA.LocalVar.fresh def_name in
          let body_env = VarMap.add def_var (LiftedA.LocalVar def_var', def_ty) locals in
          let globals, body = translate globals body_env body in
          globals, Let (def_var', def_ty, def, body)
      end

  | BoolLit b -> globals, BoolLit b
  | IntLit i -> globals, IntLit i

  | PrimApp (prim, args) ->
      let globals, args =
        List.fold_left_map (fun globals arg -> translate globals locals arg)
          globals
          args
      in
      globals, PrimApp (prim, args)

  | FunLit (param_var, param_ty, body) ->
      (* A fresh variable to be used for the environment parameter in the code
        of the closure *)
      let env_var' = LiftedA.LocalVar.fresh "env" in

      (* Create a fresh variable for the parameter in the target language,
        and then transtlate the parameter *)
      let param_var' = LiftedA.LocalVar.fresh (FunA.Var.name param_var) in
      let param_ty = translate_ty param_ty in
      let param_tm = LiftedA.LocalVar param_var' in

      (* Construct the list of variable ocurrences captured by the body of the
        function from the surrounding environment *)
      let body_fvs =
        FunA.fvs body
        |> VarSet.remove param_var
        |> VarSet.elements
      in

      (* Translate the body of the closure’s code in an environment where
        captured variable ocurrences are mapped to explicit projections off the
        environment parameter. *)
      let globals, body =
        let _, body_env =
          List.fold_left
            (fun (label, body_env) id ->
              let ty = snd (VarMap.find id locals) in
              let tm = LiftedA.TupleProj (LocalVar env_var', label) in
              label + 1, VarMap.add id (tm, ty) body_env)
            (0, VarMap.singleton param_var (param_tm, param_ty))
            body_fvs
        in
        translate globals body_env body in

      (* Lists of terms and types to be used when explicitly constructing the
        environment of the closure *)
      let env_tys = List.map (fun id -> snd (VarMap.find id locals)) body_fvs in
      let env_tms = List.map (fun id -> fst (VarMap.find id locals)) body_fvs in

      (* Create the lifted code *)
      let lifted_name = Option.value name ~default:"anon" in
      let lifted_var = LiftedA.GlobalVar.fresh lifted_name in
      let lifted_code = LiftedA.{
        env = env_var', TupleType env_tys;
        param = param_var', param_ty;
        body;
      } in

      (* Add the lifted function to the global and construct a closure *)
      (lifted_var, lifted_code) :: globals,
      ClosLit (lifted_var, TupleLit env_tms)

  | FunApp (head, arg) ->
      let globals, head = translate globals locals head in
      let globals, arg = translate globals locals arg in

      (* If we are applying an argument directly to a closure we can skip
         constructing one - otherwise we assume it’s a closure. *)
      begin match head with
      | ClosLit (code, env) -> globals, CodeApp (code, env, arg)
      | head -> globals, ClosApp (head, arg)
      end

(** Translation to lambda lifted terms. *)
let translate (tm : FunA.tm) : LiftedA.lifted_tm =
  let globals, main = translate [] FunA.VarMap.empty tm in
  { globals; main }
