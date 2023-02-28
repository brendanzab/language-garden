(** {0 Typed closure conversion on alpha renamed terms}

    Translation from {!Lang.FunA} to {!Lang.ClosA}.

    This translation converts functions into closures, separating the code of
    functions from the data implicitly captured from the surrounding environment.

    Unlike {!Translate.FunToClos}, this translation uses alpha renamed terms,
    which simplifies weakening.
*)

module FunA = Lang.FunA
module ClosA = Lang.ClosA

(** We’ll mainly be mapping source variables to target terms *)
module VarMap = FunA.VarMap
module VarSet = FunA.VarSet


(** {1 Translation} *)

(** Translation to closure converted types *)
let rec translate_ty : FunA.ty -> ClosA.ty =
  function
  | BoolType -> BoolType
  | IntType -> IntType
  | FunType (param_ty, body_ty) ->
      ClosType (translate_ty param_ty, translate_ty body_ty)

(** Translation to closure converted terms. *)
let rec translate env : FunA.tm -> ClosA.tm =
  function
  | Var var ->
      fst (VarMap.find var env)

  | Let (def_var, def_ty, def, body) ->
      (* Create a fresh variable for the definition in the target language,
         and then translate the definition *)
      let def_var' = ClosA.Var.fresh (FunA.Var.name def_var) in
      let def_ty = translate_ty def_ty in
      let def = translate env def in

      (* Translate the body of the let expression with the definition bound *)
      let body_env = VarMap.add def_var (ClosA.Var def_var', def_ty) env in
      let body = translate body_env body in

      Let (def_var', def_ty, def, body)

  | BoolLit b -> BoolLit b
  | IntLit i -> IntLit i

  | PrimApp (prim, args) ->
      let args = List.map (translate env) args in
      PrimApp (prim, args)

  | FunLit (param_var, param_ty, body) ->
      (* A fresh variable to be used for the environment parameter in the code
        of the closure *)
      let env_var' = ClosA.Var.fresh "env" in

      (* Create a fresh variable for the parameter in the target language,
         and then transtlate the parameter *)
      let param_var' = ClosA.Var.fresh (FunA.Var.name param_var) in
      let param_ty = translate_ty param_ty in
      let param_tm = ClosA.Var param_var' in

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
      let body =
        let _, body_env =
          List.fold_left
            (fun (label, body_env) id ->
              let ty = snd (VarMap.find id env) in
              let tm = ClosA.TupleProj (Var env_var', label) in
              label + 1, VarMap.add id (tm, ty) body_env)
            (0, VarMap.singleton param_var (param_tm, param_ty))
            body_fvs
        in
        translate body_env body in

      (* Lists of terms and types to be used when explicitly constructing the
         environment of the closure *)
      let env_tys = List.map (fun id -> snd (VarMap.find id env)) body_fvs in
      let env_tms = List.map (fun id -> fst (VarMap.find id env)) body_fvs in

      (* Construct the closure *)
      ClosLit
        (CodeLit ((env_var', TupleType env_tys), (param_var', param_ty), body),
          TupleLit env_tms)

  | FunApp (head, arg) ->
      let head = translate env head in
      let arg = translate env arg in
      ClosApp (head, arg)
