(** {0 Typed closure conversion on alpha renamed terms}

    Translation from {!Lang.Fun_a} to {!Lang.Clos_a}.

    This translation converts functions into closures, separating the code of
    functions from the data implicitly captured from the surrounding environment.

    Unlike {!Translate.Fun_to_clos}, this translation uses alpha renamed terms,
    which simplifies weakening.
*)

module Fun_a = Lang.Fun_a
module Clos_a = Lang.Clos_a

module Var_map = Fun_a.Var_map
module Var_set = Fun_a.Var_set


(** {1 Translation} *)

(** Translation to closure converted types *)
let rec translate_ty : Fun_a.ty -> Clos_a.ty =
  function
  | Bool_type -> Bool_type
  | Int_type -> Int_type
  | Fun_type (param_ty, body_ty) ->
      Clos_type (translate_ty param_ty, translate_ty body_ty)

(** Translation to closure converted terms. *)
let rec translate env : Fun_a.tm -> Clos_a.tm =
  function
  | Var var ->
      fst (Var_map.find var env)

  | Let (def_var, def_ty, def, body) ->
      (* Create a fresh variable for the definition in the target language,
         and then translate the definition *)
      let def_var' = Clos_a.Var.fresh (Fun_a.Var.name def_var) in
      let def_ty = translate_ty def_ty in
      let def = translate env def in

      (* Translate the body of the let expression with the definition bound *)
      let body_env = Var_map.add def_var (Clos_a.Var def_var', def_ty) env in
      let body = translate body_env body in

      Let (def_var', def_ty, def, body)

  | Bool_lit b -> Bool_lit b
  | Int_lit i -> Int_lit i

  | Prim_app (prim, args) ->
      let args = List.map (translate env) args in
      Prim_app (prim, args)

  | Fun_lit (param_var, param_ty, body) ->
      (* A fresh variable to be used for the environment parameter in the code
        of the closure *)
      let env_var' = Clos_a.Var.fresh "env" in

      (* Create a fresh variable for the parameter in the target language,
         and then transtlate the parameter *)
      let param_var' = Clos_a.Var.fresh (Fun_a.Var.name param_var) in
      let param_ty = translate_ty param_ty in
      let param_tm = Clos_a.Var param_var' in

      (* Construct the list of variable ocurrences captured by the body of the
         function from the surrounding environment *)
      let body_fvs =
        Fun_a.fvs body
        |> Var_set.remove param_var
        |> Var_set.elements
      in

      (* Translate the body of the closure’s code in an environment where
         captured variable ocurrences are mapped to explicit projections off the
         environment parameter. *)
      let body =
        let ~label:_, ~body_env =
          ListLabels.fold_left body_fvs
            ~init:(~label:0, ~body_env:(Var_map.singleton param_var (param_tm, param_ty)))
            ~f:(fun (~label, ~body_env) id ->
              let ty = snd (Var_map.find id env) in
              let tm = Clos_a.Tuple_proj (Var env_var', label) in
              ~label:(label + 1), ~body_env:(Var_map.add id (tm, ty) body_env))
        in
        translate body_env body in

      (* Lists of terms and types to be used when explicitly constructing the
         environment of the closure *)
      let env_tys = List.map (fun id -> snd (Var_map.find id env)) body_fvs in
      let env_tms = List.map (fun id -> fst (Var_map.find id env)) body_fvs in

      (* Construct the closure *)
      Clos_lit
        (Code_lit ((env_var', Tuple_type env_tys), (param_var', param_ty), body),
          Tuple_lit env_tms)

  | Fun_app (head, arg) ->
      let head = translate env head in
      let arg = translate env arg in
      Clos_app (head, arg)
