(** {0 Typed lambda lifting on alpha renamed terms}

    Translation from {!Lang.Fun_a} to {!Lang.Lifted_a}.

    This translation lifts the code of function literals to top-level
    definitions, calling the code immediately if possible. This cuts down on
    the number of closures being constructed, compared to {!Translation.Fun_a_to_clos_a}.
    We could further reduce the number of closures even more by uncurrying
    functions during this translation.
*)

module Fun_a = Lang.Fun_a
module Lifted_a = Lang.Lifted_a

module Var_map = Fun_a.Var_map
module Var_set = Fun_a.Var_set


(** {1 Translation} *)

(** Translation to lambda-lifted types *)
let rec translate_ty : Fun_a.ty -> Lifted_a.ty =
  function
  | Bool_type -> Bool_type
  | Int_type -> Int_type
  | Fun_type (param_ty, body_ty) ->
      Clos_type (translate_ty param_ty, translate_ty body_ty)

(** Translation to lambda lifted terms. The [name] parameter is used when
    naming global definitions, to aid with debugging translated terms. *)
let rec translate globals locals ?name : Fun_a.tm -> Lifted_a.globals * Lifted_a.tm =
  function
  | Var var ->
      globals, fst (Var_map.find var locals)

  | Let (def_var, def_ty, def, body) ->
      let def_name = Fun_a.Var.name def_var in
      let def_ty = translate_ty def_ty in

      (* Function literals will be lifted to the top level, so there’s no need to
        compile them to intermediate let bindings. *)
      begin match translate globals locals ~name:def_name def with
      | globals, (Clos_lit (_, _) as def) ->
          let body_env = Var_map.add def_var (def, def_ty) locals in
          translate globals body_env body
      | globals, def ->
          let def_var' = Lifted_a.Local_var.fresh def_name in
          let body_env = Var_map.add def_var (Lifted_a.Local_var def_var', def_ty) locals in
          let globals, body = translate globals body_env body in
          globals, Let (def_var', def_ty, def, body)
      end

  | Bool_lit b -> globals, Bool_lit b
  | Int_lit i -> globals, Int_lit i

  | Prim_app (prim, args) ->
      let globals, args =
        List.fold_left_map (fun globals arg -> translate globals locals arg)
          globals
          args
      in
      globals, Prim_app (prim, args)

  | Fun_lit (param_var, param_ty, body) ->
      (* A fresh variable to be used for the environment parameter in the code
        of the closure *)
      let env_var' = Lifted_a.Local_var.fresh "env" in

      (* Create a fresh variable for the parameter in the target language,
        and then transtlate the parameter *)
      let param_var' = Lifted_a.Local_var.fresh (Fun_a.Var.name param_var) in
      let param_ty = translate_ty param_ty in
      let param_tm = Lifted_a.Local_var param_var' in

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
      let globals, body =
        let ~label:_, ~body_env =
          ListLabels.fold_left body_fvs
            ~init:(~label:0, ~body_env:(Var_map.singleton param_var (param_tm, param_ty)))
            ~f:(fun (~label, ~body_env) id ->
              let ty = snd (Var_map.find id locals) in
              let tm = Lifted_a.Tuple_proj (Local_var env_var', label) in
              ~label:(label + 1), ~body_env:(Var_map.add id (tm, ty) body_env))
        in
        translate globals body_env body in

      (* Lists of terms and types to be used when explicitly constructing the
        environment of the closure *)
      let env_tys = List.map (fun id -> snd (Var_map.find id locals)) body_fvs in
      let env_tms = List.map (fun id -> fst (Var_map.find id locals)) body_fvs in

      (* Create the lifted code *)
      let lifted_name = Option.value name ~default:"anon" in
      let lifted_var = Lifted_a.Global_var.fresh lifted_name in
      let lifted_code = Lifted_a.{
        env = env_var', Tuple_type env_tys;
        param = param_var', param_ty;
        body;
      } in

      (* Add the lifted function to the global and construct a closure *)
      (lifted_var, lifted_code) :: globals,
      Clos_lit (lifted_var, Tuple_lit env_tms)

  | Fun_app (head, arg) ->
      let globals, head = translate globals locals head in
      let globals, arg = translate globals locals arg in

      (* If we are applying an argument directly to a closure we can skip
         constructing one - otherwise we assume it’s a closure. *)
      begin match head with
      | Clos_lit (code, env) -> globals, Code_app (code, env, arg)
      | head -> globals, Clos_app (head, arg)
      end

(** Translation to lambda lifted terms. *)
let translate (tm : Fun_a.tm) : Lifted_a.lifted_tm =
  let globals, main = translate [] Fun_a.Var_map.empty tm in
  { globals; main }
