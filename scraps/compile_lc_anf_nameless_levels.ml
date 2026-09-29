(** Compiling de Bruijn indexed lambda terms to terms in A-normal form, using
    de Bruijn levels to generate fresh variables.

    Inspired by {{: https://github.com/AndrasKovacs/staged/blob/9c4e2017669086e2f77df5014f1c215a5a7e07a3/opt/new/}
    Andras Kovacs’ examples}, but translating from a de Bruijn indexed core
    language, instead of one that uses levels.
*)

(* TODO: Optimise tail calls like in Andras Kovacs’ examples *)

[@@@warning "-unused-value-declaration"]

(** De Bruijn indexed lambda terms *)
module Core = struct

  (** De Bruijn index *)
  type index = int

  type tm =
    | Var of index
    | Let of string * tm * tm
    | Fun_lit of string * tm
    | Fun_app of tm * tm
    | Int_lit of int
    | Bool_lit of bool
    | Bool_elim of tm * tm * tm
    | Prim_app of [`Neg | `Add | `Mul] * tm list

  (* TODO: Pretty printing *)

end


(** De Bruijn indexed lambda terms in A-normal form *)
module Anf = struct

  (** De Bruijn level *)
  type level = int

  type tm =
    | Let_comp of string * comp_tm * tm
    | Let_join of string * string * tm * tm
    | Join_app of level * atom_tm
    | Bool_elim of atom_tm * tm * tm
    | Comp of comp_tm

  and comp_tm =
    | Fun_app of atom_tm * atom_tm
    | Prim_app of [`Neg | `Add | `Mul] * atom_tm list
    | Atom of atom_tm

  and atom_tm =
    | Var of level
    | Fun_lit of string * tm
    | Int_lit of int
    | Bool_lit of bool

  type value =
    | Fun_lit of env * tm
    | Int_lit of int
    | Bool_lit of bool
    | Join of level * tm

  and env = value list

  let eval_atom (env : env) (tm : atom_tm) : value =
    match tm with
    | Var level -> List.nth env (List.length env - level - 1)
    | Fun_lit (_, body) -> Fun_lit (env, body)
    | Int_lit b -> Int_lit b
    | Bool_lit b -> Bool_lit b

  let eval_comp (env : env) (tm : comp_tm) : value =
    match tm with
    | Fun_app (fn, arg) -> eval_atom (eval_atom env arg :: env) fn
    | Prim_app (prim, args) ->
        begin match prim, args |> List.map (eval_atom env) with
        | `Neg, [Int_lit i] -> Int_lit (Int.neg i)
        | `Add, [Int_lit i1; Int_lit i2] -> Int_lit (Int.add i1 i2)
        | `Mul, [Int_lit i1; Int_lit i2] -> Int_lit (Int.mul i1 i2)
        | _ -> invalid_arg "Anf.eval_comp"
        end
    | Atom tm -> eval_atom env tm

  let rec eval (env : env) (tm : tm) : value =
    match tm with
    | Let_comp (_, def, body) ->
        eval (eval_comp env def :: env) body
    | Let_join (_, _, cont, body) ->
        eval (Join (List.length env, cont) :: env) body
    | Join_app (level, arg) ->
        begin match List.nth env (List.length env - level - 1) with
        (* TODO: Implement evaluation for join points. We should be able to
           reuse the current environment, but I'm not sure how this works given
           we are using levels *)
        | Join (level, body) -> failwith "TODO"
        | _ -> invalid_arg "Anf.eval"
        end
    | Bool_elim (cond, tm1, tm2) ->
        begin match eval_atom env cond with
        | Bool_lit true -> eval env tm1
        | Bool_lit false -> eval env tm2
        | _ -> invalid_arg "Anf.eval"
        end
    | Comp tm -> eval_comp env tm

  (* TODO: Pretty printing *)

end


(** A-normal form conversion *)
module Anf_conv : sig

  val translate : Core.tm -> Anf.tm

end = struct

  type 'a k = size:Anf.level -> 'a -> Anf.tm
  (** Continuation. The [size] parameter tracks the number of entries in the
      target environment, and is used for generating fresh levels in the target
      term. *)

  let comp : Anf.comp_tm k =
    fun ~size:_ tm -> Anf.Comp tm

  let join_app (level : Anf.level) : Anf.atom_tm k =
    fun ~size:_ tm -> Anf.Join_app (level, tm)

  (** Translate a term to A-normal form. The environment records the level of
      the bindings in the source terms we have passed over. Note that we only
      need to extend this environment when passing over bindings in the source
      language. *)
  let rec translate (env : Anf.level list) (tm : Core.tm) : Anf.comp_tm k k =
    fun ~size k ->
      match tm with
      | Core.Var src_index ->
          k ~size (Anf.Atom (Var (List.nth env src_index)))
      | Core.Let (def_name, src_def, src_body) ->
          translate env src_def ~size @@ fun ~size def ->
            Anf.Let_comp (def_name, def,
              translate (size :: env) src_body ~size:(size + 1) k)
      | Core.Fun_lit (param_name, src_body) ->
          k ~size (Anf.Atom (Fun_lit (param_name,
            translate (size :: env) src_body ~size:(size + 1) comp)))
      | Core.Fun_app (src_fn, src_arg) ->
          translate_def env "fn" src_fn ~size @@ fun ~size fn ->
            translate_def env "arg" src_arg ~size @@ fun ~size arg ->
              k ~size (Fun_app (fn, arg))
      | Core.Int_lit i ->
          k ~size (Anf.Atom (Int_lit i))
      | Core.Bool_lit b ->
          k ~size (Anf.Atom (Bool_lit b))
      | Core.Bool_elim (src_cond, tm1, tm2) ->
          translate_def env "cond" src_cond ~size @@ fun ~size cond ->
            Anf.Let_join ("cont", "param", k ~size:(size + 1) (Anf.Atom (Var size)),
              Anf.Bool_elim (cond,
                (translate_def env "branch1" tm1 ~size:(size + 1) (join_app size)),
                (translate_def env "branch2" tm2 ~size:(size + 1) (join_app size))))
      | Core.Prim_app (name, src_args) ->
          translate_defs env "arg" src_args ~size @@ fun ~size args ->
            k ~size (Anf.Prim_app (name, args))

  (** Translate a term to A-normal form, binding it to an intermediate definition. *)
  and translate_def (env : Anf.level list) (name : string) (tm : Core.tm) : Anf.atom_tm k k =
    fun ~size k ->
      translate env tm ~size @@ fun ~size tm ->
        match tm with
        | Anf.Atom tm -> k ~size tm
        | tm -> Anf.Let_comp (name, tm, k ~size:(size + 1) (Var size))

  (** Translate a sequence of terms, binding them to intermediate definitions. *)
  and translate_defs (env : Anf.level list) (name : string) (tms : Core.tm list) : Anf.atom_tm list k k =
    fun ~size k ->
      match tms with
      | [] -> k ~size []
      | tm :: tms ->
          translate_def env name tm ~size @@ fun ~size tm ->
            translate_defs env name tms ~size @@ fun ~size tms ->
              k ~size (tm :: tms)

  (** Translate a closed term *)
  let translate (tm : Core.tm) : Anf.tm =
    translate [] tm ~size:0 comp

end


(* TODO: Closure conversion *)
(* TODO: Tests *)
