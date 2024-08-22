(**  A demo of translating inference rules for the STLC into a type inference algorithm

     Originally posted at https://gist.github.com/brendanzab/ebdf8bed6f239d3abbc50a396ce468ae
*)

(*
  T ::=
    | Bool
    | T -> T

  t ::=
    | x
    | \(x : T). t
    | t t

  Γ ::=
    | ∅
    | Γ, x : T
*)

type ty (* T *) =
  | Bool                  (* Bool *)
  | Fun of ty * ty        (* T -> T *)

type tm (* t *) =
  | Var of int            (* x *)
  | Lam of ty * tm        (* \(x : T). t *)
  | App of tm * tm        (* t t *)

type ctx (* Γ *) =
  | Empty                 (* ∅ *)
  | Extend of ctx * ty    (* Γ, x : T *)

(*
  ──────────────────── (V-Stop)
    x : T ∈ Γ, x : T

        x : T ∈ Γ
  ───────────────────── (V-Pop)
    x : T ∈ Γ, y : T'
*)

(* x : T ∈ Γ *)
let rec lookup (ctx : ctx) (i : int) =
  match ctx with
  | Empty -> failwith "unbound variable"
  | Extend (_, ty) when i = 0 -> ty          (* V-Stop *)
  | Extend (ctx, _) -> lookup ctx (i - 1)    (* V-Pop *)

(*
    x : T ∈ Γ
  ───────────── (T-Var)
    Γ ⊢ x : T

          Γ, x : T1 ⊢ t : T2
  ───────────────────────────────── (T-Lam)
    Γ ⊢ (\(x : T1). t) : T1 -> T2

    Γ ⊢ t1 : T1 -> T2    Γ ⊢ t2 : T1
  ──────────────────────────────────── (T-App)
              Γ ⊢ t1 t2 : T2
*)

(* Γ ⊢ x : T *)
let rec infer (ctx : ctx) (tm : tm) : ty =
  (* Note how we visit the nodes of a proof tree (specified by the inference
     rules) using the call-stack. This is not the only way to traverse the proof
     tree – we could take other paths through it! *)

  match tm with
  (* T-Var *)
  | Var i -> lookup ctx i

  (* T-Lam *)
  | Lam (param_ty, body_tm) ->
      let body_ty = infer (Extend (ctx, param_ty)) body_tm in
      Fun (param_ty, body_ty)

  (* T-App *)
  | App (head_tm, arg_tm) -> begin
      match infer ctx head_tm with
      | Fun (param_ty, body_ty) ->
          if infer ctx arg_tm = param_ty then body_ty else
            failwith "mismatched argument type"
      | _ ->
          failwith "unexpected argument"
  end
