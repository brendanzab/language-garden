(** A demonstration of translating bidirectional inference rules for the STLC
    into a type inference algorithm.

    For more information on this technique, see David Christiansen’s excellent
    {{: https://davidchristiansen.dk/tutorials/bidirectional.pdf} tutorial}.

    Extends [check_stlc_inference_rules.ml].
*)

[@@@warning "-unused-constructor"]
[@@@warning "-unused-value-declaration"]

(*
  t ::=
    | Bool
    | t -> t

  e ::=
    | x
    | e : t
    | \x. e
    | e e
    | true
    | false
    | if e then e else e

  Γ ::=
    | ∅
    | Γ, x : t
*)

type ty (* t *) =
  | Bool                                (* Bool *)
  | Fun of ty * ty                      (* t -> t *)

type expr (* e *) =
  | Var of string                       (* x *)
  | Ann of expr * ty                    (* e : t *)
  | Lam of string * expr                (* \x. e *)
  | App of expr * expr                  (* e e *)
  | True                                (* true *)
  | False                               (* false *)
  | If_then_else of expr * expr * expr  (* if e then e else e *)

type ctx (* Γ *) =
  | Empty                               (* ∅ *)
  | Extend of ctx * string * ty         (* Γ, x : t *)

(*
  ┌───────────┐
  │ x : t ∈ Γ │
  └───────────┘

  ──────────────────── (V-Stop)
    x : t ∈ Γ, x : t

        x : t ∈ Γ
  ───────────────────── (V-Pop)
    x : t ∈ Γ, y : t'
*)

(* x : t ∈ Γ *)
let rec lookup (ctx : ctx) (x : string) =
  match ctx with
  | Empty -> failwith "unbound variable"
  | Extend (_, y, t) when x = y -> t      (* V-Stop *)
  | Extend (ctx, _, _) -> lookup ctx x    (* V-Pop *)

(*
  ┌───────────┐
  │ Γ ⊢ e ⇐ t │
  └───────────┘

      Γ, x : t1 ⊢ e ⇐ t2
  ─────────────────────────── (C-Lam)
    Γ ⊢ (\x. e) ⇐ t1 -> t2

    Γ ⊢ e ⇒ t'    t' = t
  ──────────────────────── (C-Conv)
          Γ ⊢ e ⇐ t

    Γ ⊢ e1 ⇐ Bool    Γ ⊢ e2 ⇐ t    Γ ⊢ e3 ⇐ t
  ───────────────────────────────────────────── (C-IfThenElse)
          Γ ⊢ if e1 then e2 else e3 ⇐ t

  ┌───────────┐
  │ Γ ⊢ e ⇒ t │
  └───────────┘

    x : t ∈ Γ
  ───────────── (I-Var)
    Γ ⊢ x ⇒ t

      Γ ⊢ e ⇐ t
  ───────────────── (I-Ann)
    Γ ⊢ e : t ⇒ t

    Γ ⊢ e1 ⇒ t1 -> t2    Γ ⊢ e2 ⇐ t1
  ──────────────────────────────────── (I-App)
            Γ ⊢ e1 e2 ⇒ t2

  ─────────────────── (I-True)
    Γ ⊢ true : Bool

  ─────────────────── (I-False)
    Γ ⊢ false : Bool

*)

(* Γ ⊢ e ⇐ t *)
let rec check (ctx : ctx) (e : expr) (t : ty) : unit =
  match e, t with
  (* C-Lam *)
  | Lam (x, e), Fun (t1, t2) ->
      check (Extend (ctx, x, t1)) e t2

  (* C-IfThenElse *)
  | If_then_else (e1, e2, e3), t ->
      check ctx e1 Bool;
      check ctx e2 t;
      check ctx e3 t;

  (* C-Conv *)
  | e, t ->
      if infer ctx e = t then () else
        failwith "type mismatch"

(* Γ ⊢ e ⇒ t *)
and infer (ctx : ctx) (e : expr) : ty =
  match e with
  (* I-Var *)
  | Var x -> lookup ctx x

  (* I-Ann *)
  | Ann (e, t) ->
      check ctx e t; t

  (* I-App *)
  | App (e1, e2) ->
      begin match infer ctx e1 with
      | Fun (t1, t2) when infer ctx e2 = t1 -> t2
      | _ -> failwith "type mismatch"
      end

  (* I-True, I-False *)
  | True | False -> Bool

  | Lam _ | If_then_else _ ->
      failwith "ambiguous"
