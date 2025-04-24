(**  A demo of translating inference rules for the STLC into a type inference algorithm

     Originally posted at https://gist.github.com/brendanzab/ebdf8bed6f239d3abbc50a396ce468ae
*)

[@@@warning "-unused-constructor"]
[@@@warning "-unused-value-declaration"]

(*
  T ::=
    | Bool
    | t -> t

  e ::=
    | x
    | \(x : t). e
    | e e
    | true
    | false

  Γ ::=
    | ∅
    | Γ, x : t
*)

type ty (* t *) =
  | Bool                          (* Bool *)
  | Fun of ty * ty                (* t -> t *)

type tm (* e *) =
  | Var of string                 (* x *)
  | Lam of string * ty * tm       (* \(x : t). e *)
  | App of tm * tm                (* e e *)
  | True                          (* true *)
  | False                         (* false *)

type ctx (* Γ *) =
  | Empty                         (* ∅ *)
  | Extend of ctx * string * ty   (* Γ, x : t *)

(*
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
    x : t ∈ Γ
  ───────────── (T-Var)
    Γ ⊢ x : t

          Γ, x : t1 ⊢ e : t2
  ───────────────────────────────── (T-Lam)
    Γ ⊢ (\(x : t1). e) : t1 -> t2

    Γ ⊢ e1 : t1 -> t2    Γ ⊢ e2 : t1
  ──────────────────────────────────── (T-App)
              Γ ⊢ e1 e2 : t2

  ─────────────────── (T-True)
    Γ ⊢ true : Bool

  ─────────────────── (T-False)
    Γ ⊢ false : Bool
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
  | Lam (x, t1, e) ->
      let t2 = infer (Extend (ctx, x, t1)) e in
      Fun (t1, t2)

  (* T-App *)
  | App (e1, e2) ->
      begin match infer ctx e1 with
      | Fun (t1, t2) when infer ctx e2 = t1 -> t2
      | _ -> failwith "type mismatch"
      end

  (* T-True, T-False *)
  | True | False -> Bool
