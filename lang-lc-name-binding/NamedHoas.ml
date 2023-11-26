(** The lambda calculus, implemented using normalisation-by-evaluation with
    names in the syntax and higher-order abstract syntax (HOAS) in the semantic
    domain.
*)

(** {1 Syntax} *)

type expr =
  | Var of string
  | Let of string * expr * expr
  | FunLit of string * expr
  | FunApp of expr * expr

(** {2 Alpha Equivalence} *)

(** Compare the syntactic structure of two expressions, taking into account
    binding structure while ignoring differences in names. *)
let alpha_equiv (e1 : expr) (e2 : expr) =
  (* Compare for alpha equivalence by comparing the binding depth of variable
    names in each expression. This approach is described in section 6.1 of
    {{:https://davidchristiansen.dk/tutorials/implementing-types-hs.pdf}
    “Checking Dependent Types with Normalization by Evaluation: A Tutorial
    (Haskell Version)”} by David Christiansen. *)
  let rec go (size : int) (ns1, e1 : (string * int) list * expr) (ns2, e2 : (string * int) list * expr) : bool =
    match e1, e2 with
    | Var x1, Var x2 -> begin
        match List.assoc_opt x1 ns1, List.assoc_opt x2 ns2 with
        | None, None -> x1 = x2
        | Some l1, Some l2  -> l1 = l2
        | _, _ -> false
    end
    | Let (x1, def1, body1), Let (x2, def2, body2) ->
        go size (ns1, def1) (ns2, def2)
          && go (size + 1) ((x1, size) :: ns1, body1) ((x2, size) :: ns2, body2)
    | FunLit (x1, body1), FunLit (x2, body2) ->
        go (size + 1) ((x1, size) :: ns1, body1) ((x2, size) :: ns2, body2)
    | FunApp (head1, arg1), FunApp (head2, arg2) ->
        go size (ns1, head1) (ns2, head2) && go size (ns1, arg1) (ns2, arg2)
    | _, _ -> false
  in
  go 0 ([], e1) ([], e2)


(** {1 Semantics} *)

type value =
  | Neu of neu
  | FunLit of string * (value -> value)
and neu =
  | Var of string
  | FunApp of neu * value

type env = (string * value) list

(** {2 Evaluation} *)

let rec eval (vs : env) (e : expr) : value =
  match e with
  | Var x -> List.assoc x vs
  | Let (x, def, body) -> eval ((x, eval vs def) :: vs) body
  | FunLit (x, body) -> FunLit (x, fun v -> eval ((x, v) :: vs) body)
  | FunApp (head, arg) -> begin
      match eval vs head with
      | FunLit (_, body) -> body (eval vs arg)
      | Neu nv -> Neu (FunApp (nv, eval vs arg))
  end

(** {2 Quotation} *)

let rec fresh (ns : string list) (x : string) : string =
  match List.mem x ns with
  | true -> fresh ns (x ^ "'")
  | false -> x

let rec quote (ns : string list) (v : value) : expr =
  match v with
  | Neu nv -> quote_neu ns nv
  | FunLit (x, body) ->
      let x = fresh ns x in
      FunLit (x, quote (x :: ns) (body (Neu (Var x))))
and quote_neu (ns : string list) (nv : neu) : expr =
  match nv with
  | Var x -> Var x
  | FunApp (head, arg) -> FunApp (quote_neu ns head, quote ns arg)

(** {2 Normalisation-by-evaluation} *)

let normalise (vs : env) (e : expr) : expr =
  quote (List.map fst vs) (eval vs e)
