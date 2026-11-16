(** The type checking algorithm described by Thierry Coquand in:

    - Thierry Coquand. 1996. An algorithm for type-checking dependent types.
      https://doi.org/10.1016/0167-6423(95)00021-6

    This paper is an early demonstration of an efficient algorithm for checking
    dependent types. It features bidirectional typing (prior to Pierce and
    Turner 1998), and an algorithm that looks like a precursor to
    normalisation-by-evaluation-style approaches for checking dependent types,
    where we evaluate terms before storing them in the typing context and using
    them in further type checking. It also performs conversion checking directly
    on values, like in Andras Kovacs’ elaboration zoo.

    Bugs found in Appendix A:

    - When checking let expressions
      - definitions are not checked against their annotations
      - the size of the context is not incremented when checking the body
*)

type id = string

type exp =
  | Var of id
  | App of exp * exp
  | Abs of id * exp
  | Let of id * exp * exp *exp
  | Pi of id * exp * exp
  | Type

type value =
  | V_gen of int              (* Generic values (free variables) *)
  | V_app of value * value
  | V_type
  | V_clos of env * exp

(** [V_gen] and [V_app] are both considered “neutral” terms in NbE-style
    approaches.

    It's interesting that closures are reused for both [Abs] and [Pi].
    In later approaches, we have separate values for lambdas and pi types,
    and nest closures in the body of these.

    [V_clos] seems to also be used in some cases to get a form of lazy
    evaluation - though it would result re-doing the same work over and over.
*)

and env = (id * value) list

let update (env : env) (x : id) (u : value) : env =
  (x, u) :: env

let rec lookup (x : id) (env : env) : value =
  match env with
  | (y, u) :: env when x = y -> u
  | (_, _) :: env -> lookup x env
  | [] -> failwith (Format.lasprintf "lookup %s" [x])

let rec eval (env : env) (e : exp) : value =
  match e with
  | Var x -> lookup x env
  | App (e1, e2) -> app (eval env e1) (eval env e2)
  | Let (x, e1, _, e3) -> eval (update env x (eval env e1)) e3
  | Type -> V_type
  | Abs _ | Pi _ -> V_clos (env, e)

and app (u : value) (v : value) : value =
  match u with
  | V_clos (env, Abs (x, e)) -> eval (update env x v) e
  | _ -> V_app (u, v)

(* This seems to be needed because we have values like [V_clos]. This forces the
   outer-most closures to be evaluated. This is not required in approaches that
   are more common today, where values are in WHNF by-definition. *)
let rec whnf (v : value) : value =
  match v with
  | V_app (u, w) -> app (whnf u) (whnf v)
  | V_clos (env, e) -> eval env e
  | V_gen _ | V_type -> v

(** Conversion checking *)

let rec eq_val (k : int) (u1 : value) (u2 : value) : bool =
  match whnf u1, whnf u2 with
  | V_type, V_type -> true
  | V_app (t1, w1), V_app (t2, w2) ->
      eq_val k t1 t2 && eq_val k w1 w2
  | V_gen k1, V_gen k2 -> k1 = k2
  | V_clos (env1, Abs (x1, e1)), V_clos (env2, Abs (x2, e2)) ->
      let v = V_gen k in
      eq_val (k + 1)
        (V_clos (update env1 x1 v, e1))
        (V_clos (update env2 x2 v, e2))
  | V_clos (env1, Pi (x1, a1, b1)), V_clos (env2, Pi (x2, a2, b2)) ->
      let v = V_gen k in
      eq_val k (V_clos (env1, a1)) (V_clos (env2, a2))
        && eq_val (k + 1)
          (V_clos (update env1 x1 v, b1))
          (V_clos (update env2 x2 v, b2))
  | _, _ -> false

(** Type checking and type inference *)

(** - [k] is the size of the context, used for fresh variable generation
    - [rho] is the value environment
    - [gamma] is the type environment
*)

let rec check_exp (k, rho, gamma : int * env * env) (e : exp) (v : value) : bool =
  match e with
  | Abs (x, n) ->
      begin match whnf v with
      | V_clos (env, Pi (y, a, b)) ->
          let v = V_gen k in
          (* NOTE: We need to be careful in using the right environment for [a]
             and [b]. This seems error-prone. *)
          check_exp
            (k + 1, update rho x v, update gamma x (V_clos (env, a)))
            n (V_clos (update env y v, b))
      | _ -> failwith "expected Pi"
      end
  | Pi (x, a, b) ->
      begin match whnf v with
      | V_type ->
          (* NOTE: [V_clos] seems to be used as a thunk here, to defer
             evaluating [a] until it is needed. Unfortunately this could result
             in repeated evaluations of [a], as the result is not stored for
             later use. *)
          check_type (k, rho, gamma) a
            && check_type (k + 1, update rho x (V_gen k), update gamma x (V_clos (rho, a))) b
      | _ -> failwith "expected Type"
      end
  | Let (x, e1, e2, e3) ->
      check_type (k, rho, gamma) e2 &&
        (* NOTE: [e1] is not checked in paper, nor is [k] incremented when
           checking the body of the let expression! *)
        let a = eval rho e2 in
        check_exp (k, rho, gamma) e1 a
          && check_exp (k + 1, update rho x (eval rho e1), update gamma x a) e3 v
  | _ -> eq_val k (infer_exp (k, rho, gamma) e) v

and check_type (k, rho, gamma : int * env * env) (e : exp) : bool =
  check_exp (k, rho, gamma) e V_type

and infer_exp (k, rho, gamma : int * env * env) (e : exp) : value =
  match e with
  | Var id ->
      lookup id gamma
  | App (e1, e2) ->
      begin match whnf (infer_exp (k, rho, gamma) e1) with
      | V_clos (env, Pi (x, a, b)) ->
          if check_exp (k, rho, gamma) e2 (V_clos (env, a)) then
            V_clos (update env x (V_clos (rho, e2)), b)
          else
            failwith "application error"
      | _ -> failwith "application, expected Pi"
      end
  | Type -> V_type
  | _ -> failwith "cannot infer type"

let type_check (m : exp) (a : exp) : bool =
  check_type (0, [], []) a
    && check_exp (0, [], []) m (V_clos ([], a))

let () =
  let a = Pi ("A", Type, Pi ("x", Var "A", Var "A")) in
  let m = Abs ("A", Abs ("x", Var "x")) in
  assert (type_check m a)
