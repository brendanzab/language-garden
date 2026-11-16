(** A version of [check_dependent_coquand_1996.ml] that is altered to follow
    more up-to-date conventions with respect to values and closures.
    This is closer to the approach used in the Mini-TT paper, and in the
    implementations in Andras Kovacs’ elaboration zoo.

    I feel like the changes make evaluation, conversion, and type checking
    easier to follow. They might however make it harder to formalise compared to
    the original paper, due to the additional mutual recursion and partial
    functions involved.
*)

type id = string

type exp =
  | Var of id
  | App of exp * exp
  | Abs of id * exp
  | Let of id * exp * exp *exp
  | Pi of id * exp * exp
  | Type

(** Values, in weak head normal form.

    Compared with the original paper, we move [V_gen] and [V_app] into
    [V_neu], and replace [V_clos] with [V_abs] and [V_pi] variants. *)
type value =
  | V_neu of neu
  | V_type
  | V_abs of clos
  | V_pi of value * clos

(** Neutral values, which are prevented from evaluating further by free variables. *)
and neu =
  | N_gen of int              (* Generic values (free variables) *)
  | N_app of neu * value

and clos =
  | Clos of id * env * exp

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
  | Abs (x, e) -> V_abs (Clos (x, env, e))
  | Pi (x, e1, e2) -> V_pi (eval env e1, Clos (x, env, e2))

and app (u : value) (v : value) : value =
  match u with
  | V_abs clos -> inst_clos clos v
  | V_neu n -> V_neu (N_app (n, v))
  | _ -> failwith "application error"

(** Helper function to instantiate closures. This prevents use having to juggle
    closures directly, which could result in bugs. *)
and inst_clos (Clos (x, env, e) : clos) (v : value) : value =
  eval (update env x v) e

(** Conversion checking *)

let rec eq_val (k : int) (u1 : value) (u2 : value) : bool =
  match u1, u2 with
  | V_neu n1, V_neu n2 -> eq_neu k n1 n2
  | V_type, V_type -> true
  | V_abs clos1, V_abs clos2 ->
      let v = V_neu (N_gen k) in
      eq_val (k + 1) (inst_clos clos1 v) (inst_clos clos2 v)
  | V_pi (a1, clos1), V_pi (a2, clos2) ->
      let v = V_neu (N_gen k) in
      eq_val k a1 a2 && eq_val (k + 1) (inst_clos clos1 v) (inst_clos clos2 v)
  | _, _ -> false

and eq_neu (k : int) (n1 : neu) (n2 : neu) : bool =
  match n1, n2 with
  | N_gen k1, N_gen k2 -> k1 = k2
  | N_app (n1, w1), N_app (n2, w2) ->
      eq_neu k n1 n2 && eq_val k w1 w2
  | _, _ -> false

(** Type checking and type inference *)

(* Bundle together the information we need during typechecking *)
type ctx = {
  size : int;     (* size of the context, used for fresh variable generation *)
  type_env : env;
  value_env : env;
}

let empty_ctx = {
  size = 0;
  type_env = [];
  value_env = [];
}

let update_ctx (ctx : ctx) (x : id) (t : value) (e : value) = {
  size = ctx.size + 1;
  type_env = update ctx.type_env x t;
  value_env = update ctx.value_env x t;
}

let rec check_exp (ctx : ctx) (e : exp) (v : value) : bool =
  match e with
  | Abs (x, n) ->
      begin match v with
      | V_pi (a, clos) ->
          let v = V_neu (N_gen ctx.size) in
          check_exp (update_ctx ctx x a v) n (inst_clos clos v)
      | _ -> failwith "expected Pi"
      end
  | Pi (x, a, b) ->
      begin match v with
      | V_type ->
          check_type ctx a
            && check_type (update_ctx ctx x (eval ctx.value_env a) (V_neu (N_gen ctx.size))) b
      | _ -> failwith "expected Type"
      end
  | Let (x, e1, e2, e3) ->
      check_type ctx e2 &&
        let a = eval ctx.value_env e2 in
        check_exp ctx e1 a
          && check_exp (update_ctx ctx x a (eval ctx.value_env e1)) e3 v
  | _ -> eq_val ctx.size (infer_exp ctx e) v

and check_type (ctx : ctx) (e : exp) : bool =
  check_exp ctx e V_type

and infer_exp (ctx : ctx) (e : exp) : value =
  match e with
  | Var id ->
      lookup id ctx.type_env
  | App (e1, e2) ->
      begin match infer_exp ctx e1 with
      | V_pi (a, clos) ->
          if check_exp ctx e2 a then
            inst_clos clos (eval ctx.value_env e2)
          else
            failwith "application error"
      | _ -> failwith "application, expected Pi"
      end
  | Type -> V_type
  | _ -> failwith "cannot infer type"

let type_check (m : exp) (a : exp) : bool =
  check_type empty_ctx a
    && check_exp empty_ctx m (eval [] a)

let () =
  let a = Pi ("A", Type, Pi ("x", Var "A", Var "A")) in
  let m = Abs ("A", Abs ("x", Var "x")) in
  assert (type_check m a)
