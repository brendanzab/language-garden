(** Cleaned up version of [check_dependent_coquand_1996_nbe.ml] *)

type id = string
type level = int

type exp =
  | Var of id
  | Let of id * exp * exp *exp
  | Type
  | Fun_type of id * exp * exp
  | Fun_abs of id * exp
  | Fun_app of exp * exp

(** Semantic domain *)
module Domain = struct

  (** Values, i.e. open expressions in weak head normal form. *)
  type value =
    (* Neutral values *)
    | Neu of neu
    (* Canonical values *)
    | Type
    | Fun_type of value * clos
    | Fun_abs of clos

  (** Neutral values, which are prevented from evaluating further by free variables. *)
  and neu =
    | Var of level
    | Fun_app of neu * value

  and clos =
    | Clos of id * env * exp

  and env = (id * value) list

  let extend (env : env) (x : id) (u : value) : env =
    (x, u) :: env

  let rec lookup (x : id) (env : env) : value option =
    match env with
    | (y, u) :: env when x = y -> Some u
    | (_, _) :: env -> lookup x env
    | [] -> None

end

module D = Domain

let rec eval (env : D.env) (e : exp) : D.value =
  match e with
  | Var x -> D.lookup x env |> Option.get
  | Let (x, e1, _, e3) -> eval (D.extend env x (eval env e1)) e3
  | Type -> D.Type
  | Fun_type (x, e1, e2) -> D.Fun_type (eval env e1, Clos (x, env, e2))
  | Fun_abs (x, e) -> D.Fun_abs (Clos (x, env, e))
  | Fun_app (e1, e2) -> app (eval env e1) (eval env e2)

and app (u : D.value) (v : D.value) : D.value =
  match u with
  | D.Fun_abs clos -> inst_clos clos v
  | D.Neu n -> D.Neu (D.Fun_app (n, v))
  | _ -> failwith "application error"

(** Helper function to instantiate closures. This prevents use having to juggle
    closures directly, which could result in bugs. *)
and inst_clos (Clos (x, env, e) : D.clos) (v : D.value) : D.value =
  eval (D.extend env x v) e

(** Conversion checking *)

let rec eq_val (size : int) (v1 : D.value) (v2 : D.value) : bool =
  match v1, v2 with
  | D.Neu n1, D.Neu n2 -> eq_neu size n1 n2
  | D.Type, D.Type -> true
  | D.Fun_type (a1, clos1), D.Fun_type (a2, clos2) ->
      let v = D.Neu (D.Var size) in
      eq_val size a1 a2 && eq_val (size + 1) (inst_clos clos1 v) (inst_clos clos2 v)
  | D.Fun_abs clos1, D.Fun_abs clos2 ->
      let v = D.Neu (D.Var size) in
      eq_val (size + 1) (inst_clos clos1 v) (inst_clos clos2 v)
  | _, _ -> false

and eq_neu (size : int) (n1 : D.neu) (n2 : D.neu) : bool =
  match n1, n2 with
  | D.Var l1, D.Var l2 -> l1 = l2
  | D.Fun_app (n1, v1), D.Fun_app (n2, v2) ->
      eq_neu size n1 n2 && eq_val size v1 v2
  | _, _ -> false

(** Type checking and type inference *)

module Ctx : sig

  type t

  val empty : t
  val extend : t -> id -> D.value -> D.value -> t
  val next_var : t -> D.value
  val lookup : t -> id -> D.value option

  val eval : t -> exp -> D.value
  val eq_val : t -> D.value -> D.value -> bool

end = struct

  type t = {
    size : int;     (* size of the context, used for fresh variable generation *)
    type_env : D.env;
    value_env : D.env;
  }

  let empty = {
    size = 0;
    type_env = [];
    value_env = [];
  }

  let extend (ctx : t) (x : id) (t : D.value) (e : D.value) = {
    size = ctx.size + 1;
    type_env = D.extend ctx.type_env x t;
    value_env = D.extend ctx.value_env x t;
  }

  let next_var (ctx : t) =  D.Neu (D.Var ctx.size)
  let lookup (ctx : t) (x : id) = D.lookup x ctx.type_env
  let eval (ctx : t) = eval ctx.value_env
  let eq_val (ctx : t) = eq_val ctx.size

end

exception Type_error of string

let type_error fmt args =
  raise (Type_error (Format.lasprintf fmt args))

let rec check_exp (ctx : Ctx.t) (e : exp) (t : D.value) =
  match e with
  | Let (x, e1, e2, e3) ->
      check_type ctx e2;
      let a = Ctx.eval ctx e2 in
      check_exp ctx e1 a;
      check_exp (Ctx.extend ctx x a (Ctx.eval ctx e1)) e3 t
  | Fun_abs (x, n) ->
      begin match t with
      | D.Fun_type (a, clos) ->
          let v = Ctx.next_var ctx in
          check_exp (Ctx.extend ctx x a v) n (inst_clos clos v)
      | _ ->
          type_error "expected Pi" []
      end
  | _ ->
      if Ctx.eq_val ctx (infer_exp ctx e) t then () else
        type_error "type mismatch" []

and check_type (ctx : Ctx.t) (e : exp) =
  check_exp ctx e D.Type

and infer_exp (ctx : Ctx.t) (e : exp) : D.value =
  match e with
  | Var x ->
      begin match Ctx.lookup ctx x with
      | Some t -> t
      | None -> type_error "unbound variable %s" [x]
      end
  | Let (x, e1, e2, e3) ->
      check_type ctx e2;
      let a = Ctx.eval ctx e2 in
      check_exp ctx e1 a;
      infer_exp (Ctx.extend ctx x a (Ctx.eval ctx e1)) e3
  | Fun_type (x, a, b) ->
      check_type ctx a;
      check_type (Ctx.extend ctx x (Ctx.eval ctx a) (Ctx.next_var ctx)) b;
      D.Type
  | Fun_app (e1, e2) ->
      begin match infer_exp ctx e1 with
      | D.Fun_type (a, clos) ->
          check_exp ctx e2 a;
          inst_clos clos (Ctx.eval ctx e2)
      | _ ->
          type_error "application, expected Pi" []
      end
  | Type -> D.Type
  | _ -> type_error "cannot infer type" []

let type_check (e : exp) (t : exp) =
  check_type Ctx.empty t;
  check_exp Ctx.empty e (eval [] t)

let () =
  let a = Fun_type ("A", Type, Fun_type ("x", Var "A", Var "A")) in
  let m = Fun_abs ("A", Fun_abs ("x", Var "x")) in
  type_check m a
