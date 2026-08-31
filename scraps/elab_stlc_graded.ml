[@@@warning "-unused-constructor"]
[@@@warning "-unused-value-declaration"]

(** Pre-ordered semiring *)
module type Capability = sig

  type t

  val add : t -> t -> t
  val mul : t -> t -> t

  val zero : t
  val one : t

  val lte : t -> t -> bool

  val pp : t -> Format.formatter -> unit

end

(** Unrestricted capability *)
module Unrestricted = struct

  type t = unit

  let add () () = ()
  let mul () () = ()
  let zero = ()
  let one = ()
  let lte () () = true

  let pp () = Format.dprintf "∞"

end

(** Exact usage capability *)
module Exact = struct

  type t = int

  let add = Int.add
  let mul = Int.mul
  let zero = Int.zero
  let one = Int.one
  let lte x y = Int.equal x y

  let pp x = Format.dprintf "%i" x

end

(** Linearity (none-one-tons) capability. Like QTT *)
module Lnl = struct

  type t = Zero | One | Many

  let compare x y =
    match x, y with
    | Zero, Zero -> 0
    | Zero, (One | Many) -> -1

    | One, Zero -> 1
    | One, One -> 0
    | One, Many -> -1

    | Many, (One | Zero) -> 1
    | Many, Many -> 0

  let add x y =
    match x, y with
    | Zero, x | x, Zero -> x
    | _, _ -> Many

  let mul x y =
    match x, y with
    | Zero, x | x, Zero -> Zero
    | One, x | x, One -> x
    | _, _ -> Many

  let zero = Zero
  let one = One

  let lte x y = compare x y < 0

  let pp x =
    match x with
    | Zero -> Format.dprintf "0"
    | One -> Format.dprintf "1"
    | Inf -> Format.dprintf "∞"

end

(** Information flow capability *)
module Security = struct

  type t = Low | High

  let join (* \/ *) x y =
    match x, y with
    | High, High -> failwith "TODO"
    | High, Low -> failwith "TODO"
    | Low, Low -> failwith "TODO"
    | Low, High -> failwith "TODO"

  let meet (* /\ *) x y =
    match x, y with
    | High, High -> failwith "TODO"
    | High, Low -> failwith "TODO"
    | Low, Low -> failwith "TODO"
    | Low, High -> failwith "TODO"

  let add x y = meet x y
  let mul x y = join x y

  let zero = High
  let one = Low

  let lte x y =
    match x, y with
    | High, High -> true
    | High, Low -> true
    | Low, Low -> true
    | Low, High -> false

  let pp x =
    match x with
    | Low -> Format.dprintf "Lo"
    | High -> Format.dprintf "Hi"

end

module Interval (R : Capability) = struct

  type t = R.t * R.t

  let add (r1, r2) (s1, s2) = failwith "TODO"
  let mul (r1, r2) (s1, s2) = failwith "TODO"
  let lte (r1, r2) (s1, s2) = R.lte r1 s1 && R.lte s2 r2

  let zero = failwith "TODO"
  let one = failwith "TODO"

  let pp (r, s) =
    Format.dprintf "(%t..%t)" (R.pp r) (R.pp s)

end

module Product (R : Capability) (S : Capability) = struct

  type t = R.t * S.t

  let add (rx, sx) (ry, sy) = R.add rx ry, S.add sx sy
  let mul (rx, sx) (ry, sy) = R.mul rx ry, S.mul sx sy
  let lte (rx, sx) (ry, sy) = R.lte rx ry, S.lte sx sy

  let zero = R.zero, S.zero
  let one = R.one, S.one

  let pp (r, s) =
    Format.dprintf "%t, %t" (R.pp r) (S.pp s)

end

(** Core language *)
module Core (R : Capability) = struct

  (** Syntax *)

  type ty =
    | Unit_ty : ty
    | Int_ty : ty
    | Fun_ty : R.t * ty * ty -> ty

  type index = int

  type expr =
    | Var : index -> expr
    | Let : ty * expr * expr -> expr
    | Fun_lit : R.t * ty * expr -> expr
    | Fun_app : expr * expr -> expr
    | Unit_lit : expr

end

(** Surface language *)
module Surface = struct

  (** Syntax *)

  type cap =
    | Zero
    | One
    | Inf

  type ty =
    | Name of string
    | Fun_ty of ty * ty

  type expr =
    | Name : string -> expr
    | Ann : expr * ty -> expr
    | Let : (string * ty option * expr) * expr -> expr
    | Fun_lit : (string * ty option) * expr -> expr
    | Fun_app : expr * expr -> expr
    | Unit_lit : expr

  (** Elaboration *)

  module Elab (R : Capability) = struct

    module C = Core (R)

    exception Unbound_var of string
    exception Type_mismatch
    exception Ambiguous_param of string

    type ctx = (string * (R.t * C.ty)) list

    let rec elab_ty : ty -> C.ty =
      function
      | Name "Unit" -> Unit_ty
      | Name name -> raise (Unbound_var name)
      | Fun_ty (arg_ty, body_ty) ->
          let arg_ty = elab_ty arg_ty in
          let body_ty = elab_ty body_ty in
          Fun_ty (_, arg_ty, body_ty)

    let rec check_expr : ctx -> expr -> C.ty -> C.expr =
      fun env expr expected_ty ->
        match expr, expected_ty with
        | Let ((name, def_ty, def_expr), body_expr), body_ty ->
            let def_expr, def_ty = elab_ann_expr env def_expr def_ty in
            let body_expr = check_expr ((name, (_, def_ty)) :: env) body_expr body_ty in
            Let (def_ty, def_expr, body_expr)
        | Fun_lit ((name, None), body_expr), Fun_ty (_, param_ty, body_ty) ->
            let body_expr = check_expr ((name, (_, param_ty)) :: env) body_expr body_ty in
            Fun_lit (_, param_ty, body_expr)
        | _ ->
            let expr, ty = synth_expr env expr in
            if ty = expected_ty then expr else
              raise Type_mismatch

    and synth_expr : ctx -> expr -> C.expr * C.ty =
      fun env expr ->
        match expr with
        | Name name ->
            begin match env |> List.find_mapi (fun i (n, ty) -> if n = name then Some (i, ty) else None) with
            | Some (index, (_, ty)) -> C.Var index, ty
            | None when name = "unit" -> C.Unit_lit, C.Unit_ty
            | None -> raise (Unbound_var name)
            end
        | Ann (expr, ty) ->
            elab_ann_expr env expr (Some ty)
        | Let ((name, def_ty, def_expr), body_expr) ->
            let def_expr, def_ty = elab_ann_expr env def_expr def_ty in
            let body_expr, body_ty = synth_expr ((name, (_, def_ty)) :: env) body_expr in
            Let (def_ty, def_expr, body_expr), body_ty
        | Fun_lit ((name, Some param_ty), body_expr) ->
            let param_ty = elab_ty param_ty in
            let body_expr, body_ty = synth_expr ((name, (_, param_ty)) :: env) body_expr in
            Fun_lit (_, param_ty, body_expr), Fun_ty (_, param_ty, body_ty)
        | Fun_lit ((name, None), _) ->
            raise (Ambiguous_param name)
        | Fun_app (fn_expr, arg_expr) ->
            begin match synth_expr env fn_expr with
            | fn_expr, Fun_ty (_, param_ty, body_ty) ->
                let arg_expr = check_expr env arg_expr param_ty in
                Fun_app (fn_expr, arg_expr), body_ty
            | _ -> raise Type_mismatch
            end
        | Unit_lit ->
            Unit_lit, Unit_ty

    and elab_ann_expr : ctx -> expr -> ty option -> C.expr * C.ty =
      fun env expr ty ->
        match ty with
        | Some ty ->
            let ty = elab_ty ty in
            check_expr env expr ty, ty
        | None -> synth_expr env expr

  end

end

let () = begin

  Printf.printf "Running tests in %s ..." __FILE__;

  assert (Surface.synth_expr [] (Fun_lit (("x", Some (Name "Unit")), Name "x"))
    = (Fun_lit (Unit_ty, Var 0), Fun_ty (Unit_ty, Unit_ty)));

  assert (Surface.synth_expr [] (Fun_lit (("x", Some (Name "Unit")), Fun_lit (("y", Some (Name "Unit")), Name "x")))
    = (Fun_lit (Unit_ty, Fun_lit (Unit_ty, Var 1)), Fun_ty (Unit_ty, Fun_ty (Unit_ty, Unit_ty))));

  assert (Surface.synth_expr [] (Fun_lit (("x", Some (Name "Unit")), Fun_lit (("x", Some (Name "Unit")), Name "x")))
    = (Fun_lit (Unit_ty, Fun_lit (Unit_ty, Var 0)), Fun_ty (Unit_ty, Fun_ty (Unit_ty, Unit_ty))));

  assert (Surface.check_expr [] (Fun_lit (("x", None), Name "x")) (Fun_ty (Unit_ty, Unit_ty))
    = Fun_lit (Unit_ty, Var 0));

  Printf.printf " ok!\n";

end
