(** Type inference for the simply typed lambda calculus using a union-find data
    structure. *)

(** Union find data structure with path compression and linking-by-rank *)
module Union_find : sig

  type 'a elem

  val make : 'a -> 'a elem
  val get : 'a elem -> 'a
  val set : 'a elem -> 'a -> unit
  val eq : 'a elem -> 'a elem -> bool
  val union : 'a elem -> 'a elem -> 'a elem

end = struct

  type rank = int

  type 'a elem =
    'a content ref

  and 'a content =
    | Link of 'a elem
    | Root of rank * 'a

  let make v =
    ref (Root (0, v))

  let rec find x =
    match !x with
    | Root (_, _) -> x
    | Link y ->
        let z = find y in
        x := Link z;
        z

  let get x =
    let x = find x in
    match !x with
    | Root (_, v) -> v
    | Link _ -> assert false

  let set x v =
    let x = find x in
    match !x with
    | Root (r, _) -> x := Root (r, v)
    | Link _ -> assert false

  let link x y =
    if x == y then x else
      match !x, !y with
      | Root (rx, vx), Root (ry, _) when rx < ry -> x := Link y; y
      | Root (rx, vx), Root (ry, _) when rx > ry -> y := Link x; x
      | Root (rx, vx), Root (ry, _) -> y := Link x; x := Root (rx + 1, vx); x
      | _, _ -> assert false

  let union x y =
    link (find x) (find y)

  let eq x y =
    find x == find y

end

module Type = struct

  type t =
    | Meta of meta
    | Fun of t * t
    | Int

  and meta =
    meta_state Union_find.elem

  and meta_state =
    | Solved of t
    | Unsolved

end

module Expr = struct

  type t =
    | Var of string
    | Int of int
    | Lam of string * t
    | App of t * t

end

module Typing : sig

  val unify : Type.t -> Type.t -> unit
  val infer : Expr.t -> Type.t

end = struct

  let metas : Type.meta Dynarray.t =
    Dynarray.create ()

  let fresh_meta () : Type.t =
    let m = Union_find.make Type.Unsolved in
    Dynarray.add_last metas m;
    Type.Meta m

  let rec occurs (m : Type.meta) (t : Type.t) : unit =
    match t with
    | Type.Meta m' ->
        occurs_meta m m'
    | Type.Fun (t1, t2) ->
        occurs m t1;
        occurs m t2
    | Type.Int -> ()

  and occurs_meta (m : Type.meta) (m' : Type.meta) : unit =
    if Union_find.eq m m' then
      failwith "occurs"
    else
      match Union_find.get m' with
      | Solved t -> occurs m t
      | Unsolved -> ()

  let rec unify (t1 : Type.t) (t2 : Type.t) : unit =
    match t1, t2 with
    | Type.Meta m, t | t, Type.Meta m ->
        occurs m t;
        Union_find.set m (Type.Solved t)
    | Type.Int, Int -> ()
    | Type.Fun (t1_1, t2_1), Type.Fun (t1_2, t2_2) ->
        unify t1_1 t1_2;
        unify t2_1 t2_2
    | _, _ ->
        failwith "unify"

  let rec infer (ctx : (string * Type.t) list) (e : Expr.t) : Type.t =
    match e with
    | Expr.Var x -> List.assoc x ctx
    | Expr.Int _ -> Type.Int
    | Expr.Lam (x, body) ->
        infer ((x, fresh_meta ()) :: ctx) body
    | Expr.App (fn, arg) ->
        let fn_t = infer ctx fn in
        let param_t = infer ctx arg in
        let body_t = fresh_meta () in
        unify fn_t (Type.Fun (param_t, body_t));
        body_t

  let infer (e : Expr.t) : Type.t =
    let t = infer [] e in
    metas |> Dynarray.iter @@ begin fun m ->
      match Union_find.get m with
      | Type.Solved _ -> ()
      | Type.Unsolved -> failwith "ambiguous"
    end;
    Dynarray.clear metas;
    t

end
