(** Compiling de Bruijn indexed lambda terms to freshly named lambda terms in
    A-normal form *)

[@@@warning "-unused-value-declaration"]

(** De Bruijn indexed lambda terms *)
module Core = struct

  (** De Bruijn index*)
  type index = int

  (** De Bruijn level*)
  type level = int

  type tm =
    | Var of index
    | Let of string * tm * tm
    | Fun_lit of string * tm
    | Fun_app of tm * tm
    | Int_lit of int
    | Bool_lit of bool
    | Bool_elim of tm * tm * tm
    | Prim_app of string * tm list

  (* TODO: Pretty printing *)

  module Build : sig

    type 'a t

    val run : 'a t -> 'a

    val let' : string * tm t -> (tm t -> tm t) -> tm t
    val fun_lit : string -> (tm t -> tm t) -> tm t
    val fun_app : tm t -> tm t -> tm t
    val int_lit : int -> tm t
    val bool_lit : bool -> tm t
    val bool_elim : tm t -> tm t -> tm t -> tm t
    val prim_app : string -> tm t list -> tm t

  end = struct

    type 'a t = size:level -> 'a

    let run (type a) (x : a t) : a =
      x ~size:0

    let var (level : level) : tm t =
      fun ~size ->
        Var (size - level - 1)

    let let' (name, def) (body : tm t -> tm t) : tm t =
      fun ~size ->
        Let (name, def ~size,
          body (var size) ~size:(size + 1))

    let fun_lit name (body : tm t -> tm t) : tm t =
      fun ~size ->
        Fun_lit (name,
          body (var size) ~size:(size + 1))

    let fun_app (fn : tm t) (arg : tm t) : tm t =
      fun ~size ->
        Fun_app (fn ~size, arg ~size)

    let int_lit (i : int) : tm t =
      fun ~size:_ ->
        Int_lit i

    let bool_lit (b : bool) : tm t =
      fun ~size:_ ->
        Bool_lit b

    let bool_elim (cond : tm t) (true_branch : tm t) (false_branch : tm t) : tm t =
      fun ~size ->
        Bool_elim (cond ~size, true_branch ~size, false_branch ~size)

    let prim_app (name : string) (args : tm t list) : tm t =
      fun ~size ->
        Prim_app (name, args |> List.map (fun arg -> arg ~size))

  end

end


(** Freshly named lambda terms in A-normal form *)
module Anf = struct

  module Id : sig
    type t
    val fresh : unit -> t
  end = struct

    type t = int

    let next_id = ref 0

    let fresh () =
      let id = !next_id in
      incr next_id;
      id

  end

  type tm =
    | Let_comp of string * Id.t * comp_tm * tm
    | Let_join of string * Id.t * (string * Id.t) * tm * tm
    | Join_app of Id.t * atomic_tm
    | Bool_elim of atomic_tm * tm * tm
    | Comp of comp_tm

  and comp_tm =
    | Fun_app of atomic_tm * atomic_tm
    | Prim_app of string * atomic_tm list
    | Atom of atomic_tm

  and atomic_tm =
    | Var of Id.t
    | Fun_lit of string * Id.t * tm
    | Int_lit of int
    | Bool_lit of bool

  (* TODO: Pretty printing *)

end


module Anf_conv : sig

  val translate : Core.tm -> Anf.tm

end = struct

  type 'a k = 'a -> Anf.tm

  let comp_k : Anf.comp_tm k =
    fun tm -> Comp tm

  let join_app_k (id : Anf.Id.t) : Anf.atomic_tm k =
    fun expr -> Join_app (id, expr)

  let ( let@ ) : type a. a k -> a k = ( @@ )

  (** Translate a term to A-normal form *)
  let rec translate (src_env : Anf.Id.t list) (src_tm : Core.tm) : Anf.comp_tm k k =
    fun k ->
      match src_tm with
      | Core.Var src_index ->
          k (Anf.Atom (Var (List.nth src_env src_index)))
      | Core.Let (def_name, src_def, src_body) ->
          let def_id = Anf.Id.fresh () in
          let@ tgt_def = translate src_env src_def in
          let tgt_body = translate (def_id :: src_env) src_body k in
          Anf.Let_comp (def_name, def_id, tgt_def, tgt_body)
      | Core.Fun_lit (param_name, src_body) ->
          let param_id = Anf.Id.fresh () in
          let tgt_body = translate (param_id :: src_env) src_body comp_k in
          k (Anf.Atom (Fun_lit (param_name, param_id, tgt_body)))
      | Core.Fun_app (src_fn, src_arg) ->
          let@ tgt_fn = translate_def src_env "fn" src_fn in
          let@ tgt_arg = translate_def src_env "arg" src_arg in
          k (Anf.Fun_app (tgt_fn, tgt_arg))
      | Core.Int_lit i ->
          k (Anf.Atom (Int_lit i))
      | Core.Bool_lit b ->
          k (Anf.Atom (Bool_lit b))
      | Core.Bool_elim (src_cond, src_true_branch, src_false_branch) ->
          let@ tgt_cond = translate_def src_env "cond" src_cond in
          let cont_id = Anf.Id.fresh () in
          let param_id = Anf.Id.fresh () in
          Anf.Let_join ("cont", cont_id, ("param", param_id),
            k (Anf.Atom (Var param_id)),
            Anf.Bool_elim (tgt_cond,
              translate_def src_env "true-branch" src_true_branch (join_app_k cont_id),
              translate_def src_env "false-branch" src_false_branch (join_app_k cont_id)))
      | Core.Prim_app (name, src_args) ->
          let@ tgt_args = translate_defs src_env "arg" src_args in
          k (Anf.Prim_app (name, tgt_args))

  (** Translate a term to A-normal form, binding it to an intermediate definition if needed. *)
  and translate_def (src_env : Anf.Id.t list) (name : string) (src_tm : Core.tm) : Anf.atomic_tm k k =
    fun k ->
      let@ tgt_tm = translate src_env src_tm in
      match tgt_tm with
      | Anf.Atom tm -> k tm
      | tgt_tm ->
          let id = Anf.Id.fresh () in
          Anf.Let_comp (name, id, tgt_tm, k (Anf.Var id))

  (** Translate a sequence of terms, binding them to intermediate definitions if needed. *)
  and translate_defs (src_env : Anf.Id.t list) (name : string) (src_tms : Core.tm list) : Anf.atomic_tm list k k =
    fun k ->
      match src_tms with
      | [] -> k []
      | src_tm :: src_tms ->
          let@ tm = translate_def src_env name src_tm in
          let@ tms = translate_defs src_env name src_tms in
          k (tm :: tms)

  let translate (src_tm : Core.tm) : Anf.tm =
    translate [] src_tm comp_k

end


(* TODO: Tests *)
