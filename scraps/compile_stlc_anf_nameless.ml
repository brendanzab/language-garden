(** Compiling de Bruijn indexed lambda terms to de Bruijn indexed terms in
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


(** De Bruijn indexed lambda terms in A-normal form *)
module Anf = struct

  (** De Bruijn index*)
  type index = int

  (** De Bruijn level*)
  type level = int

  type tm =
    | Let_comp of string * comp_tm * tm
    | Let_join of string * string * tm * tm
    | Join_app of index * atomic_tm
    | Bool_elim of atomic_tm * tm * tm
    | Comp of comp_tm

  and comp_tm =
    | Fun_app of atomic_tm * atomic_tm
    | Prim_app of string * atomic_tm list
    | Atom of atomic_tm

  and atomic_tm =
    | Var of index
    | Fun_lit of string * tm
    | Int_lit of int
    | Bool_lit of bool

  (* TODO: Pretty printing *)

  module Build : sig

    type 'a t

    val run : 'a t -> 'a

    val let_comp : string * comp_tm t -> (atomic_tm t -> tm t) -> tm t
    val let_join : string * string * (atomic_tm t -> tm t) -> ((atomic_tm t -> tm t) -> tm t) -> tm t
    val fun_lit : string -> (atomic_tm t -> tm t) -> atomic_tm t
    val fun_app : atomic_tm t -> atomic_tm t -> comp_tm t
    val int_lit : int -> atomic_tm t
    val bool_lit : bool -> atomic_tm t
    val bool_elim : atomic_tm t -> tm t -> tm t -> tm t
    val prim_app : string -> atomic_tm t list -> comp_tm t

    val comp : comp_tm t -> tm t
    val atom : atomic_tm t -> comp_tm t

  end = struct

    type 'a t = size:level -> 'a

    let run (type a) (x : a t) : a =
      x ~size:0

    let var (level : level) : atomic_tm t =
      fun ~size ->
        Var (size - level - 1)

    let join_app (level : level) (arg : atomic_tm t) : tm t =
      fun ~size ->
        Join_app (size - level - 1, arg ~size)

    let let_comp (name, def) (body : atomic_tm t -> tm t) : tm t =
      fun ~size ->
        Let_comp (name, def ~size,
          body (var size) ~size:(size + 1))

    let let_join (name, param_name, def) (body : (atomic_tm t -> tm t) -> tm t) : tm t =
      fun ~size ->
        Let_join (name, param_name, def (var size) ~size,
          body (join_app size) ~size:(size + 1))

    let fun_lit name (body : atomic_tm t -> tm t) : atomic_tm t =
      fun ~size ->
        Fun_lit (name,
          body (var size) ~size:(size + 1))

    let fun_app (fn : atomic_tm t) (arg : atomic_tm t) : comp_tm t =
      fun ~size ->
        Fun_app (fn ~size, arg ~size)

    let int_lit (i : int) : atomic_tm t =
      fun ~size:_ ->
        Int_lit i

    let bool_lit (b : bool) : atomic_tm t =
      fun ~size:_ ->
        Bool_lit b

    let bool_elim (cond : atomic_tm t) (true_branch : tm t) (false_branch : tm t) : tm t =
      fun ~size ->
        Bool_elim (cond ~size, true_branch ~size, false_branch ~size)

    let prim_app (name : string) (args : atomic_tm t list) : comp_tm t =
      fun ~size ->
        Prim_app (name, args |> List.map (fun arg -> arg ~size))

    let comp (tm : comp_tm t) : tm t =
      fun ~size -> Comp (tm ~size)

    let atom (tm : atomic_tm t) : comp_tm t =
      fun ~size -> Atom (tm ~size)

  end

end


module Anf_conv : sig

  val translate : Core.tm -> Anf.tm

end = struct

  module B = Anf.Build

  type 'a k = 'a -> Anf.tm B.t

  let comp_k : Anf.comp_tm B.t k =
    fun tm -> B.comp tm

  let ( let@ ) : type a. a k -> a k = ( @@ )

  (** Translate a term to A-normal form *)
  let rec translate (src_env : Anf.atomic_tm B.t list) (src_tm : Core.tm) : Anf.comp_tm B.t k k =
    fun k ->
      match src_tm with
      | Core.Var src_index ->
          k B.(atom (List.nth src_env src_index))
      | Core.Let (def_name, src_def, src_body) ->
          let@ tgt_def = translate src_env src_def in
          B.let_comp (def_name, tgt_def) @@ fun tgt_def ->
            translate (tgt_def :: src_env) src_body k
      | Core.Fun_lit (param_name, src_body) ->
          k B.(atom (fun_lit param_name @@ fun tgt_param ->
            translate (tgt_param :: src_env) src_body comp_k))
      | Core.Fun_app (src_fn, src_arg) ->
          let@ tgt_fn = translate_def src_env "fn" src_fn in
          let@ tgt_arg = translate_def src_env "arg" src_arg in
          k B.(fun_app tgt_fn tgt_arg)
      | Core.Int_lit i ->
          k B.(atom (int_lit i))
      | Core.Bool_lit b ->
          k B.(atom (bool_lit b))
      | Core.Bool_elim (src_cond, src_true_branch, src_false_branch) ->
          let@ join_app = B.let_join ("cont", "param", fun tm -> k B.(atom tm)) in
          let@ tgt_cond = translate_def src_env "cond" src_cond in
          B.bool_elim tgt_cond
            (translate_def src_env "true-branch" src_true_branch join_app)
            (translate_def src_env "false-branch" src_false_branch join_app)
      | Core.Prim_app (name, src_args) ->
          let@ tgt_args = translate_defs src_env "arg" src_args in
          k (B.prim_app name tgt_args)

  and translate_def (src_env : Anf.atomic_tm B.t list) (name : string) (src_tm : Core.tm) : Anf.atomic_tm B.t k k =
    fun k ->
      let@ tgt_tm = translate src_env src_tm in
      B.let_comp (name, tgt_tm) k
      (*
          (* FIXME: Inspecting this term seems difficult *)
          match tgt_tm with
          | Anf.Atom tm -> k tm
          | tgt_tm ->
              B.let' (name, tgt_tm) k
      *)

  and translate_defs (src_env : Anf.atomic_tm B.t list) (name : string) (src_tms : Core.tm list) : Anf.atomic_tm B.t list k k =
    fun k ->
      match src_tms with
      | [] -> k []
      | src_tm :: src_tms ->
          let@ tm = translate_def src_env name src_tm in
          let@ tms = translate_defs src_env name src_tms in
          k (tm :: tms)

  let translate (src_tm : Core.tm) : Anf.tm =
    B.run (translate [] src_tm comp_k)

end


(* TODO: Tests *)
