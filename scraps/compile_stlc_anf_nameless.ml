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
    | Prim_app of string * tm list

  (* TODO: Pretty printing *)

  module Build : sig

    type 'a t

    val run : 'a t -> 'a

    val let' : string * tm t -> (tm t -> tm t) -> tm t
    val fun_lit : string -> (tm t -> tm t) -> tm t
    val fun_app : tm t -> tm t -> tm t
    val int_lit : int -> tm t
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
    | Let of string * comp_tm * tm
    | Comp of comp_tm

  and comp_tm =
    | Fun_app of atomic_tm * atomic_tm
    | Prim_app of string * atomic_tm list
    | Atom of atomic_tm

  and atomic_tm =
    | Var of index
    | Fun_lit of string * tm
    | Int_lit of int

  (* TODO: Pretty printing *)

  module Build : sig

    type 'a t

    val run : 'a t -> 'a

    val let' : string * comp_tm t -> (atomic_tm t -> tm t) -> tm t
    val fun_lit : string -> (atomic_tm t -> tm t) -> atomic_tm t
    val fun_app : atomic_tm t -> atomic_tm t -> comp_tm t
    val int_lit : int -> atomic_tm t
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

    let let' (name, def) (body : atomic_tm t -> tm t) : tm t =
      fun ~size ->
        Let (name, def ~size,
          body (var size) ~size:(size + 1))

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

  type 'a k = 'a -> Anf.tm Anf.Build.t

  let comp_k : Anf.comp_tm Anf.Build.t k =
    fun tm -> Anf.Build.comp tm

  let ( let@ ) : type a. a k -> a k = ( @@ )

  (** Translate a term to A-normal form *)
  let rec translate (src_env : Anf.atomic_tm Anf.Build.t list) (src_tm : Core.tm) : Anf.comp_tm Anf.Build.t k k =
    fun k ->
      match src_tm with
      | Core.Var src_index ->
          k Anf.Build.(atom (List.nth src_env src_index))
      | Core.Let (def_name, src_def, src_body) ->
          let@ tgt_def = translate src_env src_def in
          Anf.Build.let' (def_name, tgt_def) @@ fun tgt_def ->
            translate (tgt_def :: src_env) src_body k
      | Core.Fun_lit (param_name, src_body) ->
          k Anf.Build.(atom (fun_lit param_name @@ fun tgt_param ->
            translate (tgt_param :: src_env) src_body comp_k))
      | Core.Fun_app (src_fn, src_arg) ->
          (* FIXME: Use [translate_def] to avoid intermediate bindings *)
          (* let@ tgt_fn = translate_def src_env "fn" src_fn in *)
          (* let@ tgt_arg = translate_def src_env "arg" src_arg in *)
          (* k Anf.Build.(fun_app tgt_fn tgt_arg) *)
          let@ tgt_fn = translate src_env src_fn in
          let@ tgt_arg = translate src_env src_arg in
          Anf.Build.let' ("fn", tgt_fn) @@ fun tgt_fn ->
            Anf.Build.let' ("arg", tgt_arg) @@ fun tgt_arg ->
              k Anf.Build.(fun_app tgt_fn tgt_arg)
      | Core.Int_lit i ->
          k Anf.Build.(atom (int_lit i))
      | Core.Prim_app (name, src_args) ->
          (* FIXME: Use [translate_defs] to avoid intermediate bindings *)
          let rec go src_args tgt_args =
            match src_args with
            | [] -> k Anf.Build.(prim_app name (List.rev tgt_args))
            | src_arg :: src_args ->
                let@ tgt_arg = translate src_env src_arg in
                Anf.Build.let' ("arg", tgt_arg) @@ fun tgt_arg ->
                  go src_args (tgt_arg :: tgt_args)
          in
          go src_args []

  (*
  and translate_def (src_env : Anf.atomic_tm Anf.Build.t list) (name : string) (src_tm : Core.tm) : Anf.atomic_tm Anf.Build.t k k =
    fun k ->
      let@ tgt_tm = translate src_env src_tm in
      (* FIXME: Inspecting this term seems difficult *)
      match tgt_tm with
      | Anf.Atom tm -> k tm
      | tgt_tm ->
          Anf.Build.let' (name, tgt_tm) k
  *)

  let translate (src_tm : Core.tm) : Anf.tm =
    Anf.Build.run (translate [] src_tm comp_k)

end


(* TODO: Tests *)
