(** A naive implementation of Datalog, originally based on
    {{:https://dodisturb.me/posts/2018-12-25-The-Essence-of-Datalog.html}
     The Essence of Datalog} by Mistral Contrastin.
*)

(** {1 Abstract syntax} *)

(** Constant terms. *)
type const =
  | String of string
  | Int of int

(** Terms that are used in the arguments of atomic symbols. *)
type term =
  | Var of string
  (** Variables. *)

  | Const of const
  (** Constants. *)

(** Atomic symbols. These are considered {i ground} if none of the [terms]
    are variables. *)
type atom = {
  name : string;
  (** The predicate symbol of the atom. *)

  args : term list;
  (** The arguments supplied to the atom. *)
}

(** A definite clause. *)
type rule = {
  head : atom;
  (** The {i head} or {i consequent} of a rule. *)

  body : atom list;
  (** The {i body} or {i premises} of a rule. *)
}

(** The list of known facts about the universe. Each atom in this list is
    assumed to be {i ground}. *)
type knowledge_base = atom list

(** A full datalog program, consisting of a list of rules. *)
type program = {
  rules : rule list;
  queries : atom list list;
}

(** {2 Pretty printing} *)

let pp_print_const (ppf : Format.formatter) : const -> unit =
  function
  | String s -> Format.fprintf ppf "\"%s\"" s
  | Int i -> Format.fprintf ppf "%i" i

let pp_print_term (ppf : Format.formatter) : term -> unit =
  function
  | Var v -> Format.pp_print_string ppf v
  | Const c -> pp_print_const ppf c

let pp_print_atom (ppf : Format.formatter) : atom -> unit =
  function
  | { name; args = [] } ->
      Format.pp_print_string ppf name
  | { name; args } ->
      let pp_sep ppf () = Format.fprintf ppf ",@ " in
      Format.fprintf ppf "@[%s(%a)@]" name
        (Format.pp_print_list ~pp_sep pp_print_term) args

let pp_print_rule (ppf : Format.formatter) : rule -> unit =
  function
  | { head; body = [] } ->
      Format.fprintf ppf "@[%a.@]"
        pp_print_atom head
  | { head; body } ->
      let pp_sep ppf () = Format.fprintf ppf ",@ " in
      Format.fprintf ppf "@[<2>@[%a@ <-@]@ %a.@]"
        pp_print_atom head
        (Format.pp_print_list ~pp_sep pp_print_atom) body

let pp_print_query (ppf : Format.formatter) (query : atom list) : unit =
  let pp_sep ppf () = Format.fprintf ppf ",@ " in
  Format.fprintf ppf "@[<2>?@ %a.@]"
    (Format.pp_print_list ~pp_sep pp_print_atom) query


module List : sig
  (** Extensions to the {!Stdlib.List} module. *)

  include module type of List

  val remove_dupes : 'a list -> 'a list
  (** [remove_dupes xs] removes all the duplicate elements from [xs]. *)

end = struct

  include List

  let remove_dupes (l : 'a list) : 'a list =
    (* https://stackoverflow.com/a/58352698/679485 *)
    let rec go acc seen = function
        | [] -> List.rev acc
        | a :: rest when List.mem a seen -> go acc seen rest
        | a :: rest -> (go [@tailcall]) (a :: acc) (a :: seen) rest
    in
    go [] [] l

end


module Subst : sig
  (** A list of substitutions from variable names to constants. *)

  type t
  (** A substitution that maps variables to constants. *)

  val empty : t
  (** The empty substitution *)

  val extend : (string * const) -> t -> t
  (** [extend (v, c) subst] adds new variable-to-constant mapping to [subst]. *)

  val append : t -> t -> t
  (** [append subst0 subst1] merges the mappings in [subst0] with the mappings
      in [subst1]. *)

  val lookup : string -> t -> const option
  (** [lookup v subst] finds the corresponding constant mapped to the variable
      name [v] in [subst], if it exists. *)

  val apply : atom -> t -> atom
  (** [apply atom subst] replace the variables in [atom] with the substituted
      constants in [subst].

      Applying a substitution may make the atom {i ground} if all the variables
      in [atom] have a mapping in [const].
  *)

end = struct

  type t = (string * const) list

  let empty : t = []

  let extend (v, c) subst =
    (v, c) :: subst

  let append subst0 subst1 =
    subst0 @ subst1

  let lookup = List.assoc_opt

  let apply atom subst =
    let go : term -> term =
      function
      | Const _ as const -> const
      | Var v as var ->
          begin match lookup v subst with
          | Some c -> Const c
          | None -> var
          end
    in
    { atom with
      args = List.map go atom.args;
    }

end


(** Unification between a body atom and a ground atom *)
let unify (atom0 : atom) (atom1 : atom) : Subst.t option =
  let rec go (terms0 : term list) (terms1 : term list) : Subst.t option =
    match terms0, terms1 with
    | [], _ | _, [] -> Some Subst.empty
    | Const c0 :: rest0, Const c1 :: rest1 ->
        if c0 = c1 then go rest0 rest1 else None
    | Var v0 :: rest0, Const c1 :: rest1 ->
        Option.bind (go rest0 rest1)
          (fun subst ->
            match Subst.lookup v0 subst with
            | Some c0 when c0 <> c1 -> None
            | _ -> Some (Subst.extend (v0, c1) subst))
    | _ :: _, Var v :: _ ->
        (* Safe because the second atom was expected to be ground *)
        failwith ("the second atom `" ^ v ^ "` is assumed to be ground")
  in
  if atom0.name = atom1.name then
    go atom0.args atom1.args
  else
    None

(** Find facts that match the given atom and collect assignments to its
    variables as a list of substitutions. *)
let eval_atom (kb : knowledge_base) (atom : atom) (substs : Subst.t list) : Subst.t list =
  (* Use a sequence? *)
  let ( let* ) = Fun.flip List.concat_map in

  (* Get the next substitution from the list of accumulated substitutions. *)
  let* subst = substs in
  (* Update the atom with any relevant mappings found in [subst]. *)
  let atom' = Subst.apply atom subst in
  (* Unify the updated atom with each fact in the knowledge base. This is safe
     because each atom in the knowledge base should be ground. *)
  let* extension = List.filter_map (unify atom') kb in

  [Subst.append subst extension]

(** Accumulate a list of substitutions for each atom in the body. *)
let eval_body (kb : knowledge_base) (atoms : atom list) : Subst.t list =
  List.fold_right (eval_atom kb) atoms [Subst.empty]

(** Deduce new facts from the body based on the head of the rule.
    This assumes that the rule is range restricted. *)
let eval_rule (kb : knowledge_base) (r : rule) : knowledge_base =
  (* Apply the head to each substitution produced *)
  List.map (Subst.apply r.head) (eval_body kb r.body)

(** [is_range_restricted rule] enforces the {i range restriction} by returning
    [true] if every variable in the head of the rule appears somewhere in the
    body of the rule.

    Checking that each rule obeys this property ensures that the knowledge base
    always contains grounded atoms.
*)
let is_range_restricted (r : rule) : bool =
  (* TODO: Return a list of the variables that do not exist in the body, for
     better error reporting *)
  let is_subset_of xs ys = List.for_all (fun x -> List.mem x ys) xs in
  let vars atom =
    atom.args
      |> List.filter (function Var _ -> true | _ -> false)
      |> List.remove_dupes
  in
  is_subset_of (vars r.head) (List.concat_map vars r.body)

(** Evaluate each rule independently and then combines the newly derived facts
    with what we already know. *)
let immediate_consequence (rs : rule list) (kb : knowledge_base) : knowledge_base =
  List.remove_dupes (kb @ List.concat_map (eval_rule kb) rs)
  (*                      ^^^^^^^^^^^^^^^ Could be done in parallel? *)

(** Add some new rules to the knowledge base. *)
let add_rules (rs : rule list) (kb : knowledge_base) : knowledge_base =
  (* Evaluate the rules over and over until a fixed point is reached (i.e. no
     new facts are added to the knowledge base). *)
  let rec go (kb : knowledge_base) =
    let kb' = immediate_consequence rs kb in
    if kb = kb' then kb else (go [@tailcall]) kb'
  in
  if List.for_all is_range_restricted rs then
    go kb
  else
    (* TODO: Improve errors *)
    failwith "the input program is not range-restricted"

(** Produce a new knowledge base by evaluating the rules over and over until no
    new facts are added. *)
let solve (rs : rule list) : knowledge_base =
  add_rules rs []

(** Run a query against the knowledge base, returning a list of possible
    variable bindings from the query. *)
let run_query (query : atom list) (kb : knowledge_base) : (term * term) list option =
  (* Collect the free variables in the query *)
  let free_vars =
    query
      |> List.concat_map (fun atom ->
          List.filter (function Var _ -> true | _ -> false) atom.args)
      |> List.remove_dupes
  in
  let bindings =
    add_rules [{ head = { name = "$query"; args = free_vars }; body = query; }] kb
      |> List.filter (fun atom -> atom.name = "$query")
      |> List.map (fun atom -> List.combine free_vars atom.args)
  in
  match bindings with
  | [] -> None
  | bindings -> Some (List.flatten bindings)
