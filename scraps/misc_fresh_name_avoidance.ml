(** Fresh name generation that attempts to create the best possible name
    while avoiding previously defined names. This is useful for implementing
    compiler backends.
*)

module State : sig

  type t

  val create :
    ?format:(string -> int -> string) ->
    ?names:string list ->
    unit -> t

  val fresh : t -> string -> string
  val copy : t -> t

  (* TODO: What properties do we expect of [format] to prevent infinite loops
     when calling [fresh] *)

end = struct

  type t = {
    format : string -> int -> string;
    counts : (string, int) Hashtbl.t;
  }

  let default_format (name : string) (n : int) : string =
    if n = 0 then name else
      Printf.sprintf "%s_%i" name n

  let create ?(format = default_format) ?(names = []) () : t =
    let counts = List.to_seq names |> Seq.map (fun n -> n, 1) |> Hashtbl.of_seq in
    { format; counts }

  let rec fresh (state : t) (base_name : string) : string =
    (* Search for a name that’s not currently in use *)
    let rec go count =
      let candidate_name = state.format base_name count in
      if Hashtbl.mem state.counts candidate_name then
        go (count + 1)
      else begin
        Hashtbl.add state.counts candidate_name 1;
        Hashtbl.replace state.counts base_name count;
        candidate_name
      end
    in
    go (Hashtbl.find_opt state.counts base_name |> Option.value ~default:0)

  let copy (state : t) : t =
    { format = state.format;
      counts = Hashtbl.copy state.counts;
    }

end


(* Default formatting *)
let () = begin

  Printexc.record_backtrace true;

  let keywords = ["if"; "then"; "else"] in

  let state = State.create () ~names:keywords in

  (* Avoid keywords *)
  assert (State.fresh state "if" = "if_1");
  assert (State.fresh state "if" = "if_2");
  assert (State.fresh state "then" = "then_1");
  assert (State.fresh state "else" = "else_1");

  (* Simple avoidance *)
  assert (State.fresh state "x" = "x");
  assert (State.fresh state "x" = "x_1");
  assert (State.fresh state "x" = "x_2");

  (* Avoid avoided names *)
  assert (State.fresh state "x_1" = "x_1_1");
  assert (State.fresh state "x_1" = "x_1_2");
  assert (State.fresh state "x_2" = "x_2_1");
  assert (State.fresh state "x_2" = "x_2_2");

  (* Skip previous names when freshening *)
  assert (State.fresh state "y_2" = "y_2");
  assert (State.fresh state "y" = "y");
  assert (State.fresh state "y" = "y_1");
  assert (State.fresh state "y" = "y_3");

end


(* Subscript formatting *)
let () = begin

  Printexc.record_backtrace true;

  let keywords = ["if"; "then"; "else"] in

  let subscript_of_char c =
    let subs : _ Iarray.t = [| "₀"; "₁"; "₂"; "₃"; "₄"; "₅"; "₆"; "₇"; "₈"; "₉" |] in
    try Iarray.get subs Char.(code c - code '0') with
    | Invalid_argument _ -> String.make 1 c
  in

  let format base n =
    let s = string_of_int n in
    let b = Buffer.create (String.length s * 2) in
    s |> String.iter (fun c -> Buffer.add_string b (subscript_of_char c));
    base ^ (Buffer.to_bytes b |> Bytes.to_string)
  in

  let state = State.create () ~format ~names:keywords in

  assert (State.fresh state "if" = "if₁");
  assert (State.fresh state "x" = "x₀");
  assert (State.fresh state "x" = "x₁");
  assert (State.fresh state "x₁" = "x₁₁");
  assert (State.fresh state "x₁" = "x₁₂");
  assert (State.fresh state "x" = "x₂");

end
