(** Objects and classes arising from a combination of closures and mutable
    state. Inspired by “Concepts, Techniques, and Models of Computer
    Programming” by Van Roy and Haridi.

    - Peter Van Roy, {{: https://webperso.info.ucl.ac.be/~pvr/paradigms.html}
      Classification of the principal programming paradigms}
    - Peter Van Roy and Seif Haridi, {{: https://ctm.info.ucl.ac.be/} Concepts,
      Techniques, and Models of Computer Programming}
*)

(** Objects from Section 1.13 *)
let () = begin

  let (bump, read) =
    let c = ref 0 in
    let bump () = incr c; !c in
    let read () = !c in
    bump, read
  in

  assert (read () = 0);
  assert (bump () = 1);
  assert (bump () = 2);
  assert (bump () = 3);
  assert (read () = 3);

end

(** Classes from Section 1.14 *)
let () = begin

  let open struct
    type t = {
      bump : unit -> int;
      read : unit -> int;
    }
  end in

  let new_counter () =
    let c = ref 0 in
    let bump () = incr c; !c in
    let read () = !c in
    { bump; read }
  in

  let c1 = new_counter () in

  assert (c1.read () = 0);
  assert (c1.bump () = 1);

  let c2 = new_counter () in

  assert (c2.read () = 0);
  assert (c2.bump () = 1);

  assert (c1.bump () = 2);

  assert (c1.read () = 2);
  assert (c2.read () = 1);

end
