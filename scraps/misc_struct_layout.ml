(** Computing packed and aligned data structure layouts.

    Resources:

    - https://en.wikipedia.org/wiki/Data_structure_alignment
    - https://cs.brown.edu/courses/csci0300/2021/notes/l05.html
    - https://developer.ibm.com/articles/pa-dalign/
    - https://doc.rust-lang.org/reference/type-layout.html#r-layout.repr.c.struct.size-field-offset
    - https://doc.rust-lang.org/core/alloc/struct.Layout.html#method.extend
    - https://doc.rust-lang.org/nomicon/other-reprs.html
    - https://en.cppreference.com/c/language/object#Alignment
    - https://compcert.org/doc/html/compcert.cfrontend.Ctypes.html
*)

module Layout = struct

  type t = {
    size : int;   (* ≥ 0 *)
    align : int;  (* ≥ 1 *)
  }

  (** Computes a packed struct layout, along with a list of field offsets. This
      could result in unaligned primitive values that require multiple reads to
      access. For example:

      {@text[

        { x : I16; y : I16; z : I64 }

        0   1   2   3   4   5   6   7   8   9   10  11  12
        ╎                               ╎
        ┌───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┐
        │ x     │ y     │ z                             │
        └───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┘
        ╎                 ▲             ╎
                          │
                          └─── this field lies across a storage boundary, which
                               could negatively impact performance

      ]}
  *)
  let packed_struct (fields : t list) : t * offsets:int list =
    let rec go offset offsets (fields : t list) =
      match fields with
      | field :: fields -> go (offset + field.size) (offset :: offsets) fields
      | [] -> { size = offset; align = 1 }, ~offsets:(List.rev offsets)
    in

    go 0 [] fields

  (** Computes an aligned, C-style struct layout, along with a list of field
      offsets. Padding is added between the fields in order to prevent them from
      crossing storage boundaries.

      {@text[

        { x : I16; y : I16; z : I64 }

        0   1   2   3   4   5   6   7   8   9   10  11  12  13  14  15  16
        ╎                               ╎                               ╎
        ┌───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┐
        │ x     │ y     │ padding       │ z                             │
        └───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┘
        ╎                 ▲             ╎                               ╎
                          │
                          └─── padding to align `z` with a storage boundary

      ]}

      Note that this implementation does not handle edge cases such as integer
      overflow, etc.
  *)
  let aligned_struct (fields : t list) : t * offsets:int list =
    (* The padding to add between the current offset and a field of the given
       alignment. Based on https://en.wikipedia.org/wiki/Data_structure_alignment#Computing_padding *)
    let padding (offset : int) (align : int) : int =
      (align - (offset mod align)) mod align
    in

    (* Based on https://doc.rust-lang.org/reference/type-layout.html#r-layout.repr.c.struct.size-field-offset *)
    let rec go offset align (offsets : int list) (fields : t list) =
      match fields with
      | field :: fields ->
          let align = Int.max align field.align in
          let offset = offset + padding offset field.align in
          go (offset + field.size) align (offset :: offsets) fields
      | [] ->
          (* Add additional padding at the end of the struct *)
          let size = offset + padding offset align in
          { size; align }, ~offsets:(List.rev offsets)
    in

    go 0 1 [] fields

end

(** Target specifications *)
module Target = struct

  module type S = sig

    val ptr : Layout.t

    val i8 : Layout.t
    val i16 : Layout.t
    val i32 : Layout.t
    val i64 : Layout.t
    val i128 : Layout.t

    val f32 : Layout.t
    val f64 : Layout.t
    val f128 : Layout.t

  end

  (* https://llvm.org/docs/LangRef.html#data-layout *)

  (* Aligned to 32-bit word boundaries (like x86) *)
  module X86 : S = struct

    let ptr = Layout.{ size = 4; align = 4 }

    let i8 = Layout.{ size = 1; align = 1 }
    let i16 = Layout.{ size = 2; align = 2 }
    let i32 = Layout.{ size = 4; align = 4 }
    let i64 = Layout.{ size = 8; align = 4 }
    let i128 = Layout.{ size = 16; align = 4 }

    let f32 = Layout.{ size = 4; align = 4 }
    let f64 = Layout.{ size = 8; align = 4 }
    let f128 = Layout.{ size = 16; align = 4 }

  end

  (* Aligned to 128-bit word boundaries (like x86-64 or AArch64) *)
  module Aarch64 : S = struct

    let ptr = Layout.{ size = 8; align = 8 }

    let i8 = Layout.{ size = 1; align = 1 }
    let i16 = Layout.{ size = 2; align = 2 }
    let i32 = Layout.{ size = 4; align = 4 }
    let i64 = Layout.{ size = 8; align = 8 }
    let i128 = Layout.{ size = 16; align = 16 }

    let f32 = Layout.{ size = 4; align = 4 }
    let f64 = Layout.{ size = 8; align = 8 }
    let f128 = Layout.{ size = 16; align = 16 }

  end

end


let () = begin

  Printexc.record_backtrace true;

  let print_layout ~offsets (layout : Layout.t) =
    Printf.printf "{ size = %i; align = %i }; offsets = [" layout.size layout.align;
    offsets |> List.iteri (function 0 -> Printf.printf "%i" | _ -> Printf.printf "; %i");
    Printf.printf "]\n";
  in

  (* Documentation examples *)

  begin
    let layout, ~offsets = Layout.packed_struct Target.Aarch64.[i16; i16; i64] in
    assert (layout = { size = 12; align = 1 });
    assert (offsets = [0; 2; 4]);
  end;

  begin
    let layout, ~offsets = Layout.aligned_struct Target.Aarch64.[i16; i16; i64] in
    assert (layout = { size = 16; align = 8 });
    assert (offsets = [0; 2; 8]);
  end;

  (* More examples *)

  begin
    let layout, ~offsets = Layout.aligned_struct [] in
    assert (layout = { size = 0; align = 1 });
    assert (offsets = []);
  end;

  begin
    let layout, ~offsets = Layout.aligned_struct Target.Aarch64.[i8; i64; i16] in
    assert (layout = { size = 24; align = 8 });
    assert (offsets = [0; 8; 16]);
  end;

  begin
    let layout, ~offsets = Layout.aligned_struct Target.Aarch64.[i64; i8; i16] in
    assert (layout = { size = 16; align = 8 });
    assert (offsets = [0; 8; 10]);
  end;

  (* Randomised tests *)

  let test_aligned_struct (fields : Layout.t list) =
    let layout, ~offsets = Layout.aligned_struct fields in

    try
      (* The size of a structure should be padded to a a multiple of its alignment *)
      assert (layout.size mod layout.align = 0);

      let rec check_fields fields offsets =
        match fields, offsets with
        | (field : Layout.t) :: fields, offset :: offsets ->
            assert (offset mod field.align = 0);    (* Offset fields to a multiple of their alignment *)
            assert (field.align <= layout.align);   (* Alignment of fields should not exceed the max alignment *)

            begin match offsets with
            | next_offset :: _ ->
                assert (offset + field.size <= next_offset);  (* Fields should not overlap *)
                assert (offset <= next_offset);               (* Offsets should increase monotonically *)
            | [] ->
                assert (offset + field.size <= layout.size);  (* Final field should not exceed the size of the structure *)
            end;

        | [], [] -> ()
        | [], _ :: _ | _ :: _, [] ->
            assert false; (* Mismatched lengths *)
      in

      check_fields fields offsets
    with
    | Assert_failure _ as exn ->
        print_layout layout ~offsets;
        Printexc.(raise_with_backtrace exn (get_raw_backtrace ()));
  in

  let random_layout (module T : Target.S) =
    match Random.int 9 with
    | 0 -> T.ptr
    | 1 -> T.i8
    | 2 -> T.i16
    | 3 -> T.i32
    | 4 -> T.i64
    | 5 -> T.i128
    | 6 -> T.f32
    | 7 -> T.f64
    | 8 -> T.f128
    | _ -> failwith "unreachable"
  in

  let random_fields (module T : Target.S) =
    List.init (Random.int 100) (fun _ -> random_layout (module T))
  in

  Target.[(module X86); (module Aarch64)] |> List.iter begin fun (module T : Target.S) ->
    Seq.forever (fun () -> random_fields (module T))
    |> Seq.take 100
    |> Seq.iter test_aligned_struct;
  end;

end
