(** Computing packed and aligned data structure layouts.

    Resources:

    - https://en.wikipedia.org/wiki/Data_structure_alignment
    - https://cs.brown.edu/courses/csci0300/2021/notes/l05.html
    - https://developer.ibm.com/articles/pa-dalign/
    - https://doc.rust-lang.org/reference/type-layout.html#r-layout.repr.c.struct.size-field-offset
    - https://doc.rust-lang.org/core/alloc/struct.Layout.html#method.extend
    - https://doc.rust-lang.org/nomicon/other-reprs.html#reprc
    - https://doc.rust-lang.org/nomicon/other-reprs.html#reprpacked-reprpackedn
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

  (* let print_layout ~offsets (layout : Layout.t) =
    Printf.printf "{ size = %i; align = %i }; offsets = [" layout.size layout.align;
    offsets |> List.iteri (function 0 -> Printf.printf "%i" | _ -> Printf.printf "; %i");
    Printf.printf "]\n";
  in *)

  (* Documentation examples *)

  begin
    let layout, ~offsets = Layout.aligned_struct Target.X86.[i16; i16; i64] in
    (* print_layout layout ~offsets; *)
    assert (layout = { size = 12; align = 4 });
    assert (offsets = Layout.[0; 2; 4]);
  end;

  begin
    let layout, ~offsets = Layout.packed_struct Target.Aarch64.[i16; i16; i64] in
    (* print_layout layout ~offsets; *)
    assert (layout = { size = 12; align = 1 });
    assert (offsets = Layout.[0; 2; 4]);
  end;

  begin
    let layout, ~offsets = Layout.aligned_struct Target.Aarch64.[i16; i16; i64] in
    (* print_layout layout ~offsets; *)
    assert (layout = { size = 16; align = 8 });
    assert (offsets = Layout.[0; 2; 8]);
  end;

  (* x86 *)

  begin
    let layout, ~offsets = Layout.aligned_struct Target.X86.[i8; i8] in
    (* print_layout layout ~offsets; *)
    assert (layout = { size = 2; align = 1 });
    assert (offsets = Layout.[0; 1]);
  end;

  begin
    let layout, ~offsets = Layout.aligned_struct Target.X86.[i8; i16; i8] in
    (* print_layout layout ~offsets; *)
    assert (layout = { size = 6; align = 2 });
    assert (offsets = Layout.[0; 2; 4]);
  end;

  begin
    let layout, ~offsets = Layout.aligned_struct Target.X86.[i64; i32] in
    (* print_layout layout ~offsets; *)
    assert (layout = { size = 12; align = 4 });
    assert (offsets = Layout.[0; 8]);
  end;

  begin
    let layout, ~offsets = Layout.aligned_struct Target.X86.[i8; i8; i64] in
    (* print_layout layout ~offsets; *)
    assert (layout = { size = 12; align = 4 });
    assert (offsets = Layout.[0; 1; 4]);
  end;

  begin
    let layout, ~offsets = Layout.aligned_struct Target.X86.[i16; i8; i32; i64; i32] in
    (* print_layout layout ~offsets; *)
    assert (layout = { size = 20; align = 4 });
    assert (offsets = Layout.[0; 2; 4; 8; 16]);
  end;

  (* AArch64 *)

  begin
    let layout, ~offsets = Layout.aligned_struct Target.Aarch64.[i64; i32] in
    (* print_layout layout ~offsets; *)
    assert (layout = { size = 16; align = 8 });
    assert (offsets = Layout.[0; 8]);
  end;

  begin
    let layout, ~offsets = Layout.aligned_struct Target.Aarch64.[i8; i8; i64] in
    (* print_layout layout ~offsets; *)
    assert (layout = { size = 16; align = 8 });
    assert (offsets = Layout.[0; 1; 8]);
  end;

  begin
    let layout, ~offsets = Layout.aligned_struct Target.Aarch64.[i16; i8; i32; i64; i32] in
    (* print_layout layout ~offsets; *)
    assert (layout = { size = 24; align = 8 });
    assert (offsets = Layout.[0; 2; 4; 8; 16]);
  end;

  begin
    let layout, ~offsets = Layout.aligned_struct Target.Aarch64.[i8; i32; i64] in
    (* print_layout layout ~offsets; *)
    assert (layout = { size = 16; align = 8 });
    assert (offsets = [0; 4; 8]);
  end;

  (* ... *)

end
