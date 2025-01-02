type state = {
  fill_style : Diagram.style;
  stroke_style : Diagram.style;
}

let default_state = {
  fill_style =`none;
  stroke_style = `none;
}


module Core = struct

  type t = state -> Buffer.t -> int -> unit

  let indent : t =
    fun _ buf level ->
      for _ = 1 to (level * 2) do
        Buffer.add_char buf ' ';
      done

  let empty : t =
    fun _ _ _-> ()

  let over dia1 dia2 : t =
    fun state buf level -> begin
      dia2 state buf level;
      dia1 state buf level;
    end

  let circle ~diameter : t =
    fun state buf level -> begin
      indent state buf level;
      Buffer.add_string buf "<circle";
      Buffer.add_string buf (Printf.sprintf " r=\"%f\"" (diameter *. 0.5));
      (match state.fill_style with
        | `solid -> Buffer.add_string buf " fill=\"black\""
        | `none -> ());
      (match state.stroke_style with
        | `solid -> Buffer.add_string buf " stroke=\"black\""
        | `none -> ());
      Buffer.add_string buf "/>\n";
    end

  let line (x1, y1) (x2, y2) : t =
    fun state buf level -> begin
      indent state buf level;
      Buffer.add_string buf "<line ";
      Buffer.add_string buf (Printf.sprintf " x1=\"%f\"" x1);
      Buffer.add_string buf (Printf.sprintf " y1=\"%f\"" y1);
      Buffer.add_string buf (Printf.sprintf " x2=\"%f\"" x2);
      Buffer.add_string buf (Printf.sprintf " y2=\"%f\"" y2);
      (match state.stroke_style with
        | `solid -> Buffer.add_string buf " stroke=\"black\""
        | `none -> ());
      Buffer.add_string buf "/>\n";
    end

  let stroke style dia : t =
    fun state ctx ->
      dia { state with stroke_style = style } ctx

  let fill style dia =
    fun state ctx ->
      dia { state with fill_style = style } ctx

  let rotate ~radians dia : t =
    fun state buf level -> begin
      indent state buf level;
      Buffer.add_string buf "<g";
      Buffer.add_string buf (Printf.sprintf " transform=\"rotate(%f)\"" (radians *. 180.0 /. Float.pi));
      Buffer.add_string buf ">\n";

      dia state buf (level + 1);

      indent state buf level;
      Buffer.add_string buf "</g>\n";
    end

  let translate (dx, dy) dia : t =
    fun state buf level -> begin
      indent state buf level;
      Buffer.add_string buf "<g";
      Buffer.add_string buf (Printf.sprintf " transform=\"translate(%f, %f)\"" dx dy);
      Buffer.add_string buf ">\n";

      dia state buf (level + 1);

      indent state buf level;
      Buffer.add_string buf "</g>\n";
    end

  let scale xy dia : t =
    fun state buf level -> begin
      indent state buf level;
      Buffer.add_string buf "<g";
      Buffer.add_string buf (Printf.sprintf " transform=\"scale(%f)\"" xy);
      Buffer.add_string buf ">\n";

      dia state buf (level + 1);

      Buffer.add_string buf "</g>\n";
    end

end

include Diagram.Make (Core)

let run ~view_box:(minx, miny, width, height) (dia : t) : string =
  let buf = Buffer.create 256 in
  Buffer.add_string buf "<svg";
  Buffer.add_string buf (Printf.sprintf " viewBox=\"%f %f %f %f\"" minx miny width height);
  Buffer.add_string buf " xmlns=\"http://www.w3.org/2000/svg\"";
  Buffer.add_string buf ">\n";
  dia default_state buf 1;
  Buffer.add_string buf "</svg>\n";
  Buffer.to_bytes buf |> Bytes.to_string
