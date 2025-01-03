type state = {
  fill_style : string option;
  stroke_style : string option;
}

let default_state = {
  fill_style = None;
  stroke_style = None;
}

module Core = struct

  type t = state -> int -> out_channel -> unit

  let indent : t =
    fun _ level oc ->
      for _ = 1 to level do
        Printf.fprintf oc "  ";
      done

  let empty : t =
    fun _ _ _-> ()

  let over dia1 dia2 : t =
    fun state level oc -> begin
      dia2 state level oc;
      dia1 state level oc;
    end

  let circle ~diameter : t =
    fun state level oc -> begin
      Printf.fprintf oc "%t<circle" (indent state level);
      Printf.fprintf oc " r=\"%f\"" (diameter *. 0.5);
      state.fill_style |> Option.iter (Printf.fprintf oc " fill=\"%s\"");
      state.stroke_style |> Option.iter (Printf.fprintf oc " stroke=\"%s\"");
      Printf.fprintf oc "/>\n";
    end

  let line (x1, y1) (x2, y2) : t =
    fun state level oc -> begin
      Printf.fprintf oc "%t<line " (indent state level);
      Printf.fprintf oc " x1=\"%f\"" x1;
      Printf.fprintf oc " y1=\"%f\"" y1;
      Printf.fprintf oc " x2=\"%f\"" x2;
      Printf.fprintf oc " y2=\"%f\"" y2;
      state.stroke_style |> Option.iter (Printf.fprintf oc " stroke=\"%s\"");
      Printf.fprintf oc "/>\n";
    end

  let to_svg_style style =
    match style with
    | `solid -> Some "black"
    | `none -> None

  let stroke style dia : t =
    fun state ctx ->
      dia { state with stroke_style = to_svg_style style } ctx

  let fill style dia =
    fun state ctx ->
      dia { state with fill_style = to_svg_style style } ctx

  let rotate ~radians dia : t =
    fun state level oc -> begin
      Printf.fprintf oc "%t<g" (indent state level);
      Printf.fprintf oc " transform=\"rotate(%f)\"" (radians *. 180.0 /. Float.pi);
      Printf.fprintf oc ">\n";

      dia state (level + 1) oc;

      Printf.fprintf oc "%t</g>\n" (indent state level);
    end

  let translate (dx, dy) dia : t =
    fun state level oc -> begin
      Printf.fprintf oc "%t<g" (indent state level);
      Printf.fprintf oc " transform=\"translate(%f, %f)\"" dx dy;
      Printf.fprintf oc ">\n";

      dia state (level + 1) oc;

      Printf.fprintf oc "%t</g>\n" (indent state level);
    end

  let scale xy dia : t =
    fun state level oc -> begin
      Printf.fprintf oc "%t<g" (indent state level);
      Printf.fprintf oc " transform=\"scale(%f)\"" xy;
      Printf.fprintf oc ">\n";

      dia state (level + 1) oc;

      Printf.fprintf oc "%t</g>\n" (indent state level);
    end

end

include Diagram.Make (Core)

let run ~view_box:(minx, miny, width, height) (dia : t) (oc : out_channel) =
  Printf.fprintf oc "<svg";
  Printf.fprintf oc " viewBox=\"%f %f %f %f\"" minx miny width height;
  Printf.fprintf oc " xmlns=\"http://www.w3.org/2000/svg\"";
  Printf.fprintf oc ">\n";
  dia default_state 1 oc;
  Printf.fprintf oc "</svg>\n";
