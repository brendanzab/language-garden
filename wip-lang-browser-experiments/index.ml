module Dom = Js_of_ocaml.Dom
module Html = Js_of_ocaml.Dom_html
module Js = Js_of_ocaml.Js

let branch_len = 6.0
let fork_angle = 45.0

let rec draw (ctx : Html.canvasRenderingContext2D Js.t) (tree : Binary_tree.tree) : unit =
  match tree with
  | Apex ->
      begin
        ctx##arc
          (* center_x *) (Js.float 0.0)
          (* center_y *) (Js.float 0.0)
          (* radius *) (Js.float 1.5)
          (* theta1 *) (Js.float 0.0)
          (* theta2 *) (Js.float (2.0 *. Float.pi))
          (* ccw *) Js._false;
        ctx##fill;
      end

  | Fork (tree1, tree2) ->
      begin
        ctx##save;
        ctx##rotate (fork_angle *. Float.pi/.180.0);
        draw ctx tree1;
        ctx##restore;

        ctx##save;
        ctx##rotate (-.fork_angle *. Float.pi/.180.0);
        draw ctx tree2;
        ctx##restore;
      end

  | Branch tree ->
      begin
        ctx##save;

        ctx##beginPath;
        ctx##moveTo 0.0 0.0;
        ctx##lineTo 0.0 (-.branch_len);
        ctx##stroke;
        ctx##translate 0.0 (-.branch_len);

        draw ctx tree;

        ctx##restore;
      end

let init_tree (iters : int) : Binary_tree.tree = begin
  let tree = ref Binary_tree.axiom in
  for _ = 0 to iters do
    tree := Binary_tree.step !tree;
  done;
  !tree
end

let start (_ : (#Html.event as 'b) Js.t) : bool Js.t = begin
  (* Initialise canvas *)
  let canvas = Html.window##.document |> Html.createCanvas in
  canvas##.width := 400;
  canvas##.height := 400;
  Dom.appendChild Html.window##.document##.body canvas;
  let ctx = canvas##getContext Html._2d_ in

  (* Render the tree *)
  begin
    ctx##save;

    ctx##translate (400.0 /. 2.0) 400.0;
    draw ctx (init_tree 5);

    ctx##restore;
  end;

  Js._false
end

let () = begin
  Html.window##.onload :=
    Html.handler start
end
