module Source_file = struct

  type t = {
    name : string;
    contents : string;
    lines : (int * int) Dynarray.t;
  }

  let create (name : string) (contents : string) : t =
    let lines = Dynarray.create () in
    let add_line stop =
      match Dynarray.find_last lines with
      | None -> Dynarray.add_last lines (0, stop)
      | Some (_, prev_stop) -> Dynarray.add_last lines (prev_stop + 1, stop)
    in
    contents |> String.iteri (fun pos ch -> if ch = '\n' then add_line pos);
    add_line (String.length contents);

    { name; contents; lines }

  let get_line (source : t) (line : int) : string =
    let start, stop = Dynarray.get source.lines (line - 1) in
    String.sub source.contents start (stop - start)

end

let emit (source : Source_file.t) (severity : string) (start, stop : Lexing.position * Lexing.position) (message : string) =
  let start_line, start_column = start.pos_lnum, start.pos_cnum - start.pos_bol in
  let stop_line, stop_column = stop.pos_lnum, stop.pos_cnum - stop.pos_bol in

  let gutter_num = Int.to_string start_line in
  let gutter_pad = String.map (Fun.const ' ') gutter_num in

  let underline_pad = String.make start_column ' ' in
  let underline =
    if start_line <> stop_line || stop_column <= start_column then "^" else
      String.make (stop_column - start_column) '^'
  in

  Printf.eprintf "%s: %s\n" severity message;
  Printf.eprintf "%s ┌─ %s:%d:%d\n" gutter_pad source.name start_line start_column;
  Printf.eprintf "%s │\n" gutter_pad;
  Printf.eprintf "%s │ %s\n" gutter_num (Source_file.get_line source start_line);
  Printf.eprintf "%s │ %s%s\n" gutter_pad underline_pad underline;
  Printf.eprintf "\n"

let parse_template (source : Source_file.t) : Surface.template =
  let lexbuf = Sedlexing.Utf8.from_string source.contents in
  let lexpos () = Sedlexing.lexing_positions lexbuf in
  Sedlexing.set_filename lexbuf source.name;

  try
    MenhirLib.Convert.Simplified.traditional2revised Parser.template_main
      (Sedlexing.with_tokenizer (Lexer.template_token ()) lexbuf)
  with
  | Lexer.Error message -> emit source "error" (lexpos ()) message; exit 1
  | Parser.Error -> emit source "error" (lexpos ()) "syntax error"; exit 1

let context = ref []
let env = ref []

let define name ty term =
  context := (name, ty) :: !context;
  env := (name, Core.Semantics.eval !env term) :: !env

let fun_lit (name, ty) body = Core.(Fun_lit (name, ty, body (Var name)))

let rec concat = function
  | [] -> Core.Text_lit ""
  | [t] -> t
  | t :: ts -> Core.Prim_app (Test_concat, [t; concat ts])

let node name body =
  concat [
    Text_lit (Format.sprintf "<%s>" name);
    body;
    Text_lit (Format.sprintf "</%s>" name);
  ]

let () = begin

  define "true" Core.Bool_ty Core.(Bool_lit true);
  define "false" Core.Bool_ty Core.(Bool_lit false);

  define "heading1" Core.(Fun_ty (Test_ty, Test_ty)) (fun_lit ("text", Test_ty) (node "h1"));
  define "heading2" Core.(Fun_ty (Test_ty, Test_ty)) (fun_lit ("text", Test_ty) (node "h2"));
  define "heading3" Core.(Fun_ty (Test_ty, Test_ty)) (fun_lit ("text", Test_ty) (node "h3"));
  define "para" Core.(Fun_ty (Test_ty, Test_ty)) (fun_lit ("text", Test_ty) (node "p"));
  define "link" Core.(Fun_ty (Test_ty, Test_ty)) (fun_lit ("text", Test_ty) (node "a"));

end

let context = !context
let env = !env

let () =
  Printexc.record_backtrace true;

  let source = Source_file.create "<stdin>" (In_channel.input_all stdin) in

  match
    parse_template source
    |> Surface.Elab.synth_template context
    |> Core.Semantics.eval env
  with
  | Core.Semantics.Text_lit s -> print_string s
  | _ -> failwith "text literal expected"
  | exception Surface.Elab.Error (pos, msg) ->
      emit source "error" pos msg;
      exit 1
