let context = ref []
let env = ref []

let define name ty term =
  context := (name, ty) :: !context;
  env := (name, Core.Semantics.eval !env term) :: !env

let fun_lit (name, ty) body = Core.(FunLit (name, ty, body (Var name)))

let rec concat = function
  | [] -> Core.TextLit ""
  | [t] -> t
  | t :: ts -> Core.TextConcat (t, concat ts)

let node name body =
  Core.(concat [
    TextLit (Format.sprintf "<%s>" name);
    body;
    TextLit (Format.sprintf "</%s>" name);
  ])

let () =

  define "true" Core.Bool Core.(BoolLit true);
  define "false" Core.Bool Core.(BoolLit false);

  define "heading1" Core.(Fun (Text, Text)) (fun_lit ("text", Text) (node "h1"));
  define "heading2" Core.(Fun (Text, Text)) (fun_lit ("text", Text) (node "h2"));
  define "heading3" Core.(Fun (Text, Text)) (fun_lit ("text", Text) (node "h3"));
  define "para" Core.(Fun (Text, Text)) (fun_lit ("text", Text) (node "p"));
  define "link" Core.(Fun (Text, Text)) (fun_lit ("text", Text) (node "a"));

  ()

let context = !context
let env = !env

let () =
  Printexc.record_backtrace true;

  let tm =
    In_channel.input_all stdin
    |> Surface.Parser.parse_template
    |> Result.get_ok
    |> Surface.elab_template context
  in

  match Core.Semantics.eval env tm with
  | Core.Semantics.TextLit s -> print_string s
  | _ -> failwith "unexpected value"
