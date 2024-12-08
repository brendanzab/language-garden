%token <string> LOWER_NAME
%token <string> UPPER_NAME
%token <string> STRING
%token <int> NUMBER
%token COMMA ","
%token DOT "."
%token GREATER_HYPHEN "<-"
%token QUESTION "?"
%token OPEN_PAREN "("
%token CLOSE_PAREN ")"
%token END

%start <Datalog.program> program

%%

let program :=
| items = list(item); END;
    { List.fold_right ( @@ ) items Datalog.{ rules = []; queries = [] } }

let item :=
| q = query; { fun p -> Datalog.{ p with queries = q :: p.queries } }
| r = rule; { fun p -> Datalog.{ p with rules = r :: p.rules } }

let query :=
| "?"; body = separated_list(",", atom); ".";
    { body }

let rule :=
| head = atom; ".";
    { Datalog.{ head; body = [] } }
| head = atom; "<-"; body = separated_list(",", atom); ".";
    { Datalog.{ head; body } }

let atom :=
| name = LOWER_NAME;
    { Datalog.{ name; args = [] } }
| name = LOWER_NAME; args = delimited("(", separated_list(",", term) ,")");
    { Datalog.{ name; args } }

let term :=
| v = UPPER_NAME;
    { Datalog.Var v }
| c = const;
    { Datalog.Const c }

let const :=
| s = STRING;
    { Datalog.String s }
| i = NUMBER;
    { Datalog.Int i }
