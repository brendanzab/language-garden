%token <int> NUMBER
%token ADD "+"
%token HYPHEN "-"
%token OPEN_PAREN "("
%token CLOSE_PAREN ")"
%token END

%start <TreeLang.term> main

%%

let main :=
| t = term; END;
    { t }

let term :=
| t1 = atomic_term; "+"; t2 = term;
    { TreeLang.Add (t1, t2) }
| t1 = atomic_term; "-"; t2 = term;
    { TreeLang.Sub (t1, t2) }
| atomic_term

let atomic_term :=
| "("; t = term; ")";
    { t }
| n = NUMBER;
    { TreeLang.Num n }
