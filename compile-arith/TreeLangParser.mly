%token <int> NUMBER
%token ADD "+"
%token ASTERISK "*"
%token FORWARD_SLASH "/"
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
| add_term

let add_term :=
| t1 = mul_term; "+"; t2 = add_term;
    { TreeLang.add t1 t2 }
| t1 = mul_term; "-"; t2 = add_term;
    { TreeLang.sub t1 t2 }
| mul_term

let mul_term :=
| t1 = atomic_term; "*"; t2 = mul_term;
    { TreeLang.mul t1 t2 }
| t1 = atomic_term; "/"; t2 = mul_term;
    { TreeLang.div t1 t2 }
| atomic_term

let atomic_term :=
| "("; t = term; ")";
    { t }
| n = NUMBER;
    { TreeLang.num n }
| "-"; t = atomic_term;
    { TreeLang.neg t }
