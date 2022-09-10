%token <int> NUMBER
%token KEYWORD_ELSE "else"
%token KEYWORD_FALSE "false"
%token KEYWORD_IF "if"
%token KEYWORD_THEN "then"
%token KEYWORD_TRUE "true"
%token ADD "+"
%token ASTERISK "*"
%token EQUALS "="
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
| if_term

let if_term :=
| "if"; t1 = eq_term; "then"; t2 = eq_term; "else"; t3 = if_term;
    { TreeLang.if_then_else t1 t2 t3 }
| eq_term

let eq_term :=
| t1 = add_term; "="; t2 = eq_term;
    { TreeLang.eq t1 t2 }
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
| "true";
    { TreeLang.bool true }
| "false";
    { TreeLang.bool false }
| "-"; t = atomic_term;
    { TreeLang.neg t }
