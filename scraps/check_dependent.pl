% A small dependent type system, implemented in SWI-Prolog using normalisation-by-evaluation.
%
% Originally posted at https://gist.github.com/brendanzab/6c562b12b51c424f3a93356faeb106ed

:- module(core,
    [   % Operational semantics

        eval/2,         % +Expr, -VExpr
        quote/2,        % +VExpr, -Expr
        normalise/2,    % +Term, -Term

        eval/3,         % +Env, +Expr, -VExpr
        quote/3,        % +Len, +VExpr, -Expr
        normalise/3,    % +Env, +Term, -Term
        convert/3,      % +Len, +VExpr, +VExpr

        % Bidirectional typing

        to_env/2,       % +Ctx, -Env

        check/2,        % +Expr, +VType
        synth/2,        % +Expr, -VType

        check/3,        % +Ctx, +Expr, +VType
        synth/3         % +Ctx, +Expr, -VType
    ]).

:- use_module(library(lists), []).

%% type index == nonneg.
%% type level == nonneg.

%% type term --->
%%     var(index);
%%     let(term, term, term);
%%     type;
%%     fun_type(term, term);
%%     fun_lit(term);
%%     fun_call(term, term);
%%     pair_type(term, term);
%%     pair_lit(term, term);
%%     pair_fst(term);
%%     pair_snd(term).

%% type value --->
%%     neutral(neutral);
%%     type;
%%     fun_type(value, closure);
%%     fun_lit(closure);
%%     pair_type(value, closure);
%%     pair_lit(value, value);
%%     unit_type;
%%     unit_lit.

%% type neutral --->
%%     var(level);
%%     fun_call(neutral, value);
%%     pair_fst(neutral);
%%     pair_snd(neutral).

%% type env == list(value).

%% type closure ---> closure(Env, Expr).

%% type context == list(value-value).


% --------------------------------------------------------------------------------------------------
%
%  Operational semantics
%
% --------------------------------------------------------------------------------------------------


%% eval(+Expr:term, -VExpr:value) is semidet.
eval(Expr, VExpr) :-
    eval([], Expr, VExpr).

%% quote(+VExpr:value, -Expr:term) is semidet.
quote(VExpr, Expr) :-
    quote(0, VExpr, Expr).

%% normalise(+Term:term, -Term:term) is semidet.
normalise(Term1, Term2) :-
    normalise([], Term1, Term2).


%% eval(+Env:env, +Expr:term, -VExpr:value) is semidet.
eval(Env, var(Index), VExpr) :-
    lists:nth0(Index, Env, VExpr).
eval(Env, let(_, ExprDef, Expr), VExpr) :-
    eval(Env, ExprDef, VExprDef),
    eval([VExprDef | Env], Expr, VExpr).
eval(_, type, type).
eval(Env, fun_type(TypeIn, TypeOut), fun_type(VTypeIn, closure(Env, TypeOut))) :-
    eval(Env, TypeIn, VTypeIn).
eval(Env, fun_lit(ExprOut), fun_lit(closure(Env, ExprOut))).
eval(Env, fun_call(Expr, ExprIn), VTypeOut) :-
    eval(Env, Expr, VExpr),
    eval(Env, ExprIn, VExprIn),
    compute_call(VExpr, VExprIn, VTypeOut).
eval(Env, pair_type(TypeFst, TypeSnd), pair_type(VTypeFst, closure(Env, TypeSnd))) :-
    eval(Env, TypeFst, VTypeFst).
eval(Env, pair_lit(ExprFst, ExprSnd), pair_lit(VExprFst, VExprSnd)) :-
    eval(Env, ExprFst, VExprFst),
    eval(Env, ExprSnd, VExprSnd).
eval(Env, pair_fst(Expr), VExprFst) :-
    eval(Env, Expr, VExpr),
    compute_fst(VExpr, VExprFst).
eval(Env, pair_snd(Expr), VExprSnd) :-
    eval(Env, Expr, VExpr),
    compute_snd(VExpr, VExprSnd).

%% apply(+ClosExpr:closure, +VExpr:value, -VExpr:value) is semidet.
apply(closure(Env, Term), VExpr1, VExpr2) :-
    eval([VExpr1 | Env], Term, VExpr2).

%% compute_call(+VExpr:value, +VExpr:value, -VExpr:value) is semidet.
compute_call(fun_lit(ClosExprOut), VExprIn, VExprOut) :-
    apply(ClosExprOut, VExprIn, VExprOut).
compute_call(neutral(NeuExpr), VExprIn, neutral(fun_call(NeuExpr, VExprIn))).

%% compute_fst(+VExpr:value, -VExpr:value) is semidet.
compute_fst(pair_lit(VExprFst, _), VExprFst).
compute_fst(neutral(NeuExpr), neutral(pair_fst(NeuExpr))).

%% compute_snd(+VExpr:value, -VExpr:value) is semidet.
compute_snd(pair_lit(_, VExprSnd), VExprSnd).
compute_snd(neutral(NeuExpr), neutral(pair_snd(NeuExpr))).

%% quote(+Len:nonneg, +VExpr:value, -Expr:term) is semidet.
quote(Len, neutral(NeuExpr), Expr) :-
    quote_neutral(Len, NeuExpr, Expr).
quote(_, type, type).
quote(Len, fun_type(VTypeIn, ClosTypeOut), fun_type(TypeIn, TypeOut)) :-
    quote(Len, VTypeIn, TypeIn),
    apply(ClosTypeOut, neutral(var(Len)), VTypeOut),
    quote(Len + 1, VTypeOut, TypeOut).
quote(Len, fun_lit(ClosExprOut), fun_lit(ExprOut)) :-
    apply(ClosExprOut, neutral(var(Len)), VExprOut),
    quote(Len + 1, VExprOut, ExprOut).
quote(Len, pair_type(VTypeFst, ClosTypeSnd), pair_type(TypeFst, TypeSnd)) :-
    quote(Len, VTypeFst, TypeFst),
    apply(ClosTypeSnd, neutral(var(Len)), VTypeSnd),
    quote(Len + 1, VTypeSnd, TypeSnd).
quote(Len, pair_lit(VExprFst, VExprSnd), pair_lit(ExprFst, ExprSnd)) :-
    quote(Len, VExprFst, ExprFst),
    quote(Len, VExprSnd, ExprSnd).

%% quote_neutral(+Len:nonneg, +NeuExpr:neutral, -Expr:term) is semidet.
quote_neutral(Len, var(Level), var(Len - Level + 1)).
quote_neutral(Len, fun_call(NeuExpr1, VExpr2), fun_call(Expr1, Expr2)) :-
    quote_neutral(Len, NeuExpr1, Expr1),
    quote(Len, VExpr2, Expr2).
quote_neutral(Len, pair_fst(NeuExpr), pair_fst(Expr)) :-
    quote_neutral(Len, NeuExpr, Expr).
quote_neutral(Len, pair_snd(NeuExpr), pair_snd(Expr)) :-
    quote_neutral(Len, NeuExpr, Expr).

%% normalise(+Env:env, +Term1:term, -Term2:term) is semidet.
normalise(Env, Term1, Term2) :-
    eval(Env, Term1, VTerm1),
    lists:length(Env, Len),
    quote(Len, VTerm1, Term2).

%% convert(+Len:nonneg, +VExpr1:value, +VExpr2:value) is semidet.
convert(Len, neutral(NeuExpr1), neutral(NeuExpr2)) :-
    convert_neutral(Len, NeuExpr1, NeuExpr2).
convert(_, type, type).
convert(Len, fun_type(VTypeIn1, ClosTypeOut1), fun_type(VTypeIn2, ClosTypeOut2)) :-
    convert(Len, VTypeIn1, VTypeIn2),
    apply(ClosTypeOut1, neutral(var(Len)), VTypeOut1),
    apply(ClosTypeOut2, neutral(var(Len)), VTypeOut2),
    convert(Len + 1, VTypeOut1, VTypeOut2).
convert(Len, fun_lit(ClosExprOut1), fun_lit(ClosExprOut2)) :-
    apply(ClosExprOut1, neutral(var(Len)), VExprOut1),
    apply(ClosExprOut2, neutral(var(Len)), VExprOut2),
    convert(Len + 1, VExprOut1, VExprOut2).
convert(Len, VExpr1, fun_lit(ClosExprOut2)) :-
    compute_call(VExpr1, neutral(var(Len)), VExprOut1),
    apply(ClosExprOut2, neutral(var(Len)), VExprOut2),
    convert(Len, VExprOut1, VExprOut2).
convert(Len, fun_lit(ClosExprOut1), VExpr2) :-
    apply(ClosExprOut1, neutral(var(Len)), VExprOut1),
    compute_call(VExpr2, neutral(var(Len)), VExprOut2),
    convert(Len, VExprOut1, VExprOut2).
convert(Len, pair_type(VTypeFst1, ClosTypeSnd1), pair_type(VTypeFst2, ClosTypeSnd2)) :-
    convert(Len, VTypeFst1, VTypeFst2),
    apply(ClosTypeSnd1, neutral(var(Len)), VTypeSnd1),
    apply(ClosTypeSnd2, neutral(var(Len)), VTypeSnd2),
    convert(Len + 1, VTypeSnd1, VTypeSnd2).
convert(Len, pair_lit(VExprFst1, VExprSnd1), pair_lit(VExprFst2, VExprSnd2)) :-
    convert(Len, VExprFst1, VExprFst2),
    convert(Len, VExprSnd1, VExprSnd2).
convert(Len, VExpr1, pair_lit(VExprFst2, VExprSnd2)) :-
    compute_fst(VExpr1, VExprFst1),
    convert(Len, VExprFst1, VExprFst2),
    compute_snd(VExpr1, VExprSnd1),
    convert(Len, VExprSnd1, VExprSnd2).
convert(Len, pair_lit(VExprFst1, VExprSnd1), VExpr2) :-
    compute_fst(VExpr2, VExprFst2),
    convert(Len, VExprFst1, VExprFst2),
    compute_snd(VExpr2, VExprSnd2),
    convert(Len, VExprSnd1, VExprSnd2).

%% convert_neutral(+Len:nonneg, +NeuExpr1:neutral, +NeuExpr2:neutral) is semidet.
convert_neutral(_, var(Level1), var(Level2)) :- Level1 =:= Level2.
convert_neutral(Len, fun_call(NeuExpr1, VExprIn1), fun_call(NeuExpr2, VExprIn2)) :-
    convert_neutral(Len, NeuExpr1, NeuExpr2),
    convert(Len, VExprIn1, VExprIn2).
convert_neutral(Len, pair_fst(NeuExpr1), pair_fst(NeuExpr2)) :-
    convert_neutral(Len, NeuExpr1, NeuExpr2).
convert_neutral(Len, pair_snd(NeuExpr1), pair_snd(NeuExpr2)) :-
    convert_neutral(Len, NeuExpr1, NeuExpr2).


% --------------------------------------------------------------------------------------------------
%
%  Bidirectional typing
%
% --------------------------------------------------------------------------------------------------


%% to_env(+Ctx:context, -Env:env) is semidet.
to_env([], []).
to_env([Expr-_ | Ctx], [Expr | Env]) :-
    to_env(Ctx, Env).


%% check(+Expr:term, +VType:value) is semidet.
check(Expr, VType) :-
    check([], Expr, VType).

%% synth(+Expr:term, -VType:value) is semidet.
synth(Expr, VType) :-
    synth([], Expr, VType).


%% check(+Ctx:context, +Expr:term, +VType:value) is semidet.
check(Ctx, let(TypeDef, ExprDef, Expr), VType) :-
    check(Ctx, TypeDef, type),
    to_env(Ctx, Env),
    eval(Env, TypeDef, VTypeDef),
    check(Ctx, ExprDef, VTypeDef),
    eval(Env, ExprDef, VExprDef),
    check([VExprDef-VTypeDef | Ctx], Expr, VType).
check(Ctx, fun_lit(OutExpr), fun_type(VTypeIn, ClosTypeOut)) :-
    lists:length(Ctx, Len),
    apply(ClosTypeOut, neutral(var(Len)), VTypeOut),
    check([neutral(var(Len))-VTypeIn | Ctx], OutExpr, VTypeOut).
check(Ctx, pair_lit(ExprFst, ExprSnd), pair_type(VTypeFst, ClosTypeSnd)) :-
    check(Ctx, ExprFst, VTypeFst),
    to_env(Ctx, Env),
    eval(Env, ExprFst, VExprFst),
    apply(ClosTypeSnd, VExprFst, VTypeSnd),
    check([VExprFst-VTypeFst | Ctx], ExprSnd, VTypeSnd).
check(Ctx, Expr, VType2) :-
    synth(Ctx, Expr, VType1),
    lists:length(Ctx, Len),
    convert(Len, VType1, VType2).

%% synth(+Ctx:context, +Expr:term, -VType:value) is semidet.
synth(Ctx, var(Index), VType) :-
    lists:nth0(Index, Ctx, _-VType).
synth(Ctx, let(TypeDef, ExprDef, Expr), VType) :-
    check(Ctx, TypeDef, type),
    to_env(Ctx, Env),
    eval(Env, TypeDef, VTypeDef),
    check(Ctx, ExprDef, VTypeDef),
    eval(Env, ExprDef, VExprDef),
    synth([VExprDef-VTypeDef | Ctx], Expr, VType).
synth(_, type, type).
synth(Ctx, fun_type(TypeIn, TypeOut), type) :-
    check(Ctx, TypeIn, type),
    to_env(Ctx, Env),
    eval(Env, TypeIn, VTypeIn),
    lists:length(Ctx, Len),
    check([neutral(var(Len))-VTypeIn | Ctx], TypeOut, type).
synth(Ctx, fun_call(Expr, ExprIn), VTypeOut) :-
    synth(Ctx, Expr, fun_type(VTypeIn, ClosTypeOut)),
    check(Ctx, ExprIn, VTypeIn),
    to_env(Ctx, Env),
    eval(Env, ExprIn, VExprIn),
    apply(ClosTypeOut, VExprIn, VTypeOut).
synth(Ctx, pair_type(TypeFst, TypeSnd), type) :-
    check(Ctx, TypeFst, type),
    to_env(Ctx, Env),
    eval(Env, TypeFst, VTypeFst),
    lists:length(Ctx, Len),
    check([neutral(var(Len))-VTypeFst | Ctx], TypeSnd, type).
synth(Ctx, pair_fst(Expr), VTypeFst) :-
    synth(Ctx, Expr, pair_type(VTypeFst, _)).
synth(Ctx, pair_snd(Expr), VTypeSnd) :-
    synth(Ctx, Expr, pair_type(_, ClosTypeSnd)),
    to_env(Ctx, Env),
    eval(Env, Expr, VExpr),
    compute_fst(VExpr, VExprFst),
    apply(ClosTypeSnd, VExprFst, VTypeSnd).


% --------------------------------------------------------------------------------------------------
%
%  Parsing
%
% --------------------------------------------------------------------------------------------------


% TODO: Lexer is kind of broken lol
% TODO: Pass name environment with EDCG

whitespace --> " ".
whitespace --> "\n".
whitespace --> "\r".
whitespace --> "\t".

alpha(C) :- between(0'a, 0'z, C); between(0'A, 0'Z, C).
alnum(C) :- between(0'a, 0'z, C); between(0'A, 0'Z, C); between(0'0, 0'Z, 1).

name_start(C)    --> [C], { alpha(C) }.
name_continue(C) --> [C], { alnum(C); C =:= 0'- }.

token("field")    --> "field".
token("let")      --> "let".
token("param")    --> "param".
token("Type")     --> "Type".
token("(")        --> "(".
token(")")        --> ")".
token("{")        --> "}".
token(";")        --> ";".
token(".")        --> ".".
token("->")       --> "->".
token("1")        --> "1".
token("2")        --> "2".
% token(name([Start | Rest])) -->
%     name_start(Start), name_continue(Rest).

tokens(Tokens) -->
    whitespace, tokens(Tokens).
tokens([Token | Tokens]) -->
    token(Token), tokens(Tokens).

%% term(-Term:term)// is semidet.
term(Term) -->
    term([], Term).

%% term(+Names:list(string), -Term:term)// is semidet.
term(Names, let(Type1, Expr1, Expr2))           --> ["let", name(Name), ":"], term(Names, Type1), ["="],
                                                    term(Names, Expr1), [";"], term([Name | Names], Expr2).
term(Names, Term)                               --> term_arrow(Names, Term).

term_arrow(Names, fun_type(Type1, Type2))       --> ["param", name(Name), ":"], term(Names, Type1), [";"],
                                                    term_arrow([Name | Names], Type2).
term_arrow(Names, fun_type(Type1, Type2))       --> term_call(Names, Type1), ["->"], term_arrow([nil | Names], Type2).
term_arrow(Names, fun_lit(Expr))                --> ["param", name(Name), ";"], term_arrow([Name | Names], Expr).
term_arrow(Names, pair_type(Type1, Type2))      --> ["field", name(Name), ":"], term(Names, Type1), [";"],
                                                    term_arrow([Name | Names], Type2).
term_arrow(Names, pair_lit(ExprFst, ExprSnd))   --> ["field"], term(Names, ExprFst), [";"],
                                                    term_arrow(Names, ExprSnd).
                                                    % TODO: field x = y; x
term_arrow(Names, Term)                         --> term_call(Names, Term).

term_call(Names, fun_call(Expr1, Expr2))        --> term_call(Names, Expr1), term_atomic(Names, Expr2).
term_call(Names, Term)                          --> term_atomic(Names, Term).

term_atomic(Names, Term)                        --> ["("], term(Names, Term), [")"].
term_atomic(Names, var(Index))                  --> [name(Name)], { lists:nth0(Index, Names, Name) }.
term_atomic(_, type)                            --> ["Type"].
term_atomic(Names, pair_fst(Expr))              --> term_atomic(Names, Expr), [".", "1"].
term_atomic(Names, pair_snd(Expr))              --> term_atomic(Names, Expr), [".", "2"].

%% parse_term(+String:string, -Term:term) is semidet.
parse_term(String, Term) :-
    atom_chars(String, Codes),
    phrase(tokens(Tokens), Codes),
    term(Term, Tokens, []).
