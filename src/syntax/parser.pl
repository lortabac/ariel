:- module('syntax/parser', [parse_expr/2, expr//1, fun_decl//1, var_decl//1]).

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(src(syntax/lexer)).
:- use_module(src(syntax/operators)).

parse_expr(E, Str) :-
    phrase(tokens(Tokens), Str),
    phrase(expr(E), Tokens).

expr(int(E)) --> [int_lit(E)], !.

expr(string(E)) --> [string_lit(E)], !.

expr(E) --> multi_lambda(E), !.

expr(E) --> lambda(E), !.

expr(E) --> let(E), !.

expr(E) --> app(E), !.

expr(var(E)) --> [id(E)], !.

expr(E) --> between_parens(expr(E)), !.

fun_decl(fun_decl(Name, Args, Body)) --> [id(Name)], var_args(Args), [equal], !, expr(Body), !.

var_decl(var_decl(Name, E)) --> [id(Name), equal], !, expr(E), !.

lambda('=>'([Var], E)) --> [lambda_intro, id(Var)], !, [arrow], expr(E), !.

multi_lambda('=>'(Vars, E)) --> [lambda_intro], var_args(Vars), !, [arrow], expr(E), !.

app('@'(var(E), Args)) --> [id(E)], args(Args), !.
app('@'(E, Args)) --> between_parens(lambda(E)), args(Args), !.
app('@'(E, Args)) --> between_parens(multi_lambda(E)), args(Args), !.
app('@'(E, Args)) --> between_parens(app(E)), args(Args), !.

let(let(Var = S) in E) --> let_var(let(Var = S) in E), !.
let(let(Var = S) in E) --> let_fun(let(Var = S) in E), !.

let_var(let(Var = S) in E) --> [let_intro], var_decl(var_decl(Var, S)), !, [partial_expr_sep], expr(E), !.

let_fun(let(Var = (Args => S)) in E) --> [let_intro], fun_decl(fun_decl(Var, Args, S)), !, [partial_expr_sep], expr(E), !.

between_parens(Parser) --> [open_parens], !, call(Parser), [close_parens], !.

id(I) --> [id(I)], !.

args(Args) --> sequence([open_parens], expr, [arg_sep], [close_parens], Args), !.

var_args(Args) --> sequence([open_parens], id, [arg_sep], [close_parens], Args), !.
