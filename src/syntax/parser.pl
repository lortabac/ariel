:- module('syntax/parser', [parse_expr/2, parse_expr_or_decl/2, parse_type/2]).

:- use_module(library(apply)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(src(syntax/lexer)).
:- use_module(src(syntax/operators)).

%!  parse_expr_or_decl(-X, +Str:list) is det.
%
%   Parse an Ariel expression or declaration.

parse_expr_or_decl(X, Str) :-
    parse_expr_or_decl_(X, Str), !;
    throw(error(parsing_error)).

parse_expr_or_decl_(X, Str) :-
    phrase(tokens(Tokens), Str),
    phrase(expr_or_decl(X), Tokens).

%!  parse_expr(-E:expr, +Str:list) is det.
%
%   Parse an Ariel expression.

parse_expr(E, Str) :-
    parse_expr_(E, Str), !;
    throw(error(parsing_error)).

parse_expr_(E, Str) :-
    phrase(tokens(Tokens), Str),
    phrase(expr(E), Tokens).

%!  parse_type(-T, +Str:list) is det.
%
%   Parse an Ariel type.

parse_type(T, Str) :-
    parse_type_(T, Str), !;
    throw(error(parsing_error)).

parse_type_(T, Str) :-
    phrase(tokens(Tokens), Str),
    phrase(type(T), Tokens).

expr_or_decl(term_decl(Name, E)) --> term_decl(term_decl(Name, E)), !.
expr_or_decl(expr(E)) --> expr(E), !.

expr(E) --> app(E), !.
expr(E as T) --> as_expr(E, T), !.
expr(E) --> nlr_expr(E), !.
expr(E) --> between_parens(expr(E)), !.

as_expr(E, T) --> nlr_expr(E), [colon], type(T), !.
as_expr(E, T) --> between_parens(as_expr(E, T)), !.

nlr_expr(int(E)) --> [int_lit(E)], !.
nlr_expr(string(E)) --> [string_lit(E)], !.
nlr_expr(E) --> multi_lambda(E), !.
nlr_expr(E) --> lambda(E), !.
nlr_expr(E) --> let(E), !.
nlr_expr(var(E)) --> [id(E)], !.
nlr_expr(qvar(Module, Name)) --> [qid(Module, Name)], !.
nlr_expr(E) --> between_parens(nlr_expr(E)), !.

type([t_int] -> Res) --> [t_int, arrow], !, type(Res), !.
type([t_string] -> Res) --> [t_string, arrow], !, type(Res), !.
type([tvar(V)] -> Res) --> [id(V), arrow], !, type(Res), !.
type(Args -> Res) --> type_args(Args), [arrow], !, type(Res), !.
type(t_int) --> [t_int], !.
type(t_string) --> [t_string], !.
type(forall(TVars, T)) --> [forall], type_var_args(TVars), [dot], !, type(T), !.
type(tvar(V)) --> [id(V)], !.

term_decl(term_decl(Name, (Args => E))) --> fun_decl(fun_decl(Name, Args, E)), !.
term_decl(term_decl(Name, E)) --> var_decl(var_decl(Name, E)), !.

fun_decl(fun_decl(Name, Args, Body)) --> [id(Name)], var_args(Args), [equal], !, expr(Body), !.

var_decl(var_decl(Name, E)) --> [id(Name), equal], !, expr(E), !.

lambda('=>'([Var], E)) --> [lambda_intro, id(Var)], !, [arrow], expr(E), !.

multi_lambda('=>'(Vars, E)) --> [lambda_intro], var_args(Vars), !, [arrow], expr(E), !.

app((E as T) @ Args) --> as_expr(E, T), args(Args), !.
app(E @ Args) --> nlr_expr(E), args(Args), !.
app(E @ Args) --> between_parens(app(E)), args(Args), !.

let(let(Var = S) in E) --> let_var(let(Var = S) in E), !.
let(let(Var = S) in E) --> let_fun(let(Var = S) in E), !.

let_var(let(Var = S) in E) --> [let_intro], var_decl(var_decl(Var, S)), !, [partial_expr_sep], expr(E), !.

let_fun(let(Var = (Args => S)) in E) --> [let_intro], fun_decl(fun_decl(Var, Args, S)), !, [partial_expr_sep], expr(E), !.

between_parens(Parser) --> [open_parens], !, call(Parser), [close_parens], !.

id(I) --> [id(I)], !.

args(Args) --> sequence([open_parens], expr, [arg_sep], [close_parens], Args), !.

var_args(Args) --> sequence([open_parens], id, [arg_sep], [close_parens], Args), !.

type_args(Args) --> sequence([open_parens], type, [arg_sep], [close_parens], Args), !.

type_var_args(TVars) --> sequence([open_parens], id, [arg_sep], [close_parens], Args), !, {maplist(wrap_in_tvar, Args, TVars)}.

wrap_in_tvar(V, tvar(V)) :- !.
