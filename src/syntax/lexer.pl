:- module('syntax/lexer', [tokens//1]).

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

tokens([X|Xs]) --> token(X), blanks, tokens(Xs), !.
tokens([]) --> [], !.

token(let_intro) --> "let", blank, !.
token(forall) --> "forall", blank, !.
token(forall), "(" --> "forall", "(", !.
token(t_int) --> "Int", !.
token(t_string) --> "String", !.
token(id(R)) --> lc_identifier(R), !.
token(qid(Module, Name)) --> uc_identifier(Module), ".", !, lc_identifier(Name), !.
token(arrow) --> "->", !.
token(equal) --> "=", !.
token(lambda_intro) --> "\\", !.
token(open_parens) --> "(", !.
token(close_parens) --> ")", !.
token(arg_sep) --> ",", !.
token(partial_expr_sep) --> ";", !.
token(dot) --> ".", !.
token(colon) --> ":", !.
token(int_lit(X)) --> integer(X), !.
token(string_lit(Str)) --> "\"", string_without("\"", Codes), "\"", {string_codes(Str, Codes)}, !.

lc_identifier(Name) -->
    [C0], { code_type(C0, lower) }, !,
    identifier_continue(CL),
    { atom_codes(Name, [C0|CL]) }.

uc_identifier(Name) -->
    [C0], { code_type(C0, upper) }, !,
    identifier_continue(CL),
    { atom_codes(Name, [C0|CL]) }.

identifier_continue([H|T]) -->
    [H], { code_type(H, prolog_identifier_continue) }, !,
    identifier_continue(T).
    identifier_continue([]) --> "".
