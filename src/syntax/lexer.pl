:- module('syntax/lexer', [tokens//1, token//1]).

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

tokens([X|Xs]) --> token(X), blanks, tokens(Xs), !.
tokens([]) --> [], !.

token(let_intro) --> "let", blank, !.
token(id(R)) --> lc_identifier(R), !.
token(arrow) --> "->", !.
token(equal) --> "=", !.
token(lambda_intro) --> "\\", !.
token(open_parens) --> "(", !.
token(close_parens) --> ")", !.
token(arg_sep) --> ",", !.
token(partial_expr_sep) --> ";", !.
token(dot) --> ".", !.
token(int_lit(X)) --> integer(X), !.
token(string_lit(Str)) --> "\"", string_without("\"", Codes), "\"", {string_codes(Str, Codes)}, !.

identifier(Atom) --> lc_identifier(Codes), !, {atom_codes(Atom, Codes)}.

lc_identifier(Name) -->
    [C0], { code_type(C0, prolog_atom_start) }, !,
    lc_identifier_continue(CL),
    { atom_codes(Name, [C0|CL]) }.

lc_identifier_continue([H|T]) -->
    [H], { code_type(H, prolog_identifier_continue) }, !,
    lc_identifier_continue(T).
    lc_identifier_continue([]) --> "".
