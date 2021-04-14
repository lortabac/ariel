:- module('tc/unify', [unify/2, check_subsumption/2]).

:- use_module(library(chr)).

:- chr_constraint unify/2.

unify(X ,Y) <=> unify_or_throw(X, Y).

unify_or_throw(X, Y) :-
    unify_with_occurs_check(X, Y), !;
    throw(error(type_mismatch(X, Y))).

check_subsumption(X, Y) :-
    subsumes_term(X, Y), !;
    throw(error(type_mismatch(X, Y))).
