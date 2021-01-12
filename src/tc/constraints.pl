:- module('tc/constraints', [unify/2]).

:- use_module(library(chr)).

:- chr_constraint unify/2.

unify(X ,Y) <=> unify_with_occurs_check(X, Y).
