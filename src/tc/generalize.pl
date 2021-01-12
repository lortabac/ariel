:- module('tc/generalize', [generalize/3]).

:- use_module(library(clpfd)).
:- use_module(library(ordsets)).

generalize(Ctx, Rh, S) :-
    term_variables_ord(Rh, TVars),
    term_variables_ord(Ctx, CtxVars),
    ord_subtract(TVars, CtxVars, GenVars),
    (GenVars = [] ->
    S = Rh;
    quantify(Rh, GenVars, S)).

quantify(T, Vars, forall(Vars, T)) :-
    unify_with_tvars(Vars).

unify_with_tvars(Vs) :-
    unify_with_tvars_(97, Vs).

unify_with_tvars_(_, []) :- !.
unify_with_tvars_(N, [V|Vs]) :-
    char_code(C, N),
    V = tvar(C),
    N1 #= N + 1,
    unify_with_tvars_(N1, Vs).

term_variables_ord(T, OrdVars) :-
    term_variables(T, Vars),
    list_to_ord_set(Vars, OrdVars).
