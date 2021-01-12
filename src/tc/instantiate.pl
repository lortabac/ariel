:- module('tc/instantiate', [instantiate/2]).

:- use_module(library(apply)).
:- use_module(library(assoc)).

instantiate(T, R) :-
    nonvar(T),
    T = forall(Vars, E),
    metavar_assoc(Vars, MvAssoc),
    instantiate_(MvAssoc, E, R),
    !.

instantiate(T, T) :-
    T \= forall(_, _),
    !.

instantiate(T, T) :-
    var(T).

instantiate_(MvAssoc, T, MV) :-
    nonvar(T),
    T = tvar(TV),
    get_assoc(TV, MvAssoc, MV),
    !.

instantiate_(MvAssoc, T, RA -> RB) :-
    nonvar(T),
    T = (A -> B),
    maplist(instantiate_(MvAssoc), A, RA),
    instantiate_(MvAssoc, B, RB),
    !.

instantiate_(_, T, T) :-
    T \= tvar(_),
    T \= (_ -> _),
    !.

instantiate_(_, T, T) :-
    var(T),
    !.

metavar_assoc(TVs, R) :-
    maplist(zip_to_fresh_var, TVs, Pairs),
    list_to_assoc(Pairs, R).

zip_to_fresh_var(tvar(TV), TV-MV) :-
    copy_term(_, MV).
