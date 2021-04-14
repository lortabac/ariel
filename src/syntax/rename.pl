:- module('syntax/rename', [qualify_globals/3]).

:- use_module(library(apply)).
:- use_module(library(assoc)).
:- use_module(library(ordsets)).

qualify_globals(Globals, E, R) :-
    ord_empty(Locals),
    qualify_globals_(Globals, Locals, E, R).

qualify_globals_(Globals, Ctx, E as T, E1 as T) :-
    qualify_globals_(Globals, Ctx, E, E1),
    !.
qualify_globals_(_, _, int(X), int(X)) :- !.
qualify_globals_(_, _, string(X), string(X)) :- !.
qualify_globals_(_, _, qvar(Module, Name), qvar(Module, Name)) :- !.
qualify_globals_(Globals, Locals, var(Name), R) :-
    ord_memberchk(Name, Locals) ->
    R = var(Name);
    qualify_var(Globals, Name, Module),
    R = qvar(Module, Name).
qualify_globals_(Globals, Locals, Args => E, Args => R) :-
    ord_union(Locals, Args, Locals1),
    qualify_globals_(Globals, Locals1, E, R),
    !.
qualify_globals_(Globals, Locals, E @ Args, E1 @ Args1) :-
    qualify_globals_(Globals, Locals, E, E1),
    maplist(qualify_globals_(Globals, Locals), Args, Args1).

qualify_var(Globals, Name, Module) :-
    findall(M, gen_assoc(M-Name, Globals, _), [Module]).
