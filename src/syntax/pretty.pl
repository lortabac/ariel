:- module('syntax/pretty', [pp_type//1]).

pp_type(t_int) --> "Int", !.
pp_type(t_string) --> "String", !.
pp_type([X,Y|Args] -> Res) -->
    list([X,Y|Args]),
    space,
    "->",
    space,
    pp_type(Res),
    !.
pp_type([Param] -> Res) -->
    pp_type(Param),
    space,
    "->",
    space,
    pp_type(Res),
    !.
pp_type(forall([], T)) --> pp_type(T), !.
pp_type(forall(Vars, T)) -->
    "forall",
    list(Vars),
    ".",
    space,
    pp_type(T),
    !.
pp_type(tvar(V)) --> {atom_codes(V, Codes)}, Codes, !.
pp_type(V) --> {var(V)}, "<metavar>", !.

parensed(X) --> open_parens, X, close_parens.

open_parens --> "(".

close_parens --> ")".

space --> " ".

list(Xs) --> parensed(list_(Xs)).

list_([]) --> [], !.
list_([X]) --> pp_type(X), !.
list_([X0,X1|Xs]) --> pp_type(X0), ",", space, list_([X1|Xs]).
