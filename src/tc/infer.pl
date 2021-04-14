:- module('tc/infer', [typecheck_ng/2, typecheck/3]).

:- use_module(library(assoc)).
:- use_module(src(syntax/operators)).
:- use_module(src(tc/generalize)).
:- use_module(src(tc/instantiate)).
:- use_module(src(tc/unify)).

typecheck_ng(E, T) :-
    empty_assoc(Globals),
    typecheck(Globals, E, T).

typecheck(Globals, E, T) :-
    typecheck_(Globals, E, T), !;
    throw(error(type_error)).

typecheck_(Globals, E, T) :-
    empty_assoc(Ctx),
    infer_gen(Globals, Ctx, E, T),
    !.

infer(_, _, int(_), t_int) :- !.
infer(_, _, string(_), t_string) :- !.
infer(_, Ctx, var(Name), Rh) :-
    get_assoc(Name, Ctx, T),
    instantiate(T, Rh),
    !.
infer(Globals, _, qvar(Module, Name), Rh) :-
    get_assoc(Module-Name, Globals, T),
    instantiate(T, Rh),
    !.
infer(Globals, Ctx, Names => E, Rh) :-
    is_list(Names),
    maplist([_]>>free_metavar, Names, MVs),
    zip(Names, MVs, Pairs),
    put_many_assoc(Pairs, Ctx, NewCtx),
    infer(Globals, NewCtx, E, BodyT),
    unify(Rh, MVs -> BodyT),
    !.
infer(Globals, Ctx, E as S, Rh) :-
    infer(Globals, Ctx, E, T),
    instantiate(S, Rh),
    check_subsumption(T, Rh),
    !.
infer(Globals, Ctx, E @ AppArgs, ResT) :-
    infer(Globals, Ctx, E, T),
    unify_arr(T, AppArgs, LamArgs -> ResT),
    zip(LamArgs, AppArgs, Pairs),
    maplist(unify_app_arg(Globals, Ctx), Pairs),
    !.
infer(Globals, Ctx, let(Name = Sub) in E, Rh) :-
    infer_gen(Globals, Ctx, Sub, S),
    put_assoc(Name, Ctx, S, NewCtx),
    infer(Globals, NewCtx, E, Rh).

infer_gen(Globals, Ctx, E, S) :-
    infer(Globals, Ctx, E, Rh),
    generalize(Ctx, Rh, S).

unify_arr(T, Args, ArgMVs -> ResMV) :-
    maplist([_]>>free_metavar, Args, ArgMVs),
    free_metavar(ResMV),
    T = (ArgMVs -> ResMV).

unify_app_arg(Globals, Ctx, LamArg-AppArg) :-
    infer(Globals, Ctx, AppArg, InferedArg),
    unify(LamArg, InferedArg).

free_metavar(MV) :-
    copy_term(_, MV).

zip([], [], []).
zip([X|Xs], [Y|Ys], [X-Y|Zs]) :- zip(Xs, Ys, Zs).

attach_metavar(X, X-MV) :-
    free_metavar(MV).

put_many_assoc(Elems, Assoc, NewAssoc) :-
    foldl(put_tuple_assoc, Elems, Assoc, NewAssoc).

put_tuple_assoc(Key-Value, Assoc, NewAssoc) :-
    put_assoc(Key, Assoc, Value, NewAssoc).
