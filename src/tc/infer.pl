:- module('tc/infer', [typecheck/2, infer/3, infer_gen/3]).

:- use_module(library(assoc)).
:- use_module(src(syntax/operators)).
:- use_module(src(tc/generalize)).
:- use_module(src(tc/instantiate)).

typecheck(E, T) :-
    empty_assoc(Ctx),
    infer_gen(Ctx, E, T),
    !.

infer(_, int(_), t_int) :- !.
infer(_, string(_), t_string) :- !.
infer(Ctx, var(Name), Rh) :-
    get_assoc(Name, Ctx, T),
    instantiate(T, Rh),
    !.
infer(Ctx, Names => E, Rh) :-
    is_list(Names),
    maplist([_]>>free_metavar, Names, MVs),
    zip(Names, MVs, Pairs),
    put_many_assoc(Pairs, Ctx, NewCtx),
    infer(NewCtx, E, BodyT),
    unify(Rh, MVs -> BodyT),
    !.
infer(Ctx, E @ AppArgs, ResT) :-
    infer(Ctx, E, T),
    unify_arr(T, AppArgs, LamArgs -> ResT),
    zip(LamArgs, AppArgs, Pairs),
    maplist(unify_app_arg(Ctx), Pairs),
    !.
infer(Ctx, let(Name = Sub) in E, Rh) :-
    infer_gen(Ctx, Sub, S),
    put_assoc(Name, Ctx, S, NewCtx),
    infer(NewCtx, E, Rh).

infer_gen(Ctx, E, S) :-
    infer(Ctx, E, Rh),
    generalize(Ctx, Rh, S).

unify_arr(T, Args, ArgMVs -> ResMV) :-
    maplist([_]>>free_metavar, Args, ArgMVs),
    free_metavar(ResMV),
    T = (ArgMVs -> ResMV).

unify_app_arg(Ctx, LamArg-AppArg) :-
    infer_gen(Ctx, AppArg, InferedArg),
    subsumes_term(LamArg, InferedArg),
    instantiate(InferedArg, InferedArgRh),
    unify(InferedArgRh, LamArg).

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
