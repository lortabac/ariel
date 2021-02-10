:- module(api, [
    init_backend/1,
    stop_backend/1,
    run_query/3,
    run_one_query/2,
    typeof/2,
    pretty_type/2,
    pretty_typeof/2,
    runnable_type/1
]).

:- use_module(library(assoc)).
:- use_module(src(backend/codegen)).
:- use_module(src(backend/interpreter)).
:- use_module(src(syntax/parser)).

init_backend(Backend) :-
    start_scheme_repl(Backend).

stop_backend(Backend) :-
    stop_scheme_repl(Backend).

run_query(Backend, ArielStr, R) :-
    string_codes(ArielStr, ArielCodes),
    parse_expr(E, ArielCodes),
    phrase(codegen(E), BackendCodes),
    string_codes(BackendStr, BackendCodes),
    query_scheme_repl(Backend, BackendStr, R).

run_one_query(ArielExpr, R) :-
    init_backend(Backend),
    run_query(Backend, ArielExpr, R),
    stop_backend(Backend).

typeof(ArielCodes, T) :-
    parse_expr(E, ArielCodes),
    typecheck(E, T).

pretty_type(T, S) :-
    phrase(pp_type(T), TCodes),
    string_codes(S, TCodes).

pretty_typeof(ArielCodes, S) :-
    typeof(ArielCodes, T),
    pretty_type(T, S).

runnable_type(forall(_, T)) :-
    runnable_type(T),
    !.
runnable_type(T) :-
    T \= (_ -> _),
    T \= forall(_, _),
    !.
