:- module(api, [
    init_backend/2,
    stop_backend/1,
    run_query/5,
    run_one_query/2,
    arrow_type/1
]).

:- use_module(library(assoc)).
:- use_module(src(backend/codegen)).
:- use_module(src(backend/interpreter)).
:- use_module(src(syntax/parser)).
:- use_module(src(syntax/rename)).
:- use_module(src(tc/infer)).

init_backend(Backend, Globals) :-
    empty_assoc(Globals),
    start_scheme_repl(Backend).

stop_backend(Backend) :-
    stop_scheme_repl(Backend).

run_query(Backend, Globals, ArielStr, R, Globals1) :-
    catch(run_query_(Backend, Globals, ArielStr, R, Globals1), Err, show_error(Err, R)).

run_query_(Globals, Backend, ArielStr, R, Globals1) :-
    string_codes(ArielStr, ArielCodes),
    parse_expr_or_decl(E, ArielCodes),
    run_expr_or_decl(Backend, Globals, E, R, Globals1),
    !.

run_expr_or_decl(Backend, Globals, expr(E_), R, Globals) :-
    qualify_globals(Globals, E_, E),
    typecheck(Globals, E, T),
    pretty_type(T, PrettyT),
    (arrow_type(T) ->
        SchemeRes = "<function>";
        generate_code(E, BackendStr),
        query_scheme_repl(Backend, BackendStr, SchemeRes)),
    atomics_to_string([SchemeRes, " : ", PrettyT], R).

run_expr_or_decl(Backend, Globals, term_decl(Name, E), R, Globals1) :-
    typecheck(Globals, E, T),
    pretty_type(T, PrettyT),
    Module = 'Repl',
    put_assoc(Module-Name, Globals, T, Globals1),
    generate_term_decl_code(Module, Name, E, BackendStr),
    exec_scheme_repl(Backend, BackendStr),
    atomics_to_string([Name, " : ", PrettyT], R).

run_one_query(ArielExpr, R) :-
    init_backend(Backend, Globals),
    run_query(Backend, Globals, ArielExpr, R, _),
    stop_backend(Backend).

pretty_type(T, S) :-
    phrase(pp_type(T), TCodes),
    string_codes(S, TCodes).

arrow_type(forall(_, T)) :-
    arrow_type(T),
    !.
arrow_type(_ -> _) :- !.

show_error(error(parsing_error), "Parsing error").
show_error(error(type_error), "Type error").
show_error(error(type_mismatch(X, Y)), R) :-
    pretty_type(X, PX),
    pretty_type(Y, PY),
    format(string(R), "Type mismatch between ~w and ~w", [PX, PY]).
