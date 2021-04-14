:- module('backend/interpreter', [
    start_scheme_repl/1,
    stop_scheme_repl/1,
    query_scheme_repl/3,
    exec_scheme_repl/2
]).

:- use_module(library(process)).
:- use_module(library(readutil)).

start_scheme_repl(In-Out-PID) :-
    process_create(path('chezscheme9.5'), ['--quiet'], [stdin(pipe(In)), stdout(pipe(Out)), process(PID)]).

stop_scheme_repl(In-_-PID) :-
    write(In, "(exit)"),
    nl(In),
    flush_output(In),
    process_release(PID).

query_scheme_repl(In-Out-_, Cmd, R) :-
    write(In, Cmd),
    nl(In),
    flush_output(In),
    read_line_to_string(Out, R).

exec_scheme_repl(In-_-_, Cmd) :-
    write(In, Cmd),
    nl(In),
    flush_output(In).
