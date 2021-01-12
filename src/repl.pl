:- module(repl, [start_ariel_repl/0]).

:- use_module(src(api)).

start_ariel_repl :-
    init_backend(Backend),
    loop(Backend).

loop(Backend) :-
    prompt1('> '),
    read_line_to_codes(user_input, ArielCodes),
    (ArielCodes == end_of_file -> stop(Backend); continue(Backend, ArielCodes)).

continue(Backend, ArielCodes) :-
    (typeof(ArielCodes, T) ->
        run(Backend, ArielCodes, T);
        writeln("Invalid expression")),
    loop(Backend).

run(Backend, ArielCodes, T) :-
    pretty_type(T, TStr),
    (runnable_type(T) ->
        run_query(Backend, ArielCodes, Res),
        write(Res),
        write(" : "),
        write(TStr),
        nl;
        write("<non-runnable>"),
        write(" : "),
        write(TStr),
        nl).

stop(Backend) :-
    writeln("exit"),
    stop_backend(Backend).
