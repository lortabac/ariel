:- dynamic user:file_search_path/2.
:- multifile user:file_search_path/2.

:- prolog_load_context(directory, Dir),
    atomic_list_concat([Dir, src], '/', Src),
    asserta(user:file_search_path(src, Src)).

:- use_module(src(syntax/lexer)).
:- use_module(src(syntax/operators)).
:- use_module(src(syntax/parser)).
:- use_module(src(syntax/pretty)).
:- use_module(src(tc/constraints)).
:- use_module(src(tc/generalize)).
:- use_module(src(tc/infer)).
:- use_module(src(tc/instantiate)).
:- use_module(src(backend/codegen)).
:- use_module(src(backend/interpreter)).
:- use_module(src(api)).
:- use_module(src(repl)).
