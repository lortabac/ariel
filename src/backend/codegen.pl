:- module('backend/codegen', [generate_term_decl_code/4, generate_code/2]).

:- use_module(src(syntax/operators)).

generate_term_decl_code(Module, Name, E as _, CodeStr) :-
    generate_term_decl_code(Module, Name, E, CodeStr),
    !.

generate_term_decl_code(Module, Name, E, CodeStr) :-
    (E \= (_ as _)),
    phrase(codegen_term_decl(Module, Name, E), Codes),
    string_codes(CodeStr, Codes),
    !.

generate_code(E as _, CodeStr) :-
    generate_code(E, CodeStr),
    !.
generate_code(E, CodeStr) :-
    (E \= (_ as _)),
    phrase(codegen(E), Codes),
    string_codes(CodeStr, Codes),
    !.

codegen_term_decl(Module, Name, E) -->
    {atom_codes(Module, ModuleCodes)},
    {atom_codes(Name, NameCodes)},
    open_parens,
    "define",
    space,
    ModuleCodes,
    ".",
    NameCodes,
    space,
    codegen(E),
    close_parens,
    !.

codegen(int(N)) -->
    {number_codes(N, Codes)},
    Codes,
    !.
codegen(string(S)) -->
    {with_output_to(codes(Codes), writeq(S))},
    Codes,
    !.
codegen(var(A)) -->
    codegen(A),
    !.
codegen(qvar(Module, Name)) -->
    {atomic_list_concat([Module, ".", Name], Qname)},
    codegen(Qname),
    !.
codegen(A) -->
    {atom(A)},
    {atom_codes(A, Codes)},
    Codes,
    !.
codegen(Args => E) -->
    {is_list(Args)},
    open_parens,
    "lambda",
    space,
    list(Args),
    space,
    codegen(E),
    close_parens,
    !.
codegen(Arg => E) -->
    {atom(Arg)},
    codegen([Arg] => E),
    !.
codegen(E @ Args) -->
    list([E|Args]),
    !.
codegen(let(Name = Sub) in E) -->
    open_parens,
    "let",
    space,
    parensed(list([Name, Sub])),
    space,
    codegen(E),
    close_parens,
    !.

list(Xs) --> parensed(list_(Xs)).

list_([]) --> [], !.
list_([X]) --> codegen(X), !.
list_([X0,X1|Xs]) --> codegen(X0), space, list_([X1|Xs]).

parensed(X) --> open_parens, X, close_parens.

open_parens --> "(".

close_parens --> ")".

space --> " ".
