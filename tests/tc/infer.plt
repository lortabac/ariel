:- use_module(src(syntax/operators)).
:- use_module(src(tc/infer)).

:- begin_tests(infer).

test(int) :-
    typecheck(int(1), t_int).

test(string) :-
    typecheck(string("hello"), t_string).

test(id) :-
    typecheck([x] => var(x), forall([tvar(a)], [tvar(a)] -> tvar(a))).

test(const) :-
    typecheck([x, y] => var(x), forall([tvar(a), tvar(b)], [tvar(a), tvar(b)] -> tvar(a))).

test(const_int) :-
    typecheck([x] => int(1), forall([tvar(a)], [tvar(a)] -> t_int)).

test(app_id) :-
    typecheck(([x] => var(x)) @ [int(1)], t_int).

test(test_app_const) :-
    typecheck(([x, y] => var(x)) @ [int(1), string("hello")], t_int).

test(let_int) :-
    typecheck(let(x = int(1)) in var(x), t_int).

test(let_id) :-
    typecheck(let(id = [x] => var(x)) in var(id), forall([tvar(a)], [tvar(a)] -> tvar(a))).

test(let_id_int) :-
    typecheck(let(id = [x] => var(x)) in var(id) @ [int(1)], t_int).

test(let_id_id_int) :-
    typecheck(let(id = [x] => var(x)) in var(id) @ [var(id) @ [int(1)]], t_int).
