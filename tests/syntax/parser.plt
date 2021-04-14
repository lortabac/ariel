:- use_module(src(syntax/operators)).
:- use_module(src(syntax/parser)).

:- begin_tests(parser).

test(int) :-
    parse_expr(int(1), `1`).

test(string) :-
    parse_expr(string("hello"), `"hello"`).

test(var) :-
    parse_expr(var(x), `x`).

test(qvar) :-
    parse_expr(qvar('Foo', bar), `Foo.bar`).

test(lambda) :-
    parse_expr([x] => var(x), `\\x -> x`).

test(lambda_no_spaces) :-
    parse_expr([x] => var(x), `\\x->x`).

test(lambda_spaces_after) :-
    parse_expr([x] => var(x), `\\x -> x `).

test(multi_lambda) :-
    parse_expr([x, y] => var(x), `\\(x, y) -> x`).

test(multi_lambda_one_arg) :-
    parse_expr([x] => var(x), `\\(x) -> x`).

test(multi_lambda_no_arg) :-
    parse_expr([] => var(x), `\\() -> x`).

test(let) :-
    parse_expr(let(x = int(1)) in var(x), `let x = 1; x`).

test(let_no_spaces) :-
    parse_expr(let(x = int(1)) in var(x), `let x=1;x`).

test(app) :-
    parse_expr(var(id)@[int(1)], `id(1)`).

test(app_space) :-
    parse_expr(var(id)@[int(1)], `id (1)`).

test(app2) :-
    parse_expr(var(id)@[int(1), int(2)], `id(1, 2)`).

test(lambda_app) :-
    parse_expr(([x] => var(x)) @ [int(1)], `(\\x -> x)(1)`).

test(lambda_app_app) :-
    parse_expr((([x] => var(x)) @ [int(1)]) @ [int(2)], `((\\x -> x)(1))(2)`).

test(qapp) :-
    parse_expr(qvar('Repl', id)@[int(1)], `Repl.id(1)`).

test(as) :-
    parse_expr(int(1) as t_int, `1 : Int`).

test(as_no_space) :-
    parse_expr(int(1) as t_int, `1:Int`).

test(id_int) :-
    parse_expr(([x] => var(x)) as ([t_int] -> t_int), `(\\x -> x) : Int -> Int`).

test(id_int_app) :-
    parse_expr((([x] => var(x)) as ([t_int] -> t_int)) @ [int(1)], `((\\x -> x) : Int -> Int)(1)`).

test(id_int_app_space) :-
    parse_expr((([x] => var(x)) as ([t_int] -> t_int)) @ [int(1)], `((\\x -> x) : Int -> Int) (1)`).
