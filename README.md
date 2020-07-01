_We are such stuff as dreams are made on, and our little life is rounded with a sleep._ - William Shakespeare

# Ariel

A simple purely-functional programming language, mainly for educational purposes.

## Features

These features are under development right now:

- Strict (call-by-value) evaluation.

- Purity through "monadic" IO.

- Full type inference.

- Polymorphism over both record fields and constructors.

- Implicit parameters.

- Pretty-printable core language (with the same syntax as the surface language) for easy inspection.

- A rich REPL environment with runtime tracing and debugging facilities.

## Wishlist

These features will be added sometime in the future:

- Built-in structural polymorphism (like Haskell's `Generic`).

- Native compilation via Scheme.

- JavaScript backend.

## Implementation

The evaluator uses nameless environments, similarly to the LETREC interpreter
described in Friedman, Wand. 2008. _Essentials of Programming Languages_, MIT Press.

The type-checker is based on [Peyton Jones, Vytiniotis, Weirich, Shields. 2007. _Practical type inference for arbitrary-rank types_](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/putting.pdf),
adapted to a query-based approach, as described in https://ollef.github.io/blog/posts/query-based-compilers.html.

## Status

The project is still in a very early phase.
