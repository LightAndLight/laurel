# Compiling monadic bind to PostgreSQL

[`from : Relation a -> (a -> Relation b) -> Relation b`](git:commit:aa8e68760cb55977ba651b74587950ec91096a1c/tree/src/Dblang/Expr.hs#L21) takes an arbitrary expression as its
second argument. Translation is easiest when that expression is a lambda: `[[ from a (\x -> b) ]]`
becomes `SELECT [[ b ]]_expr FROM [[ a ]] AS x [[ b ]]_froms`. What when the expression isn't a
lambda, like in `from a f` or `from a (f x)`?

That second argument is a function `a -> Relation b`, so I think it should be compiled to a
PostgreSQL function, call it `pf`. If `f` is already a name, then we can assume it refers to an
existing function. We then use `LATERAL` to pass each row of `a` as an argument to
`pf`: `[[ from a name ]] = SELECT y.* FROM [[ a ]] as x CROSS JOIN LATERAL name(x) as y`.

I notice that in the translation we implictly eta expanded to give `a`'s items a name:
`[[ from a name ]] = [[ from a (\x -> name x) ]]`, and we add in a (technically redundant)
`from`/`yield` pair for the results of `name x`:
`[[ from a (\x -> name x) ]] = [[ from a (\x -> from (name x) (\y -> yield y))]]`.

I also notice that `LATERAL` is required when the first argument of `from` depends on a previous
relation's rows.

I think for the core language, `from` can have its own binding structure instead of taking a
second expression argument (changed in <git:commit:d363eeceb7e6010a2944c8ed1f4bf017ea8d600f/tree/src/Dblang/Expr.hs#L22>). This forces me to explicitly eta expand where I had been doing it implicitly
before. The frontend can eta-expand calls to `from` when needed, inserting unique variable names.