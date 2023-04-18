# Sketching REPL usage

In <git:commit:a6e7ca886550c53cb5ad67c7e62063271547d3e8> I started working on a REPL. It supports
CSV data sources, and I've planned to add PostgreSQL support.

I load up the REPL:

```
Welcome to the dblang REPL. Type :quit to exit.
> 
```

And enter an expression:

```
> 1 + 1
error: no connection found
```

I get an error because the system wants me to connect to a data source before it will evaluate
expressions. I don't like this; I think the REPL should evaluate without any connection.

> ℹ Improvement: evaluate without having "connected" to a data source

```
> :connect csv dblang/test/data/example.csv
connected
```

I can use `:connect` to "load" a data source. This CSV file has been added to `tables` (I haven't
implemented `:type` yet, but here's what it would do)

```
> :type tables
{
  example : Relation {
    merchant : String,
    `transaction type` : String,
    amount : String,
    category : String
  }
}
```

I chose the word "connect" because I imagined using the same command to connect to remote databases.
Something like `:connect postgres { host = "localhost", port = 5432, database = "test" }`. I'm
wondering whether this action should also infer types for the database's tables.

> ℹ Improvement (tentative): infer table types for Postgres connection ([continued here](./extracting-types-from-information-schema.md))

How do I modify tables? The `tables` variable comes from <modifying-tables.md#implicit-tables>, and
it works fine with the "multiple data source" approach I've stumbled across. But the [`set`
command](modifying-tables.md#set-command) might not work as nicely. With multiple data sources I'm
tempted to aim for a sort of "cross data source atomicity", which is too complicated for my taste
right now. So my latest description of the `set` command is good enough. `connect`
brings a set of tables into scope, and it's these tables that can be updated using `set`.

I should be able to list all the in-scope tables using `:tables`:

```
> :tables

table t1 {
  f1 : Int [PrimaryKey],
  f2 : String,
  f3 : Int
}

table t2 {
  f4 : Int [PrimaryKey],
  f5 : Bool
}

table t3 {
  f6 : ()
}
```

Each of these tables is accessible using the `tables` variable.