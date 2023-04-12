# Formatting values

I want to pretty-print
[`Value`s](git:commit:3d024c2df08129ad3d37343efca679bcff9892cd/tree/src/Dblang/Value.hs) by
rendering tables in the terminal.

## Prior art

Postgres uses this syntax ([source](https://stackoverflow.com/a/16108898/2884502)):

```
 id | time  |       humanize_time             | value 
----+-------+---------------------------------+-------
  1 | 09:30 |  Early Morning - (9.30 am)      |   570
  2 | 11:30 |  Late Morning - (11.30 am)      |   690
  3 | 13:30 |  Early Afternoon - (1.30pm)     |   810
  4 | 15:30 |  Late Afternoon - (3.30 pm)     |   930
(4 rows)
```

[Nushell](https://www.nushell.sh) uses this syntax ([source](https://www.nushell.sh/book/working_with_tables.html#working-with-tables)):

```
───┬───────────────┬──────┬─────────┬────────────
 # │ name          │ type │ size    │ modified
───┼───────────────┼──────┼─────────┼────────────
 0 │ files.rs      │ File │  4.6 KB │ 5 days ago
 1 │ lib.rs        │ File │   330 B │ 5 days ago
 2 │ lite_parse.rs │ File │  6.3 KB │ 5 days ago
 3 │ parse.rs      │ File │ 49.8 KB │ 1 day ago
 4 │ path.rs       │ File │  2.1 KB │ 5 days ago
 5 │ shapes.rs     │ File │  4.7 KB │ 5 days ago
 6 │ signature.rs  │ File │  1.2 KB │ 5 days ago
───┴───────────────┴──────┴─────────┴────────────
```

and this:

```
╭────┬───────────────────┬──────────┬──────────┬──────────────╮
│ #  │     filename      │ filetype │ filesize │     date     │
├────┼───────────────────┼──────────┼──────────┼──────────────┤
│  0 │ Applications      │ dir      │    256 B │ 3 days ago   │
│  1 │ Data              │ dir      │    256 B │ 2 weeks ago  │
│  2 │ Desktop           │ dir      │    448 B │ 2 hours ago  │
│  3 │ Disks             │ dir      │    192 B │ a week ago   │
│  4 │ Documents         │ dir      │    416 B │ 4 days ago   │
```

## Plan

A `Relation a` should be displayed as a table when `a` is a record:

```
> tables.people
╭────────────────┬───────────────┬───────────╮
│ id : people.Id │ name : String | age : Int │
├────────────────┼───────────────┼───────────┤
│ 1              │ Joe Bloe      │ 22        │
│ 2              │ John Doe      │ 5         │
│ 3              │ Jane Doe      │ 17        │
│ 4              │ Li Wang       │ 62        │
│ 5              │ Alan Turing   │ 79        │
│ 6              │ Alonzo Church │ 23        │
│ 7              │ Plato         │ 34        │
╰────────────────┴───────────────┴───────────╯
```

When `a` isn't a record, I think it should be displayed as a table containing a single field called
`_`:

```
> for person in tables.people yield person.age
╭─────────╮
│ _ : Int │
├─────────┤
│ 22      │
│ 5       │
│ 17      │
│ 62      │
│ 79      │
│ 23      │
│ 34      │
╰─────────╯
```

A `Map k v` can appear as a table with columns `key : k` and `value : v`:

```
> tables.books group by _.genre |> map count
╭─────────────────┬─────────────╮
│ key : String    │ value : Int │
├─────────────────┼─────────────┤
│ fantasy         │ 22          │
│ science fiction │ 5           │
│ crime           │ 17          │
│ biography       │ 62          │
│ drama           │ 79          │
╰─────────────────┴─────────────╯

> for { key, value } in (tables.books group by _.genre) yield { genre = key, count = count value }
╭─────────────────┬─────────────╮
│ genre : String  │ count : Int │
├─────────────────┼─────────────┤
│ fantasy         │ 22          │
│ science fiction │ 5           │
│ crime           │ 17          │
│ biography       │ 62          │
│ drama           │ 79          │
╰─────────────────┴─────────────╯
```

I allow nested relations. Should I just render tables within tables?

```
> tables.people group by _.household
╭─────────────────────┬───────────────────────────────────────────────────────────────╮
│ key : households.Id │ value : Relation { id : people.Id, name : String, age : Int } |
├─────────────────────┼───────────────────────────────────────────────────────────────┤
│                     │ ╭────────────────┬───────────────┬───────────╮                │
│ 1                   │ │ id : people.Id │ name : String | age : Int │                │
│                     │ ├────────────────┼───────────────┼───────────┤                │
│                     │ │ 1              │ Larry         │ 22        │                │
│                     │ │ 2              │ Curly         │ 23        │                │
│                     │ │ 3              │ Moe           │ 24        │                │
│                     │ ╰────────────────┴───────────────┴───────────╯                │
│                     │                                                               │
│                     │ ╭────────────────┬───────────────┬───────────╮                │
│ 2                   │ │ id : people.Id │ name : String | age : Int │                │
│                     │ ├────────────────┼───────────────┼───────────┤                │
│                     │ │ 4              │ Socrates      │ 53        │                │
│                     │ │ 5              │ Plato         │ 54        │                │
│                     │ │ 6              │ Aristotle     │ 55        │                │
│                     │ ╰────────────────┴───────────────┴───────────╯                │
│                     │                                                               │
│                     │ ╭────────────────┬─────────────────────┬───────────╮          │
│ 3                   │ │ id : people.Id │ name : String       | age : Int │          │
│                     │ ├────────────────┼─────────────────────┼───────────┤          │
│                     │ │ 7              │ Friederich Neitsche │ 99        │          │
│                     │ ╰────────────────┴─────────────────────┴───────────╯          │
╰─────────────────────┴───────────────────────────────────────────────────────────────╯
```

That seems neat enough.