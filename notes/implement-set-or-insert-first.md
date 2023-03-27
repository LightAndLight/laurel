# Implement `set` or `insert` first?

Changes to tables should have a simple translation to relational algebra. But do I need to support
the most generation version first?  

## Add person to `people` table

Using `set`:

```
:set { people = tables.people union values [ { name = "Test", age = 27 } ] }
```

Using `insert`:

```
:insert people { name = "Test", age = 27 }
```

Does it batch?

```
:insert {
  people = { name = "Harry Dresden", age = 37 },
  pets = { name = "Mouse", age = 2 }
}
```

How can I use generated values of one inserted row as an argument to another?

```
:insert {
  people = { name = "Harry Dresden", age = 37 },
  pets = { ownerId = inserted.people.id, name = "Mouse", age = 2 }
}
```

And assert that there are no cycles between rows?

## Increment a person's age

Using `set`:

```
:set
  let targets = for person in people yield person where person.id == 1;
  { 
    people = 
      (tables.people minus targets) union
      (for person in targets yield { person | age = person.age + 1 })
  }
```

Using `update`/`merge`/`union` (not sure what to call it):

```
:update
  people
  (
    for person in tables.people
    where person.id == 1
    yield { person | age = person.age + 1 }
  )
```

```
:merge
  people
  (
    for person in tables.people
    where person.id == 1
    yield { person | age = person.age + 1 }
  )
```

```
:union
  people
  (
    for person in tables.people
    where person.id == 1
    yield { person | age = person.age + 1 }
  )
```

Is there a single row version? I don't like the duplication of `people` and `tables.people`; that
the target table is `people` should be implied from the context.

```
:update people \person -> where person.id == 1 yield { person | age = person.age + 1 }
```

```
:update person in people where person.id == 1 yield { person | age = person.age + 1 }
```

What about batching?

```
:update {
  people =
    \person ->
    where person.id == 1
    yield { person | age = person.age + 1 },
  pets = \pet -> yield { pet | isGood = true }
}
```

## Conclusion

I'll start with the operations that make single-row changes easiest, and work toward more general operations.