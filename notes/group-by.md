# "Group by"

What's the type of "group by"?

```
groupBy_field :
  Relation { field : a, r } ->
  Relation { field : a, group : Relation { r } }

groupBy_field :
  Relation { field : a, r } ->
  Relation { field : a, group : Relation { r } }
groupBy_field rows = 
  for field in distinct (for row in rows yield row.field)
  yield { 
    field,
    group =
      for { field = field', ..rest } in rows
      where field == field'
      yield rest
  }

countBy_field :
  Relation { field : a, r } ->
  Relation { field : a, count : Int }
countBy_field rows = 
  for field in distinct (for row in rows yield row.field)
  yield { 
    field,
    count =
      count <|
        for { field = field', ..rest } in rows
        where field == field'
        yield rest
  }
```

Translating `countBy_field` to SQL:

```
SELECT
  rows1.field,
  (
    SELECT
      COUNT(*)
    FROM rows AS rows2
    WHERE rows1.field = rows2.field
  ) AS count
FROM
  (SELECT DISTINCT rows.field FROM rows) AS rows1
```

And then `groupBy_field`:

```
SELECT
  rows1.field,
  ARRAY(
    SELECT rows2
    FROM rows AS rows2
    WHERE rows1.field = rows2.field
  ) AS group
FROM
  (SELECT DISTINCT rows.field FROM rows) AS rows1;
```

With an arbitrary comparison:

```
groupBy :
  (a -> a -> Bool) ->
  Relation a ->
  Relation (Relation a)
```

There's an intersting generalisation for `groupBy_field`:

```
groupBy_field :
  { zero : g, one : { r } -> g, combine : g -> g -> g } ->
  Relation { field : a, r } ->
  Relation { field : a, group : g }

# count
groupBy_field { zero = 0, one = \_ -> 1, combine = (+) } :
  Relation { field : a, r } -> Relation { field : a, group : Int }

# distinct
groupBy_field { zero = (), one = \_ -> (), combine = \() () -> () } :
  Relation { field : a, r } -> Relation { field : a, group : () }

# group
groupBy_field { zero = values [], one = \a -> values [a], combine = union } :
  Relation { field : a, r } -> Relation { field : a, group : Relation { r } }
```

Then trying to generalise over `field` comparisons:

```
groupBy :
  (a -> { label : b, item : c }) ->
  { zero : g, one : c -> g, combine : g -> g -> g } ->
  Relation a ->
  Relation { label : b, group : g }

groupBy_field =
  groupBy (\{ field, ..rest } -> { label = field, item = rest })
```

Then abstracting the comparison function;

```
groupBy :
  (b -> b -> Bool) ->
  (a -> { label : b, item : c }) ->
  { zero : g, one : c -> g, combine : g -> g -> g } ->
  Relation a ->
  Relation { label : Relation (b, b), group : g }
```

The "label" is now a relation itself, containing all the `b`s that were involved in the creation of
the group.

There's something a little weird about the `Relation (b, b)`, though. What do you do for the first
`b` you encounter?

```
groupBy' :
  (a -> { label : b, item : c }) ->
  { zero : g, one : c -> g, combine : g -> g -> g } ->
  Relation a ->
  Relation { label : Relation b, group : g }
groupBy' split monoid relation =
  for { label, group } in groupBy (==) split monoid relation
  for (left, right) in distinct label
  where (left == right)
  yield { label = left, group }
```