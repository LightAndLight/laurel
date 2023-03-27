# Sum types and foreign key references

With sum times and a convenient check constraint language, we can write "disjunctive foreign key
constraints"; saying that a field references one table or another or another.

```
table square {
  type Id = Int,
  id : Id [PrimaryKey],
  side : Int
}

table circle {
  type Id = Int,
  id : Id [PrimaryKey],
  diameter : Int
}

table triangle {
  type Id = Int,
  id : Id [PrimaryKey],
  base : Int,
  height : Int
}

enum ShapeId {
    Square(squares.Id),
    Circle(circles.Id),
    Triangle(triangles.Id)
}

table shapes {
  type Id = Int,
  id : Id [PrimaryKey],
  value : ShapeId,
  Check(\row ->
    case row.value of
      ShapeId.Square(id) ->
        id in (for square in squares yield square.id)
      ShapeId.Circle(id) ->
        id in (for circle in circles yield circle.id)
      ShapeId.Triangle(id) ->
        id in (for triangle in trianbles yield triangle.id)
  )
}
```

## Alternative sketch 1: a `references` type

Statically encode foreign key references using a special type.

### Semantics 1: checked at construction time

```
a : Type    lookupType(table, field) = a
---------------------------------------- (references-type)
    a references table.field : Type

fkey(table.field) : a -> a references table.field

unfkey(table.field) : a references table.field -> a
```

```
                     value ~> value'
notEmpty (for row in table where table.field == value') ~> true
---------------------------------------------------------------
     fkey(table.field) value ~> fkey(table.field, value)

value ~> fkey(table.field, value')
-----------------------------------
unfkey(table.field) value ~> value'
```

```
enum ShapeId {
    Square(squares.Id references squares.id),
    Circle(circles.Id references circles.id),
    Triangle(triangles.Id references triangles.id)
}

table shapes {
  type Id = Int,
  id : Id [PrimaryKey],
  value : ShapeId,
}
```

Disadvantage: less batch-checkable?

### Semantics 2: checked at insertion time

To regain batch checking, defer the foriegn key checks until the row is about to be inserted.

Disadvantage: `fkey` and `unfkey` is boilerplate / inconvenient?

## Alternative sketch 2: pattern-matching `references` constraint

If a field has an enum type, then it can be given a foreign key constraint in pattern-matching
style, specifying a foreign key constraint for each variant that needs one.

```
enum ShapeId {
    Square(squares.Id),
    Circle(circles.Id),
    Triangle(triangles.Id)
}

table shapes {
  type Id = Int,
  id : Id [PrimaryKey],
  value : ShapeId [
    References(
      Square(id) -> References(id, squares.id),
      Circle(id) -> References(id, circles.id),
      Triangle(id) -> References(id, triangles.id)
    )
  ]
}
```

```
table shapes {
  type Id = Int,
  id : Id [PrimaryKey],
  value : ShapeId,
  References(
    value,
    Square(id) -> References(id, squares.id),
    Circle(id) -> References(id, circles.id),
    Triangle(id) -> References(id, triangles.id)
  )
}
```

Or this could generalise to a `Match` constraint, which applies constraints conditional on the value
of the field:


```
table shapes {
  type Id = Int,
  id : Id [PrimaryKey],
  value : ShapeId,
  Match(
    value,
    Square(id) -> References(id, squares.id),
    Circle(id) -> References(id, circles.id),
    Triangle(id) -> References(id, triangles.id)
  )
}
```

Some constraints don't make sense when they're conditionally applied, such as `Key` and
`PrimaryKey`. Whether or not some fields form a key is independent of any values in a table.

You could restrict the branches `Match` to only allow references to bound variables, which would
disallow conditional keys on other fields:

```
table shapes {
  type Id = Int,
  id : Id,
  value : ShapeId,
  Match(
    value,
    Square(ignored) -> PrimaryKey(id), # error: `id` not in scope
  )
}
```

But there are still some key constraints that wouldn't be rejected:

```
table shapes {
  value : ShapeId,
  Match(
    value,
    Square(id) -> PrimaryKey(id),
  )
}
```

```
table shapes {
  value : ShapeId,
  Match(
    value,
    Square(id) -> PrimaryKey(id),
    Circle(id) -> PrimaryKey(id),
    Triangle(id) -> PrimaryKey(id),
  )
}
```

```
table shapes {
  value : ShapeId,
  Match(
    value,
    Square(id) -> PrimaryKey(id.value),
    Circle(id) -> PrimaryKey(id.value),
    Triangle(id) -> PrimaryKey(id.value),
  )
}
```

The first example *should* be rejected because it leaves the primary key partially undefined.

The second example gives a fully defined primary key, but should also be rejected because each `id`
is of a different type, so they aren't comparable. A key must correspond to a single type.

The third example would be acceptable. It says that the value of the `id` field in the `ShapeId`
enum is the primary key. It would mean that rows like `{ value = ShapeId.Square(squares.Id(1)) }`
and `{ value = ShapeId.Triangle(triangles.Id(1)) }` can't both exist in the table at the same time.
I don't know if this would be useful.

It all gets pretty complicated, so I'd prefer the simple "matching `References`" if I had to
implement it now.

## Alternative sketch 3: constraints in type definitions

What if you could add constraints to the fields of type definitions, such that those constraints get
added to tables that contain said types?

```
enum ShapeId {
    Square(squares.Id [References(squares.id)]),
    Circle(circles.Id [References(circles.id)]),
    Triangle(triangles.Id [References(triangles.id)])
}

table shapes {
  type Id = Int,
  id : Id [PrimaryKey],
  value : ShapeId
}
```

Does it get weird for key constraints?

```
enum ShapeId {
    Square(squares.Id [Key]),
    Circle(circles.Id [Key]),
    Triangle(triangles.Id [Key])
}
```

A key is a value that should uniquely identify a row. We can't define a key on a constuctor's
argument because it's not able to identify rows that don't contain said constructor. Primary keys
would be ruled out for the same reason. Defaults and foreign key references would work fine.

## Alternative sketch 4: syntax for naming enum variants

If there was an operator to "name" an enum's constructor, like `.` "names" a record's field, then we
could locate a specific field as the target of a foreign key constraint. I wouldn't use `.` for this
though, because I only use `.` for projection from products. 

```
enum ShapeId {
    Square(squares.Id),
    Circle(circles.Id),
    Triangle(triangles.Id)
}

table shapes {
  type Id = Int,
  id : Id [PrimaryKey],
  value : ShapeId,
  References(value@Square.0, squares.id),
  References(value@Circle.0, circles.id),
  References(value@Triangle.0, triangles.id),
}
```

## How to prevent dangling references?

Before I remove a row `r` from `squares`, I first have to check that

```
for shape in shapes
where case shape.value of
  Square(id) -> id == r.id
  Circle(id) -> false
  Triangle(id) -> false
```

is empty. That is, there's no row in `shapes` that references the row in `squares`. Do we have to
scan every row in `shapes` to find out?

What if there was an internally maintained table of back-references for each foreign key reference?

```
table `shapes that have ShapeId.Square(square_id)` {
  squareId : squares.Id,
  shapeId : shapes.Id
}
```

But a square can be referenced by multiple shapes, so we can't efficiently find its parent.

Here's a related [StackOverflow answer](https://dba.stackexchange.com/a/138270):

> For those trying to understand why: consider a foreign key from table A to table B. If you delete
> a row from table B the database has to verify that no rows in table A reference this row. If table
> A does not have an index on the referencing column, it has to sequentially scan the whole table,
> which could be very slow if the table is large.

In my situation, this would suggest that `shapes.value` needs an index, which doesn't make sense
because it's not a unique value.

Going back to the back-reference table, we could do it like this:

```
table `shapes that contain a squares.Id` {
  squareId : squares.Id [PrimaryKey],
  shapeId : Relation shapes.Id
}
```

or this:

```
table `number of shapes that contain a squares.Id` {
  squareId : squares.Id [PrimaryKey],
  referenceCount : Int
}
```

And then what about composite foriegn key references?