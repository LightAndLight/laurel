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

## Alternative sketch: a `references` type

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

Advantage: statically encode inter-table references.

### How to prevent dangling references?

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