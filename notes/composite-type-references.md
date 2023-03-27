# Foreign key references in composite types

What if a solution for <sum-type-references.md> worked for any kind of composite type?

"Type" style:

```
table thing {
  blah : {
    a : other.Id references other.id,
    b : Bool,
    c : String
  }
}
```

"Pattern match" style:

```
table thing {
  blah : {
    a : other.Id,
    b : Bool,
    c : String
  } [References({ a, b, c } -> References(a, other.id))]
}
```

"Constraints in types" style:

```
table thing {
  blah : {
    a : other.Id [References(other.id)],
    b : Bool,
    c : String
  }
}
```