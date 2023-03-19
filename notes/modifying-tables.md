# Modifying tables

As of <git:commit:45ab9abd540cd35dff344d49fdcda3d0b3eb5a9b> I have a typed query language and a way
to define tables. Now I need to move data in and out of the tables.

## Sketch 1

```
get : (r -> a) -> Command r a

getNames : Command { people : Relation people.Out, r } (Relation String) =
  get \tables -> for person in tables.people yield person.name

modify : (r -> r) -> Command r ()

setName (id : people.Id) (name : String) : Command { people : Relation people.Out, r } () =
  modify \tables -> {
    tables |
    people =
      let target =
        for person in tables.people
        where person.id == id
        yield person;
      
      tables.people minus
      target union 
      (for person in target yield { person | name = name })
  }
```

## "Assignables?"

One way I like to think of tables is as ["assignables"](https://existentialtype.wordpress.com/2012/02/01/words-matter/) over relations. Bob Harper explains the
topic comprehensively in [Practical Foundations for Programming
Languages](http://www.cs.cmu.edu/~rwh/pfpl.html).

## Sketch 2

```
tableRowType(table) = row    table : Relation row |- body : a
-------------------------------------------------------------
           get table body : Command { table } a
```

```
                        tableRowType(table_1) = row_1
                        tableRowType(table_2) = row_2
                                    ...
                        tableRowType(table_n) = row_n
table_1 : Relation row_1, table_2 : Relation row_2, ..., table_n : Relation row_n |- body : a
---------------------------------------------------------------------------------------------
get { table_1, table_2, ..., table_n } body : Command { table_1, table_2, ..., table_n } a
```

```
getNames : Command { people } (Relation String) =
  get people for person in people yield person.name
```

## Sketch 3

```
      tables |- table defined
-----------------------------------
get table : Query (Relation table.Out)
```

```
      tables |- table defined
-----------------------------------------
set table : Relation table.In -> Query ()
```

```
                       tables |- table defined
--------------------------------------------------------------------
modify table : (Relation table.Out -> Relation table.In) -> Query ()
```

```
getNames : Query (Relation String) {
  people <- get tables.people;
  return for person in people yield person.name
}

setName (id : people.Id) (name : String) : Query () {
  people <- get tables.people;
  set tables.people (
    let target =
      for person in people
      where person.id == id
      yield person;
    
    people minus
    target union 
    (for person in target yield { person | name = name })
  )
}

setName (id : people.Id) (name : String) : Query () {
  modify tables.people \people ->
  let target =
    for person in people
    where person.id == id
    yield person;
  
  people minus
  target union 
  (for person in target yield { person | name = name })
}
```

## Do I need an imperative language?

If a database is something that an application interacts with, do I actually need to design an
imperative language on the database side? The application itself could sequence the commands.

Having called that into question, it seems that for now un-sequencable get/set/modify might do.

## Sketch 4

```
            tables |- table defined
------------------------------------------------
get table : (Relation table.Out -> a) -> Query a
```

```
         tables |- table defined
-----------------------------------------
set table : Relation table.In -> Query ()
```

```
                    tables |- table defined
--------------------------------------------------------------------
modify table : (Relation table.Out -> Relation table.In) -> Query ()
```

```
getNames : Query (Relation String) =
  get tables.people \people ->
  for person in people yield person.name

setName (id : people.Id) (name : String) : Query () =
  modify tables.people \people ->
  let target =
    for person in people
    where person.id == id
    yield person;
  
  people minus
  target union 
  (for person in target yield { person | name = name })
```

## Sketch 5

I might not even have to internalise get/set/modify. They could be commands external to the
language.

`get` could provide all tables as their underlying relations:

```
repl> :get for person in tables.people yield person.name
```

`set` could similarly provide the initial state of all tables, and then replace the contents of the
target table with the computed relation. This way it's sort of an in-between of `set` and `modify`:

```
repl> :set people (
  let target =
    for person in tables.people
    where person.id == id
    yield person;
  
  tables.people minus
  target union 
  (for person in target yield { person | name = name })
)
```

We could extend this to multi-update:

```
repl> :set { people, pets } (
  let targetPerson =
    for person in tables.people where person.id == id yield person;
  
  let targetPet =
    for pet in tables.pets
    for person in targetPerson
    where pet.owner == person.id;
  
  {
    people = tables.people minus targetPerson,
    pets = 
      tables.pets minus targetPet union
      (for pet in targetPet yield { pet | owner = None })
  }
)
```

Does this still allow us to factor out and compose queries?

```
deletePerson (tables : { people : Relation people.Out, pets : Relation pets.Out, r }) (id : people.Id) : { people : Relation people.In, pets : Relation pets.In } =
  let targetPerson =
    for person in tables.people where person.id == id yield person;
  
  let targetPet =
    for pet in tables.pets
    for person in targetPerson
    where pet.owner == person.id;
  
  {
    people = tables.people minus targetPerson,
    pets = 
      tables.pets minus targetPet union
      (for pet in targetPet yield { pet | owner = None })
  }
```

```
repl> :set { people, pets } (deletePerson tables 22)
```

```
deletePerson (people : Relation people.Out) (pets : Relation pets.Out) (id : people.Id): { people : Relation people.In, pets : Relation pets.In } =
  let targetPerson =
    for person in people where person.id == id yield person;
  
  let targetPet =
    for pet in pets
    for person in targetPerson
    where pet.owner == person.id;
  
  {
    people = tables.people minus targetPerson,
    pets = 
      tables.pets minus targetPet union
      (for pet in targetPet yield { pet | owner = None })
  }
```

```
repl> :set { people, pets } (deletePerson tables.people tables.pets 22)
```

Perhaps each defined query has implicit access to `tables`, which represents the state of the
database at the time the query was executed:

```
deletePerson (id : people.Id) : { people : Relation people.In, pets : Relation pets.In } =
  let targetPerson =
    for person in tables.people where person.id == id yield person;
  
  let targetPet =
    for pet in tables.pets
    for person in targetPerson
    where pet.owner == person.id;
  
  {
    people = tables.people minus targetPerson,
    pets = 
      tables.pets minus targetPet union
      (for pet in targetPet yield { pet | owner = None })
  }
```

```
repl> :set { people, pets } (deletePerson 22)
```