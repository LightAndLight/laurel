# Improvements

This is a list of problems that are pretty easy to fix, but not worth doing at this stage of playing
around.

* Variable shadowing

  Postgres doesn't like table name shadowing, so during compilation rename variables to avoid it.

* Query normalisation

  Example:

  ```
  from
    (from 
      people
      (\person1 -> when (person1.age == 25) (yield person1))
    )
    (\person2 -> yield person2.name)

  = { right-associate from }

  from
    people
    (\person1 ->
      from
        (when (person1.age == 25) (yield person1))
        (\person2 -> yield person2.name)
    )

  = { from into when }

  from
    people
    (\person1 ->
      when
        (person1.age == 25)
        (from
          (yield person1)
          (\person2 -> yield person2.name)
        )
    )

  = { from-yield }

  from
    people
    (\person1 ->
      when
        (person1.age == 25)
        ((\person2 -> yield person2.name) person1)
    )

  = { beta reduction }

  from
    people
    (\person1 ->
      when
        (person1.age == 25)
        (yield person1.name)
    )
  ```

  * `when true a = a`
  * `x == x = true`