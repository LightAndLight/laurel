# String parsing

In the [CSV example](git:commit:3ebc19b5eee137a8c4f0725f0a6ae6277e0452e1/tree/laurel-core/test/data/example.csv) I have this data:

```
> tables.example
╭───────────────────┬───────────────────────────┬─────────────────┬───────────────────╮
│ merchant : String │ transaction type : String │ amount : String │ category : String │
├───────────────────┼───────────────────────────┼─────────────────┼───────────────────┤
│ Google            │ debit                     │ 10.00           │ tech              │
│ NAB               │ credit                    │ 5.00            │ finance           │
│ Spotify           │ debit                     │ 12.00           │ entertainment     │
│ Some Cafe         │ debit                     │ 22.00           │ eating out        │
│ Hotpot Restaurant │ debit                     │ 50.00           │ eating out        │
│ Woolworths        │ debit                     │ 38.00           │ groceries         │
│ NAB               │ credit                    │ 5.00            │ finance           │
╰───────────────────┴───────────────────────────┴─────────────────┴───────────────────╯
```

How do I sum the `amount`s for all debits? I need to convert them to numbers first.

I'd like to write something like this:

```
for row in tables.example 
where row.`transaction type` == "debit"
parseDollars row.amount
|> fold addDollars zeroDollars
```

Where:

* `type Dollars = { dollars : Int, cents : Int }`
* `zeroDollars = { dollars = 0, cents = 0 }`
* `addDollars : Dollars -> Dollars -> Dollars`
* `parseDollars : String -> Relation Dollars`
* `fold : (b -> a -> b) -> b -> Relation a -> b`
