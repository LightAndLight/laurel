# Querying CSVs

I was analysing my spending for the month by looking over some CSVs that I exported from my bank
account. I realised that the results I was trying to compute could probably be phrased as relational
queries.

The CSVs in question would have the following schema:

```
table transactions {
  date : String,
  amount : String,
  accountNumber : String,
  transactionType : String,
  transactionDetails : String,
  category : String,
  merchantName : String
}
```

or if I wanted to give it more precise types:

```
table transactions {
  date : { year : Int, month : Int, day : Int },
  amount : { dollars : Int, cents : Int },
  accountNumber : String,
  transactionType : (| Credit, Debit |),
  transactionDetails : String,
  category : String,
  merchantName : String
}
```

Here are some queries I wanted to do:

* Total spent

  ```
  totalSpent : { dollars : Int, cents : Int } =
    for transaction in tables.transactions
    where transaction.transactionType == Debit
    yield transaction.amount
    |> reduce addMoney { dollars = 0, cents = 0 }
  ```

* Show all categories

  ```
  categories : Relation String =
    for transaction in tables.transactions
    yield transaction.category
    |> distinct
  ```

* Show counts for categories (should really use [groupBy](./group-by.md))

  ```
  categoryCounts : Relation { category : String, count : Int } =
    for category in categories
    yield {
      category,
      count = 
        for transaction in tables.transactions
        where transaction.category == category
        yield transaction
        |> count
    }
  ```

* Show counts for categories, as well as the amount spent for that category

  ```
  categoryCountsAndAmount :
    Relation {
      category : String,
      count : Int,
      amount : { dollars : Int, cents : Int }
    }
    =
    let categoryTransactions =
      for transaction in tables.transactions
      where transaction.category == category
      yield transaction;
    
    for category in categories
    yield {
      category,
      count =
        count categoryTransactions,
      amount = 
        map _.amount categoryTransactions
        |> reduce addMoney { dollars = 0, cents = 0 }

    }
  ```