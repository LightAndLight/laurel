# Pretty errors in REPL

Display readable error messages in the REPL. Currently they look like this:

```
> for row in tables.example yield row.merchant;
ParseError (Unexpected {position = 44, expected = fromList [Eof,Char '.',String "group by",String "identifier"]})
```