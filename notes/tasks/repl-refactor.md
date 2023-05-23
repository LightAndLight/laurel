# REPL refactor

* Create a `Command` datatype for [REPL commands](git:commit:18e068a7be12640c54c4cb6356aaa2ec9a713452/tree/laurel-cli/app/Repl.hs#L102).
* Parse commands using parser combinators (string splitting will have edge cases in string literals)
* Are there good places to split
  [`repl.hs`](git:commit:18e068a7be12640c54c4cb6356aaa2ec9a713452/tree/laurel-cli/app/Repl.hs) in to modules?