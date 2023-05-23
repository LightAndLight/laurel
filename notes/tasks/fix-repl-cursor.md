# Fix REPL cursor

If I keep pressing the left arrow key, the cursor can hover over the prompt text:

```
> 1 + 1
^
|
Cursor can live here
```

It should stop after the prompt:

```
> 1 + 1
  ^
  |
This is the farthest the cursor can go
```

I can also backspace the cursor, but I shouldn't be able to.