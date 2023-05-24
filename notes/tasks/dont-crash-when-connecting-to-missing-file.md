# Don't crash when connecting to missing file

```
> :connect "csv" ["file.txt"]
laurel: file.txt: openBinaryFile: does not exist (No such file or directory)
```

Report an error and keep the REPL open.