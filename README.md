A document generator for zig.

```
cat examples/top_level.zig | ./docgen
```

For now, the idea is to perform lexing and some simplistic parsing of the
top-level structure, returning useful definitions in an intermediate format
(.e.g JSON).

Eventually it may make sense to provide lexing/parsing support of the language
as part of the stdlib.

# checklist

 - [ ] Complete tokenization support
 - [ ] Partial top-level parsing support
 - [ ] Intermediate output generation
 - [ ] Hook into an existing backend/generate output
