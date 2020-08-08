# ParserRecovery

This is an investigation on how existing monadic parsers could be extended to improve error recovery

## What is error recovery within parsers?

Error recovery isn't (usually) a reference to a parser fixing its input, but instead that it allows for multiple errors to be fixed. Let's say that we were parsing the following JSON code:

```json
{
    "name"err: 3,
    "name2" 4
}
```

there are two errors, the addition of the `err` and the lack of a `:` for the `"name2"` attribute. Most parsers would only show you the first error, because after it hits that unknown input it doesn't know how to recover from it and continue parsing. More intelligent parsers have the ability to recover from it and start on the next attribute, and find that error too.

Generally, the most common versions of that would require either hand-coding the parser, or defining it in an LL or LR parser generator to describe the whole language.

## Goals of the project

1) Allow for parsers to recover (at a minimum using the panic-mode error recovery method) from errors that occur.
2) Allow for multiple errors to be returned from a parser
3) Allow for error codes to be descriptive (string describing errors, location etc.)