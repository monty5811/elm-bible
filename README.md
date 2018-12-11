# elm-bible

[![Build Status](https://semaphoreci.com/api/v1/monty5811/elm-bible/branches/master/badge.svg)](https://semaphoreci.com/monty5811/elm-bible)

Parse and format Bible references in Elm.

## Features

- Parse a reference from a string
- Nicely format a reference to a string
- Convert a reference to an encoded representation for sorting/comparing/storage

The following reference formats can be parsed:

 - Genesis 1
 - Genesis 1:1
 - Genesis 1:1-20
 - Genesis 1:20-2:24
 - Genesis 1-5
 - Genesis 1 - Exodus 5
 - Genesis 1:1 - Exodus 5:20
 - Genesis 1:1 - Exodus 5
 - Genesis 1 - Exodus 5:20

## Examples

```elm

(fromString "Gen 1:1" |> Result.map format)
     == Ok "Genesis 1:1" 

(fromString "Gen 1:1 - Rev 5") |> Result.map format)
    == Ok "Genesis 1:1 - Revelation 5:14" 

(fromString "Gen 1:1 - Rev 5") |> Result.map encode) 
    == Ok {start = 1001001, end = 66005014}

```

## Contributing

Contributions welcome, please [open an issue](https://github.com/monty5811/elm-bible/issues/new) to get started.
