# elm-bible

[![Build Status](https://semaphoreci.com/api/v1/monty5811/elm-bible/branches/master/badge.svg)](https://semaphoreci.com/monty5811/elm-bible)

Parse and format Bible references in Elm.

Inspired by [python-scriptures](https://github.com/davisd/python-scriptures).

## Examples

```elm

-- parsing

(fromString "Gen 1:1") == [ Ok <| Reference Genesis 1 1 1 1]

(fromString "Gen 1:1, Exodus 1, Luke 10:16, Acts 28:100")
    ==
    [ Ok <| Reference Genesis 1 1 1 1
    , Ok <| Reference Exodus 1 1 1 22
    , Ok <| Reference Luke 10 16 10 16
    , Err "Invalid end verse"
    ]

-- formatting

(format (Reference Genesis 1 1 1 1)) == "Genesis 1:1"

(format (Reference Genesis 1 1 11 5)) == "Genesis 1:1-11:5"
```
