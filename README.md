# elm-bible

[![CI](https://github.com/monty5811/elm-bible/actions/workflows/ci.yml/badge.svg)](https://github.com/monty5811/elm-bible/actions/workflows/ci.yml)

Parse and format Bible references in Elm.

## Features

- Parse a reference from a string
- Nicely format a reference to a string
- Convert a reference to an encoded representation for sorting/comparing/storage
- Look up books (`Bible.Book`): names, numbers, chapter and verse counts and the spellings the parser accepts

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

Book names can be written in full or abbreviated (`Psalms 23`, `Ps. 23`,
`1 Cor 13`, `First Corinthians 13`, `Jhn 3:16`), in any case.

## Examples

```elm

(fromString "Gen 1:1" |> Result.map format)
     == Ok "Genesis 1:1" 

(fromString "Gen 1:1 - Rev 5") |> Result.map format)
    == Ok "Genesis 1:1 - Revelation 5:14" 

(fromString "Gen 1:1 - Rev 5") |> Result.map encode) 
    == Ok {start = 1001001, end = 66005014}

```

## Python and Rust

[elm-bible-rs](rust/) is a Rust port with Python bindings (`pip install
elm-bible-rs`) that gives the same answers and error messages as this
package, so a Python backend can agree with an Elm frontend. elm-bible-rs
X.Y.Z matches elm-bible X.Y.Z. It lives in `rust/`, and CI checks the two
agree.

## Contributing

Contributions welcome, please [open an issue](https://github.com/monty5811/elm-bible/issues/new) to get started.

```sh
yarn install          # installs elm and elm-format (pinned in elm-tooling.json)
yarn test             # elm-test
yarn format:check     # elm-format --validate
yarn docs             # elm make --docs=docs.json
yarn diff             # elm diff against the latest published version
```

A change to `src/` must keep `rust/` passing, in the same PR. See
[rust/COMPAT.md](rust/COMPAT.md#changing-behaviour).
