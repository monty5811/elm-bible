module Bible exposing
    ( fromString
    , format
    , encode, decode
    , toStartBookString, toEndBookString, toStartChapter, toEndChapter, toStartVerse, toEndVerse
    , Reference
    )

{-| A library for working with Bible references.


# Parsing

@docs fromString


# Formatting

@docs format


# Encoding

It may be useful to have a unique, compact representation of a reference for storage, searching, sorting, etc.

An easy way to achieve this is to convert the start and end of the reference to an `Int`.
These integers can then be stored in a database, sorted, checked for intersections to do searches, etc.

The encoding process is as follows:

    (1000000 * Book.toInt book) + (1000 * chapter) + verse

This results in an `Int` with the following structure

    16001001
    --===___
     | |  |
     | |  |
     | |  |--- Zero padded verse number
     | |------ Zero padded chapter number
     |-------- Book number

@docs encode, decode


# Lower Level Parts

These functions may be useful if you want to build your own formatter, or something else.

@docs toStartBookString, toEndBookString, toStartChapter, toEndChapter, toStartVerse, toEndVerse


# Types

@docs Reference

-}

import Internal.Bible as Internal
import Internal.Book as Book
import Internal.Reference


{-| An opaque type to represent a Bible reference.
-}
type Reference
    = Reference Internal.Reference.Reference



-- Parse


{-| Attempt to convert a `String` into a `Reference`.

The following formats should work:

  - Genesis 1
  - Genesis 1:1
  - Genesis 1:1-20
  - Genesis 1:20-2:24
  - Genesis 1-5
  - Genesis 1 - Exodus 5
  - Genesis 1:1 - Exodus 5:20
  - Genesis 1:1 - Exodus 5
  - Genesis 1 - Exodus 5:20

-}
fromString : String -> Result String Reference
fromString str =
    Result.map Reference <| Internal.fromString str



-- Format


{-| Get a formatted `String` from a `Reference`.

Some example outputs:

  - Genesis 1:1
  - Genesis 1:1-11
  - Genesis 1:1 - 12:2
  - Genesis 1:1 - Exodus 2:4

-}
format : Reference -> String
format (Reference ref) =
    Internal.format ref



-- Encode


{-| Convert a reference to an encoded representation.

    (fromString "Gen 1:1 - Rev 5") |> Result.map encode)
        == Ok {start = 1001001, end = 66005014}

-}
encode : Reference -> { start : Int, end : Int }
encode (Reference ref) =
    Internal.encode ref


{-| Attempt to convert an encoded value to a reference.
-}
decode : { start : Int, end : Int } -> Result String Reference
decode ref =
    Result.map Reference <| Internal.decode ref



-- Parts


{-| Get the name of the book the reference starts with.
-}
toStartBookString : Reference -> String
toStartBookString (Reference { startBook }) =
    Book.toString startBook


{-| Get the chapter the reference starts with.
-}
toStartChapter : Reference -> Int
toStartChapter (Reference { startChapter }) =
    startChapter


{-| Get the verse the reference starts with.
-}
toStartVerse : Reference -> Int
toStartVerse (Reference { startVerse }) =
    startVerse


{-| Get the name of the book the reference ends with.
-}
toEndBookString : Reference -> String
toEndBookString (Reference { endBook }) =
    Book.toString endBook


{-| Get the chapter the reference ends with.
-}
toEndChapter : Reference -> Int
toEndChapter (Reference { endChapter }) =
    endChapter


{-| Get the verse the reference ends with.
-}
toEndVerse : Reference -> Int
toEndVerse (Reference { endVerse }) =
    endVerse
