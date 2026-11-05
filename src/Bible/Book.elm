module Bible.Book exposing
    ( Book
    , all, fromInt, toInt
    , name, title
    , numChapters, numVerses
    , aliases
    )

{-| The 66 books of the Bible, in order, with their names, sizes and the
spellings the parser accepts.

    Bible.fromString "Ps 23"
        |> Result.map (Bible.toStartBook >> title)
        == Ok "Psalms"

@docs Book


# Listing books

@docs all, fromInt, toInt


# Names

@docs name, title


# Size

@docs numChapters, numVerses


# Parsing

@docs aliases

-}

import Internal.Book
import Internal.Parser


{-| A book of the Bible. Get one from [`all`](#all), [`fromInt`](#fromInt) or
[`Bible.toStartBook`](Bible#toStartBook).
-}
type alias Book =
    Internal.Book.Book


{-| Every book, from Genesis to Revelation.
-}
all : List Book
all =
    Internal.Book.books


{-| The book with this number, counting Genesis as 1 and Revelation as 66.

    Maybe.map name (fromInt 19) == Just "Psalm"

    fromInt 67 == Nothing

-}
fromInt : Int -> Maybe Book
fromInt =
    Internal.Book.fromInt >> Result.toMaybe


{-| The book's number, from 1 (Genesis) to 66 (Revelation). This is the book
part of [`Bible.encode`](Bible#encode).
-}
toInt : Book -> Int
toInt =
    Internal.Book.toInt


{-| The name used in a reference, as [`Bible.format`](Bible#format) writes it:
"Genesis", "1 Samuel", "Psalm".
-}
name : Book -> String
name =
    Internal.Book.toString


{-| The name used for the book as a whole, such as in a heading. This is the
same as [`name`](#name) except for Psalms: `name` gives "Psalm" (as in
"Psalm 23") and `title` gives "Psalms".
-}
title : Book -> String
title book =
    if book == Internal.Book.Psalms then
        "Psalms"

    else
        name book


{-| How many chapters the book has.

    Maybe.map numChapters (fromInt 19) == Just 150

-}
numChapters : Book -> Int
numChapters =
    Internal.Book.numChapters


{-| How many verses a chapter of the book has, or 0 if the book has no such
chapter.

    Maybe.map (\psalms -> numVerses psalms 119) (fromInt 19) == Just 176

-}
numVerses : Book -> Int -> Int
numVerses =
    Internal.Book.numVerses


{-| The lowercase spellings [`Bible.fromString`](Bible#fromString) accepts for
the book. Matching ignores case, a trailing `.` and extra whitespace.

    Maybe.map aliases (fromInt 1) == Just [ "genesis", "gen" ]

-}
aliases : Book -> List String
aliases book =
    Internal.Parser.aliases
        |> List.filter (\( _, b ) -> b == book)
        |> List.map Tuple.first
