module TestEdgeCases exposing (suite)

{-| Pins down how the package handles malformed and unusual input.

The fuzz tests only generate valid references. These cover everything else.
If you change one of these behaviours on purpose, update its test in the same
change, so the behaviour change is visible in review.

-}

import Bible
import Expect
import Internal.Bible
import Internal.Book as Book
import Internal.Parser
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Edge cases"
        [ describe "Trailing text is ignored"
            [ parses "John 3:16; Rom 5:8" "John 3:16"
            , parses "John 3:16, 18" "John 3:16"
            , parses "John 3:16a" "John 3:16"
            , parses "John 3:16." "John 3:16"
            , parses "Gen 1 NIV" "Genesis 1:1-31"
            , parses "Gen 1 esv" "Genesis 1:1-31"
            , parses "Gen 1e5" "Genesis 1:1-31"
            , fails "Gen 1:1-Exod" "No valid reference found"
            ]
        , describe "Book names end at a word boundary"
            [ parses "Psalm 23" "Psalm 23:1-6"
            , parses "Psalms 23" "Psalm 23:1-6"
            , parses "Psalms 23:1-6" "Psalm 23:1-6"
            , parses "Psa 23" "Psalm 23:1-6"
            , parses "Ps 23" "Psalm 23:1-6"
            , parses "Genesis1" "Genesis 1:1-31"
            , parses "1sam1" "1 Samuel 1:1-28"
            , parses "Song of Songs 1" "Song of Solomon 1:1-17"
            , parses "Song 1" "Song of Solomon 1:1-17"
            , parses "Phil 1" "Philippians 1:1-30"
            , parses "Philemon 2" "Philemon 2"
            , fails "Psalmz 23" "No reference found"
            , fails "Genesiss 1" "No reference found"
            ]
        , describe "Dots"
            [ parses "Ps. 23" "Psalm 23:1-6"
            , parses "1 Cor. 13" "1 Corinthians 13:1-13"
            , parses "Matt.5:3" "Matthew 5:3"
            , fails "John 3.16" "Use a colon between chapter and verse, as in John 3:16"
            , fails "Gen 1:1.5" "Use a colon between chapter and verse, as in John 3:16"
            ]
        , describe "Whitespace"
            [ fails "" "No reference found"
            , fails "  " "No reference found"
            , fails "\t\n" "No reference found"
            , fails " x" "No reference found"
            , parses "   Gen 1" "Genesis 1:1-31"
            , parses "Gen 1   " "Genesis 1:1-31"
            , parses "Gen  1 :  1 -  5" "Genesis 1:1-5"
            , parses "Gen\n1:1" "Genesis 1:1"
            , parses "Gen\t1:1" "Genesis 1:1"
            , parses "Gen\u{00A0}1:1" "Genesis 1:1"
            , parses "1\u{00A0}Sam 1" "1 Samuel 1:1-28"
            , parses "1  Sam 1" "1 Samuel 1:1-28"
            , parses "Song  of\tSongs 1" "Song of Solomon 1:1-17"
            ]
        , describe "Numbers"
            [ parses "Gen 01" "Genesis 1:1-31"
            , parses "Gen 1:05" "Genesis 1:5"
            , parses "Gen 001:001-002" "Genesis 1:1-2"
            , fails "Gen 0x1" "Chapter numbers start at 1"
            , fails "Gen 99999999999999999999" "Genesis only has 50 chapters"
            , fails "Gen 1:99999999999999999999" "Genesis 1 only has 31 verses"
            ]
        , describe "Abbreviations"
            [ parses "Jn 3:16" "John 3:16"
            , parses "Jhn 3:16" "John 3:16"
            , parses "Mk 1" "Mark 1:1-45"
            , parses "Lk 2:1" "Luke 2:1"
            , parses "Mt 5:3" "Matthew 5:3"
            , parses "Rm 8:28" "Romans 8:28"
            , parses "Ex 20" "Exodus 20:1-26"
            , parses "Pss 1" "Psalm 1:1-6"
            , parses "Qoh 1" "Ecclesiastes 1:1-18"
            , parses "1 Jn 1:9" "1 John 1:9"
            , parses "I John 1:9" "1 John 1:9"
            , parses "First John 1:9" "1 John 1:9"
            , parses "1st Cor 13" "1 Corinthians 13:1-13"
            , parses "2nd Tim 3:16" "2 Timothy 3:16"
            , parses "III John 2" "3 John 2"
            , parses "Phm 2" "Philemon 2"
            , parses "Rv 22:21" "Revelation 22:21"
            , fails "Kings 1" "No reference found"
            , fails "Xyz 1" "No reference found"
            ]
        , describe "Case"
            [ parses "GEN 1:1" "Genesis 1:1"
            , parses "gEn 1:1" "Genesis 1:1"
            , parses "i sam 1" "1 Samuel 1:1-28"
            ]
        , describe "Single-chapter books"
            [ parses "Jude 3" "Jude 3"
            , parses "Jude 1:3" "Jude 3"
            , parses "Jude 3-5" "Jude 3-5"
            , fails "Jude 2:3" "Jude only has 1 chapter"
            , fails "Jude 26" "Jude only has 25 verses"
            ]
        , describe "Dashes"
            [ parses "Matt 5-7" "Matthew 5:1-7:29"
            , parses "Matt 5–7" "Matthew 5:1-7:29"
            , parses "Matt 5 — 7" "Matthew 5:1-7:29"
            ]
        , describe "Zero chapters and verses"
            [ fails "Gen 0" "Chapter numbers start at 1"
            , fails "Gen 0:0" "Chapter numbers start at 1"
            , fails "Gen 0:1" "Chapter numbers start at 1"
            , fails "Gen 1:0" "Verse numbers start at 1"
            , fails "Gen 1:1-0" "Verse numbers start at 1"
            , fails "Gen 1-0" "Chapter numbers start at 1"
            , fails "Jude 0" "Verse numbers start at 1"
            ]
        , describe "Validation messages"
            [ fails "Ps 151" "Psalm only has 150 chapters"
            , fails "Rev 22:21 - Gen 1" "End book must come after start book"
            , fails "Rev 22:22" "Revelation 22 only has 21 verses"
            , fails "Gen 2-1" "End chapter must come after start chapter"
            , fails "Gen 1:2-1" "End verse must come after start verse"
            ]
        , describe "Decoding"
            [ formats ( 19023001, 19023006 ) "Psalm 23:1-6"
            , formats ( 19001001, 19150006 ) "Psalm 1:1-150:6"
            , formats ( 1001001, 66022021 ) "Genesis 1:1 - Revelation 22:21"
            , decodeFails ( 1000000, 1000000 ) "Chapter numbers start at 1"
            , decodeFails ( 1001000, 1001000 ) "Verse numbers start at 1"
            , decodeFails ( 65000000, 65000000 ) "Chapter numbers start at 1"
            , decodeFails ( 0, 0 ) "Invalid book number"
            , decodeFails ( -1001001, 1001001 ) "Invalid book number"
            , decodeFails ( 67000000, 67000000 ) "Invalid book number"
            , decodeFails ( 1001001, 67001001 ) "Invalid book number"
            , decodeFails ( 4294967297001001, 4294967297001001 ) "Invalid book number"
            , decodeFails ( 66999999, 66999999 ) "Revelation only has 22 chapters"
            ]
        , describe "Aliases"
            [ test "every alias, followed by a chapter, parses to its own book" <|
                \() ->
                    Internal.Parser.aliases
                        |> List.filter
                            (\( alias, book ) ->
                                Internal.Bible.fromString (alias ++ " 1")
                                    |> Result.map .startBook
                                    |> (/=) (Ok book)
                            )
                        |> List.map Tuple.first
                        |> Expect.equalLists []
            , test "every alias still parses in title case and with a dot" <|
                \() ->
                    Internal.Parser.aliases
                        |> List.filter
                            (\( alias, book ) ->
                                Internal.Bible.fromString (String.toUpper (String.left 1 alias) ++ String.dropLeft 1 alias ++ ". 1")
                                    |> Result.map .startBook
                                    |> (/=) (Ok book)
                            )
                        |> List.map Tuple.first
                        |> Expect.equalLists []
            , test "no alias belongs to two books" <|
                \() ->
                    Internal.Parser.aliases
                        |> List.filter
                            (\( alias, book ) ->
                                List.any (\( a, b ) -> a == alias && b /= book) Internal.Parser.aliases
                            )
                        |> List.map (\( alias, book ) -> alias ++ " (" ++ Book.toString book ++ ")")
                        |> Expect.equalLists []
            , test "aliases are lowercase with single spaces" <|
                \() ->
                    Internal.Parser.aliases
                        |> List.map Tuple.first
                        |> List.filter (\a -> a /= String.toLower a || a /= String.join " " (String.words a))
                        |> Expect.equalLists []
            ]
        ]


parse : String -> Result String String
parse =
    Bible.fromString >> Result.map Bible.format


format : ( Int, Int ) -> Result String String
format ( start, end ) =
    Bible.decode { start = start, end = end } |> Result.map Bible.format


parses : String -> String -> Test
parses input expected =
    test (Debug.toString input) <|
        \() -> Expect.equal (Ok expected) (parse input)


fails : String -> String -> Test
fails input message =
    test (Debug.toString input ++ " fails") <|
        \() -> Expect.equal (Err message) (parse input)


formats : ( Int, Int ) -> String -> Test
formats encoded expected =
    test (Debug.toString encoded) <|
        \() -> Expect.equal (Ok expected) (format encoded)


decodeFails : ( Int, Int ) -> String -> Test
decodeFails encoded message =
    test (Debug.toString encoded ++ " fails") <|
        \() -> Expect.equal (Err message) (format encoded)
