module TestEdgeCases exposing (suite)

{-| Pins down how the package handles malformed and unusual input.

Several of these results are surprising (trailing text is ignored, tabs aren't
whitespace, zero is a valid chapter...). They are recorded as the current
behaviour, not as the intended one: if you change one on purpose, update its
test in the same change, so the behaviour change is visible in review.

-}

import Bible
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Edge cases"
        [ describe "Trailing text is ignored"
            [ parses "John 3:16; Rom 5:8" "John 3:16"
            , parses "John 3:16, 18" "John 3:16"
            , parses "Gen 1 NIV" "Genesis 1:1-31"
            , fails "Gen 1:1-Exod" "No valid reference found"
            ]
        , describe "Aliases match as prefixes"
            [ parses "Psalms 23" "Psalm 1:1-150:6"
            , parses "Psa 23" "Psalm 1:1-150:6"
            , parses "Genesis1" "Genesis 1:1-31"
            , parses "1sam1" "1 Samuel 1:1-28"
            ]
        , describe "Dots, exponents and number prefixes"
            [ fails "Gen 1 esv" "TODO deadEndsToString"
            , fails "Ps. 23" "TODO deadEndsToString"
            , fails "John 3:16." "TODO deadEndsToString"
            , fails "Gen 1e5" "TODO deadEndsToString"
            , fails "Gen 0x1" "TODO deadEndsToString"
            ]
        , describe "Whitespace"
            [ fails "" "No reference found"
            , fails "  " "TODO deadEndsToString"
            , fails " x" "TODO deadEndsToString"
            , parses "   Gen 1" "Genesis 1:1-31"
            , parses "Gen  1 :  1 -  5" "Genesis 1:1-5"
            , parses "Gen\n1:1" "Genesis 1:1"
            , test "tab isn't whitespace, so this is the whole book" <|
                \() -> Expect.equal (Ok "Genesis 1:1-50:26") (parse "Gen\t1:1")
            ]
        , describe "Leading zeros split a number"
            [ fails "Gen 01" "No valid reference found"
            , fails "Gen 1:05" "No valid reference found"
            ]
        , describe "Unknown abbreviations"
            [ fails "Jn 3:16" "No reference found"
            , fails "Kings 1" "No reference found"
            ]
        , describe "Case"
            [ parses "GEN 1:1" "Genesis 1:1"
            , parses "gEn 1:1" "Genesis 1:1"
            , parses "i sam 1" "1 Samuel 1:1-28"
            ]
        , describe "Single-chapter books"
            [ parses "Jude 3" "Jude 3"
            , parses "Jude 1:3" "Jude 3"
            , fails "Jude 2:3" "Jude only has 1 chapters"
            ]
        , describe "Huge numbers"
            [ fails "Gen 99999999999999999999" "Genesis only has 50 chapters"
            , fails "Gen 1:99999999999999999999" "Genesis 1 only has 31 verses"
            ]
        , describe "Dashes"
            [ parses "Matt 5-7" "Matthew 5:1-7:29"
            , parses "Matt 5–7" "Matthew 5:1-7:29"
            , parses "Matt 5 — 7" "Matthew 5:1-7:29"
            ]
        , describe "Zero chapters and verses"
            [ parses "Gen 0:0" "Genesis 0:0"
            , parses "Gen 1:0" "Genesis 1:0"
            , fails "Gen 0" "Genesis 0 only has 0 verses"
            , fails "Gen 0:1" "Genesis 0 only has 0 verses"
            ]
        , describe "Validation messages"
            [ fails "Ps 151" "Psalm only has 150 chapters"
            , fails "Rev 22:21 - Gen 1" "End book must come after start book"
            , fails "Rev 22:22" "Revelation 22 only has 21 verses"
            ]
        , describe "Decoding"
            [ formats ( 19023001, 19023006 ) "Psalm 23:1-6"
            , formats ( 19001001, 19150006 ) "Psalm 1:1-150:6"
            , formats ( 1000000, 1000000 ) "Genesis 0:0"
            , formats ( 65000000, 65000000 ) "Jude 0"
            , test "values past 2^32 * 10^6 wrap to a valid book" <|
                \() -> Expect.equal (Ok "Genesis 1:1") (format ( 4294967297001001, 4294967297001001 ))
            , test "book 0 is an error" <|
                \() -> Expect.err (format ( 0, 0 ))
            , test "book 67 is an error" <|
                \() -> Expect.err (format ( 67000000, 67000000 ))
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
