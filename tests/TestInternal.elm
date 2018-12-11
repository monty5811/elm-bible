module TestInternal exposing (suite)

import Expect exposing (Expectation)
import Fuzz
import Internal.Bible as Internal
import Internal.Book as Book exposing (Book(..), books)
import Internal.Parser as P
import Internal.Reference exposing (Reference)
import Test exposing (..)


suite : Test
suite =
    describe "Parse Bible Refs" <|
        [ describe "From String"
            [ testStringToRef "Genesis 1:1" <| ref Genesis 1 1 Genesis 1 1
            , testStringToRef "Rev 20:1-5" <| ref Revelation 20 1 Revelation 20 5
            , testStringToRef "Jude 1:25" <| ref Jude 1 25 Jude 1 25
            , testStringToRef "Genesis 2:1" <| ref Genesis 2 1 Genesis 2 1
            , testStringToRef "Genesis 1" <| ref Genesis 1 1 Genesis 1 31
            , testStringToRef "Genesis 1:1-20" <| ref Genesis 1 1 Genesis 1 20
            , testStringToRef "Genesis 1:20-2:24" <| ref Genesis 1 20 Genesis 2 24
            , testStringToRef "Genesis 1-5" <| ref Genesis 1 1 Genesis 5 32
            , testStringToRef "Genesis 1 - Exodus 5" <| ref Genesis 1 1 Exodus 5 23
            , testStringToRef "Genesis 1:1 - Exodus 5:20" <| ref Genesis 1 1 Exodus 5 20
            , testStringToRef "Genesis 1:1 - Exodus 5" <| ref Genesis 1 1 Exodus 5 23
            , testStringToRef "Genesis 1 - Exodus 5:20" <| ref Genesis 1 1 Exodus 5 20
            , testStringToRef "Jude 1" <| ref Jude 1 1 Jude 1 1
            , testStringToRef "Jude" <| ref Jude 1 1 Jude 1 25
            , testStringToRef "Jude 1-5" <| ref Jude 1 1 Jude 1 5
            , testStringToRef "3 John 1" <| ref John_3 1 1 John_3 1 1
            , testStringToRef "3 John" <| ref John_3 1 1 John_3 1 14
            , testStringToRef "3 John 1-5" <| ref John_3 1 1 John_3 1 5
            , testStringToRef "Matthew 1 - Jude 12" <| ref Matthew 1 1 Jude 1 12
            , testStringToRef "Matthew 1:1 - Jude 12" <| ref Matthew 1 1 Jude 1 12
            , testStringToRef "Jude 12 - Revelation 1:1" <| ref Jude 1 12 Revelation 1 1
            , testStringToRef "3 John 3 - Jude 12" <| ref John_3 1 3 Jude 1 12
            , testStringToRef "Genesis - Revelation" <| ref Genesis 1 1 Revelation 22 21
            , testStringToRef "Genesis 1 - Jude 5" <| ref Genesis 1 1 Jude 1 5
            , testStringToRef "Genesis - Jude 5" <| ref Genesis 1 1 Jude 1 5
            , testStringToRef "Genesis 1:1 - Jude 5" <| ref Genesis 1 1 Jude 1 5
            , testStringToRef "3 John 2 - Revelation 1" <| ref John_3 1 2 Revelation 1 20
            , testStringToRef "3 John 2 - Revelation 1:1" <| ref John_3 1 2 Revelation 1 1
            , testStringToRef "3 John - Revelation 1" <| ref John_3 1 1 Revelation 1 20
            ]
        , describe "Parseable, but invalid"
            [ testStringToErr "Genesis 52" "Genesis only has 50 chapters"
            , testStringToErr "Genesis 1:32" "Genesis 1 only has 31 verses"
            , testStringToErr "Jude 32" "Jude only has 25 verses"
            , testStringToErr "Mark 2-1" "End chapter must come after start chapter"
            , testStringToErr "Luke 1:10-6" "End verse must come after start verse"
            , testStringToErr "Revelation - Genesis" "End book must come after start book"
            ]
        , describe "Round Trip"
            [ testRoundTrip "Genesis 1:1"
            , testRoundTrip "Exodus 1:1-10"
            , testRoundTrip "John 1:1-14"
            , testRoundTrip "Jude 1"
            , testRoundTrip "Jude 1-25"
            , testRoundTrip "1 John 1:1-5"
            , testRoundTrip "Exodus 1:1-17:16"
            , testRoundTrip "Genesis 1:1 - Exodus 1:22"
            , testRoundTrip "Matthew 1:1 - Jude 12"
            , testRoundTrip "3 John 3 - Jude 12"
            , testRoundTrip "Jude 12 - Revelation 1:1"
            ]
        , fuzz fuzzRef "If generated ref is valid, round trips it through the encoder and the formatter" fuzzRoundTrip
        , describe "Found by fuzzer"
            [ testRefRoundTrip "Ecc 1:1 - 1 Cor 1:1" { endBook = Corinthians_1, endChapter = 1, endVerse = 1, startBook = Ecclesiastes, startChapter = 1, startVerse = 1 }
            , testRefRoundTrip "Judges 1:1 - 1 Samuel 1:1" { endBook = Samuel_1, endChapter = 1, endVerse = 1, startBook = Judges, startChapter = 1, startVerse = 1 }
            , testRefRoundTrip "Judges 1:1 - Daniel 1:1" { endBook = Daniel, endChapter = 1, endVerse = 1, startBook = Judges, startChapter = 1, startVerse = 1 }
            , testRefRoundTrip "Ezra 1:2 - Jude 1" { endBook = Jude, endChapter = 1, endVerse = 1, startBook = Ezra, startChapter = 1, startVerse = 2 }
            ]
        ]


ref : Book -> Int -> Int -> Book -> Int -> Int -> Reference
ref sb sc sv eb ec ev =
    Reference sb sc sv eb ec ev


testRoundTrip : String -> Test
testRoundTrip str =
    test str <| testRoundTripHelp str


testRoundTripHelp : String -> () -> Expectation
testRoundTripHelp str _ =
    Expect.equal str (roundTripString str)


roundTripString : String -> String
roundTripString str =
    str
        |> Internal.fromString
        |> Result.withDefault (Reference Genesis -1 -1 Genesis -1 -1)
        |> Internal.format


testStringToRef : String -> Reference -> Test
testStringToRef str ref_ =
    test str <| \() -> Expect.equal (Ok ref_) (Internal.fromString str)


testStringToErr : String -> String -> Test
testStringToErr refStr errStr =
    test refStr <| \() -> Expect.equal (Err errStr) (Internal.fromString refStr)


testRefRoundTrip : String -> Reference -> Test
testRefRoundTrip name ref_ =
    test name <|
        \() ->
            Expect.all
                [ \r -> Expect.equal (Ok r) (r |> Internal.format |> Internal.fromString)
                , \r -> Expect.equal (Ok r) (r |> Internal.encode |> Internal.decode)
                ]
                ref_



-- Fuzzing


fuzzRoundTrip : Result String Reference -> Expect.Expectation
fuzzRoundTrip res =
    case res of
        Err _ ->
            Expect.pass

        -- let an invalid reference pass as it is a fuzzing issue, not a test issue
        Ok ref_ ->
            Expect.all
                [ \r -> Expect.equal (Ok r) (r |> Internal.format |> Internal.fromString)
                , \r -> Expect.equal (Ok r) (r |> Internal.encode |> Internal.decode)
                ]
                ref_


{-| This is a very inefficient fuzzer as it won't generate a valid reference most of the time.
If we had `Fuzz.andThen` it would be much easier to limit the chapter once a book is chosen, and so on.

(<https://github.com/elm-explorations/test/issues/17>)

-}
fuzzRef : Fuzz.Fuzzer (Result String Reference)
fuzzRef =
    Fuzz.map ref fuzzBook
        |> Fuzz.andMap fuzzChapter
        |> Fuzz.andMap fuzzVerse
        |> Fuzz.andMap fuzzBook
        |> Fuzz.andMap fuzzChapter
        |> Fuzz.andMap fuzzVerse
        |> Fuzz.map P.validateRef


fuzzBook : Fuzz.Fuzzer Book
fuzzBook =
    Fuzz.oneOf <|
        List.map Fuzz.constant books


fuzzChapter : Fuzz.Fuzzer Int
fuzzChapter =
    Fuzz.intRange 1 150


fuzzVerse : Fuzz.Fuzzer Int
fuzzVerse =
    Fuzz.intRange 1 176
