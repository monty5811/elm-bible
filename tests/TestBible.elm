module TestBible exposing (suite)

import Array
import Bible exposing (Book(..), Reference(..), bookToString, format, fromString, numChapters, numVerses)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Random
import Shrink
import Test exposing (..)


suite : Test
suite =
    describe "Parse Bible Refs" <|
        List.concat
            [ [ describe "From String"
                    [ testStringToRef "Genesis 1:1" [ Reference Genesis 1 1 1 1 ]
                    , testStringToRef "Rev 20:1-5" [ Reference Revelation 20 1 20 5 ]
                    , testStringToRef "Jude 1:25" [ Reference Jude 1 25 1 25 ]
                    ]
              , describe "Round Trip"
                    [ testRoundTrip "Genesis 1:1"
                    , testRoundTrip "Exodus 1:1-10"
                    , testRoundTrip "John 1:1-14"
                    , testRoundTrip "Jude 1"
                    , testRoundTrip "Jude 1-25"
                    , testRoundTrip "1 John 1:1-5"
                    , testRoundTrip "Exodus 1:1-17:16"
                    ]
              ]
            , List.map fuzzBook books
            ]


fuzzBook book =
    fuzz (ref book) ("Fuzz " ++ bookToString book) t2


t1 : Reference -> () -> Expectation
t1 reference _ =
    t2 reference


t2 : Reference -> Expectation
t2 reference =
    reference
        |> format
        |> Bible.fromString
        |> List.map Result.toMaybe
        |> List.filterMap identity
        |> List.head
        |> Maybe.withDefault (Reference Genesis -1 -1 -1 -1)
        |> Expect.equal reference


testRoundTrip : String -> Test
testRoundTrip str =
    test str <| testRoundTripHelp str


testRoundTripHelp : String -> () -> Expectation
testRoundTripHelp str _ =
    Expect.equal str (roundTripString str)


roundTripString : String -> String
roundTripString str =
    str
        |> Bible.fromString
        |> List.map Result.toMaybe
        |> List.filterMap identity
        |> List.head
        |> Maybe.withDefault (Reference Genesis -1 -1 -1 -1)
        |> format


testStringToRef : String -> List Reference -> Test
testStringToRef str refs =
    test str <| \() -> Expect.equal (List.map Ok refs) (Bible.fromString str)


genRefTup : Book -> Random.Generator { book : Book, startChapter : Int, startVerse : Int, endChapter : Int, endVerse : Int }
genRefTup book =
    Random.int 1 (numChapters book)
        |> Random.andThen (s1 book)
        |> Random.andThen s2
        |> Random.andThen s3


s1 : Book -> Int -> Random.Generator { book : Book, startChapter : Int, startVerse : Int }
s1 book startChapter =
    Random.map (\i -> { book = book, startChapter = startChapter, startVerse = i }) (Random.int 1 (numVerses book startChapter))


s2 : { book : Book, startChapter : Int, startVerse : Int } -> Random.Generator { book : Book, startChapter : Int, startVerse : Int, endChapter : Int }
s2 { book, startChapter, startVerse } =
    Random.map (\i -> { book = book, startChapter = startChapter, startVerse = startVerse, endChapter = i }) (Random.int startChapter (numChapters book))


s3 : { book : Book, startChapter : Int, startVerse : Int, endChapter : Int } -> Random.Generator { book : Book, startChapter : Int, startVerse : Int, endChapter : Int, endVerse : Int }
s3 { book, startChapter, startVerse, endChapter } =
    if startChapter == endChapter then
        Random.map (\i -> { book = book, startChapter = startChapter, startVerse = startVerse, endChapter = endChapter, endVerse = i }) (Random.int startVerse (numVerses book startChapter))

    else
        Random.map (\i -> { book = book, startChapter = startChapter, startVerse = startVerse, endChapter = endChapter, endVerse = i }) (Random.int 1 (numVerses book endChapter))


ref : Book -> Fuzzer Reference
ref inputBook =
    let
        generator =
            Random.map (\{ book, startChapter, startVerse, endChapter, endVerse } -> Reference book startChapter startVerse endChapter endVerse) (genRefTup inputBook)

        shrinker (Reference book sc sv ec ev) =
            Shrink.noShrink <| Reference book sc sv ec ev
    in
    Fuzz.custom generator shrinker


books : List Book
books =
    [ Genesis
    , Exodus
    , Leviticus
    , Numbers
    , Deuteronomy
    , Joshua
    , Judges
    , Ruth
    , Samuel_1
    , Samuel_2
    , Kings_1
    , Kings_2
    , Chronicles_2
    , Chronicles_1
    , Ezra
    , Nehemiah
    , Esther
    , Job
    , Psalms
    , Proverbs
    , Ecclesiastes
    , Song_of_Solomon
    , Isaiah
    , Jeremiah
    , Lamentations
    , Ezekiel
    , Daniel
    , Hosea
    , Joel
    , Amos
    , Obadiah
    , Jonah
    , Micah
    , Nahum
    , Habakkuk
    , Zephaniah
    , Haggai
    , Zechariah
    , Malachi
    , Matthew
    , Mark
    , Luke
    , John
    , Acts
    , Romans
    , Corinthians_1
    , Corinthians_2
    , Galatians
    , Ephesians
    , Philippians
    , Colossians
    , Thessalonians_1
    , Thessalonians_2
    , Timothy_1
    , Timothy_2
    , Titus
    , Philemon
    , Hebrews
    , James
    , Peter_1
    , Peter_2
    , John_1
    , John_2
    , John_3
    , Jude
    , Revelation
    ]
