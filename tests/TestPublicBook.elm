module TestPublicBook exposing (suite)

import Bible
import Bible.Book as Book exposing (Book)
import Expect
import Fuzz
import Test exposing (Test, describe, fuzz, test)


suite : Test
suite =
    describe "Bible.Book"
        [ test "all lists 66 books from Genesis to Revelation" <|
            \() ->
                Expect.equal
                    ( 66, List.head (List.map Book.name Book.all), List.head (List.reverse (List.map Book.name Book.all)) )
                    ( List.length Book.all, Just "Genesis", Just "Revelation" )
        , test "all is in order of toInt" <|
            \() -> Expect.equal (List.range 1 66) (List.map Book.toInt Book.all)
        , fuzz (Fuzz.intRange 1 66) "fromInt and toInt round trip" <|
            \n -> Expect.equal (Just n) (Maybe.map Book.toInt (Book.fromInt n))
        , test "fromInt rejects numbers outside 1-66" <|
            \() -> Expect.equal [ Nothing, Nothing, Nothing ] (List.map Book.fromInt [ 0, 67, -1 ])
        , test "name and title differ only for Psalms" <|
            \() ->
                Book.all
                    |> List.filter (\b -> Book.name b /= Book.title b)
                    |> List.map (\b -> ( Book.name b, Book.title b ))
                    |> Expect.equal [ ( "Psalm", "Psalms" ) ]
        , test "names" <|
            \() ->
                List.filterMap Book.fromInt [ 1, 9, 22, 62, 66 ]
                    |> List.map Book.name
                    |> Expect.equal [ "Genesis", "1 Samuel", "Song of Solomon", "1 John", "Revelation" ]
        , test "numChapters" <|
            \() ->
                List.filterMap Book.fromInt [ 1, 19, 65 ]
                    |> List.map Book.numChapters
                    |> Expect.equal [ 50, 150, 1 ]
        , test "numVerses" <|
            \() ->
                psalms
                    |> Maybe.map (\ps -> [ Book.numVerses ps 23, Book.numVerses ps 119, Book.numVerses ps 0, Book.numVerses ps 151 ])
                    |> Expect.equal (Just [ 6, 176, 0, 0 ])
        , test "aliases" <|
            \() ->
                Expect.equal
                    (Just [ "psalms", "psalm", "pslm", "psm", "pss", "psa", "ps" ])
                    (Maybe.map Book.aliases psalms)
        , test "every book has an alias, and every alias parses to its book" <|
            \() ->
                Book.all
                    |> List.filter
                        (\b ->
                            List.isEmpty (Book.aliases b)
                                || List.any (\a -> Result.map Bible.toStartBook (Bible.fromString (a ++ " 1")) /= Ok b) (Book.aliases b)
                        )
                    |> List.map Book.name
                    |> Expect.equalLists []
        , describe "toStartBook and toEndBook"
            [ test "one book" <|
                \() ->
                    Bible.fromString "Ps 23"
                        |> Result.map (\r -> ( Book.title (Bible.toStartBook r), Book.title (Bible.toEndBook r) ))
                        |> Expect.equal (Ok ( "Psalms", "Psalms" ))
            , test "two books" <|
                \() ->
                    Bible.fromString "Gen 1 - Exod 2"
                        |> Result.map (\r -> ( Book.toInt (Bible.toStartBook r), Book.toInt (Bible.toEndBook r) ))
                        |> Expect.equal (Ok ( 1, 2 ))
            , test "match the encoding" <|
                \() ->
                    Bible.decode { start = 40005003, end = 43003016 }
                        |> Result.map (\r -> ( Book.toInt (Bible.toStartBook r), Book.toInt (Bible.toEndBook r) ))
                        |> Expect.equal (Ok ( 40, 43 ))
            ]
        ]


psalms : Maybe Book
psalms =
    Book.fromInt 19
