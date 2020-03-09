module TestBook exposing (suite)

import Expect exposing (Expectation)
import Internal.Book as Book exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Check Bible data"
        [ describe "Check chapter numbers is same length as verses list" <| List.map testChapterNum books
        , test "Check book order" <| \() -> bookOrder
        ]


testChapterNum : Book -> Test
testChapterNum book =
    test (toString book) <|
        \() ->
            Expect.equal
                (numChapters book)
                ((data >> .numVerses >> List.length) book)


bookOrder : Expectation
bookOrder =
    Expect.equal
        (List.map Book.toInt books)
        (List.range 1 66)
