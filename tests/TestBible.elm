module TestBible exposing (suite)

import Bible exposing (..)
import Expect exposing (Expectation)
import Test exposing (..)


suite : Test
suite =
    describe "Test Bible"
        [ describe "Test encoding"
            [ testRoundTrip "Genesis 1:1" { start = 1001001, end = 1001001 }
            ]
        ]


type alias Encoded =
    { start : Int, end : Int }


testRoundTrip : String -> Encoded -> Test
testRoundTrip str ref_ =
    test str <| \() -> testRoundTripHelp ref_


testRoundTripHelp : Encoded -> Expectation
testRoundTripHelp ref_ =
    Expect.equal (Ok ref_) (roundTripRef ref_)


roundTripRef : { start : Int, end : Int } -> Result String Encoded
roundTripRef ref_ =
    case decode ref_ of
        Ok r ->
            Ok <| encode r

        Err err ->
            Err err
