port module Harness exposing (main)

{-| Runs the elm-bible package in ../../src (or a published version) over a batch of inputs so
rust/compat/corpus.py can compare it with the Rust port.

Flags: { "parse": [String], "format": [[Int, Int]] }
Output (through the `done` port): { "parse": [Result], "format": [Result] }
where a Result is { "ok": [start, end] } / { "ok": "Genesis 1:1" } or { "err": "message" }.

-}

import Bible
import Json.Decode as D
import Json.Encode as E


port done : E.Value -> Cmd msg


type alias Flags =
    { parse : List String
    , format : List ( Int, Int )
    }


flagsDecoder : D.Decoder Flags
flagsDecoder =
    D.map2 Flags
        (D.field "parse" (D.list D.string))
        (D.field "format" (D.list (D.map2 Tuple.pair (D.index 0 D.int) (D.index 1 D.int))))


result : (a -> E.Value) -> Result String a -> E.Value
result encodeOk r =
    case r of
        Ok value ->
            E.object [ ( "ok", encodeOk value ) ]

        Err message ->
            E.object [ ( "err", E.string message ) ]


parseOne : String -> E.Value
parseOne text =
    Bible.fromString text
        |> Result.map Bible.encode
        |> result (\{ start, end } -> E.list E.int [ start, end ])


formatOne : ( Int, Int ) -> E.Value
formatOne ( start, end ) =
    Bible.decode { start = start, end = end }
        |> Result.map Bible.format
        |> result E.string


run : D.Value -> E.Value
run flags =
    case D.decodeValue flagsDecoder flags of
        Ok { parse, format } ->
            E.object
                [ ( "parse", E.list parseOne parse )
                , ( "format", E.list formatOne format )
                ]

        Err error ->
            E.object [ ( "error", E.string (D.errorToString error) ) ]


main : Program D.Value () Never
main =
    Platform.worker
        { init = \flags -> ( (), done (run flags) )
        , update = \_ model -> ( model, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }
