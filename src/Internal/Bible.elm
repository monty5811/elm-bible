module Internal.Bible exposing
    ( decode
    , encode
    , format
    , fromString
    )

import Internal.Book as Book exposing (Book(..), numChapters, numVerses)
import Internal.Parser exposing (parse, validateRef)
import Internal.Reference exposing (Reference)
import Template as T exposing (render)



-- Format


format : Reference -> String
format ref =
    if ref.startBook == ref.endBook then
        formatHelpSingleBook ref

    else
        case ( numChapters ref.startBook, numChapters ref.endBook ) of
            ( 1, 1 ) ->
                render ref multiBookBothSingleChapterBook

            ( 1, _ ) ->
                render ref multiBookBeginsWithSingleChapterBook

            ( _, 1 ) ->
                render ref multiBookEndsWithSingleChapterBook

            ( _, _ ) ->
                render ref multiBook


formatHelpSingleBook : Reference -> String
formatHelpSingleBook r =
    if numChapters r.startBook == 1 then
        renderSingleChapterBook r.startBook r.startVerse r.endVerse

    else if r.startChapter == r.endChapter then
        if r.startVerse == r.endVerse then
            render
                { book = r.startBook, chapter = r.startChapter, verse = r.startVerse }
                singleBookSingleChapterSingleVerse

        else
            render
                { book = r.startBook, chapter = r.startChapter, startVerse = r.startVerse, endVerse = r.endVerse }
                singleBookSingleChapterMultiVerse

    else
        render
            { book = r.startBook, startChapter = r.startChapter, startVerse = r.startVerse, endChapter = r.endChapter, endVerse = r.endVerse }
            singleBookMultiChapter


renderSingleChapterBook : Book -> Int -> Int -> String
renderSingleChapterBook book sv ev =
    if sv == ev then
        render
            { book = book, verse = sv }
            singleChapterBookSingleVerse

    else
        render
            { book = book, startVerse = sv, endVerse = ev }
            singleChapterBookMultiVerse


multiBookBothSingleChapterBook =
    T.template ""
        |> T.withValue (.startBook >> Book.toString)
        |> T.withString " "
        |> T.withValue (.startVerse >> String.fromInt)
        |> T.withString " - "
        |> T.withValue (.endBook >> Book.toString)
        |> T.withString " "
        |> T.withValue (.endVerse >> String.fromInt)


multiBookBeginsWithSingleChapterBook =
    T.template ""
        |> T.withValue (.startBook >> Book.toString)
        |> T.withString " "
        |> T.withValue (.startVerse >> String.fromInt)
        |> T.withString " - "
        |> T.withValue (.endBook >> Book.toString)
        |> T.withString " "
        |> T.withValue (.endChapter >> String.fromInt)
        |> T.withString ":"
        |> T.withValue (.endVerse >> String.fromInt)


multiBookEndsWithSingleChapterBook =
    T.template ""
        |> T.withValue (.startBook >> Book.toString)
        |> T.withString " "
        |> T.withValue (.startChapter >> String.fromInt)
        |> T.withString ":"
        |> T.withValue (.startVerse >> String.fromInt)
        |> T.withString " - "
        |> T.withValue (.endBook >> Book.toString)
        |> T.withString " "
        |> T.withValue (.endVerse >> String.fromInt)


multiBook =
    T.template ""
        |> T.withValue (.startBook >> Book.toString)
        |> T.withString " "
        |> T.withValue (.startChapter >> String.fromInt)
        |> T.withString ":"
        |> T.withValue (.startVerse >> String.fromInt)
        |> T.withString " - "
        |> T.withValue (.endBook >> Book.toString)
        |> T.withString " "
        |> T.withValue (.endChapter >> String.fromInt)
        |> T.withString ":"
        |> T.withValue (.endVerse >> String.fromInt)


singleChapterBookSingleVerse =
    T.template ""
        |> T.withValue (.book >> Book.toString)
        |> T.withString " "
        |> T.withValue (.verse >> String.fromInt)


singleChapterBookMultiVerse =
    T.template ""
        |> T.withValue (.book >> Book.toString)
        |> T.withString " "
        |> T.withValue (.startVerse >> String.fromInt)
        |> T.withString "-"
        |> T.withValue (.endVerse >> String.fromInt)


singleBookSingleChapterSingleVerse =
    T.template ""
        |> T.withValue (.book >> Book.toString)
        |> T.withString " "
        |> T.withValue (.chapter >> String.fromInt)
        |> T.withString ":"
        |> T.withValue (.verse >> String.fromInt)


singleBookSingleChapterMultiVerse =
    T.template ""
        |> T.withValue (.book >> Book.toString)
        |> T.withString " "
        |> T.withValue (.chapter >> String.fromInt)
        |> T.withString ":"
        |> T.withValue (.startVerse >> String.fromInt)
        |> T.withString "-"
        |> T.withValue (.endVerse >> String.fromInt)


singleBookMultiChapter =
    T.template ""
        |> T.withValue (.book >> Book.toString)
        |> T.withString " "
        |> T.withValue (.startChapter >> String.fromInt)
        |> T.withString ":"
        |> T.withValue (.startVerse >> String.fromInt)
        |> T.withString "-"
        |> T.withValue (.endChapter >> String.fromInt)
        |> T.withString ":"
        |> T.withValue (.endVerse >> String.fromInt)



-- Parse


{-| Parse a string into a `Reference`
-}
fromString : String -> Result String Reference
fromString str =
    parse <| String.toLower str



-- Encoding


encode : Reference -> { start : Int, end : Int }
encode { startBook, startChapter, startVerse, endBook, endChapter, endVerse } =
    { start = encodeHelp startBook startChapter startVerse
    , end = encodeHelp endBook endChapter endVerse
    }


encodeHelp : Book -> Int -> Int -> Int
encodeHelp book chapter verse =
    (Book.toInt book * 1000000)
        + (chapter * 1000)
        + verse


decode : { start : Int, end : Int } -> Result String Reference
decode { start, end } =
    decodeHelp start end
        |> Result.andThen validateRef


decodeHelp : Int -> Int -> Result String Reference
decodeHelp start end =
    let
        startResult =
            decodeHelpInt start

        endResult =
            decodeHelpInt end
    in
    case ( startResult, endResult ) of
        ( Ok ( startBook, startChapter, startVerse ), Ok ( endBook, endChapter, endVerse ) ) ->
            Ok <| Reference startBook startChapter startVerse endBook endChapter endVerse

        ( _, _ ) ->
            Err "Invalid book number"


decodeHelpInt : Int -> Result String ( Book, Int, Int )
decodeHelpInt num =
    let
        bookResult =
            num
                // 1000000
                |> Book.fromInt

        verse =
            remainderBy 1000 num

        chapter =
            num
                // 1000
                |> remainderBy 1000
    in
    case bookResult of
        Err err ->
            Err "Invalid book number"

        Ok book ->
            Ok ( book, chapter, verse )
