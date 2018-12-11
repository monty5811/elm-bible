module Internal.Parser exposing (parse, validateRef)

import Internal.Book as Book exposing (Book(..), numChapters, numVerses)
import Internal.Reference exposing (Reference)
import Parser as P exposing ((|.), (|=))



-- Parse


parse : String -> Result String Reference
parse str =
    P.run parser str
        |> Result.mapError P.deadEndsToString
        |> Result.andThen processStatements


type Statement
    = BookName Book
    | Num Int
    | Dash
    | Colon


processStatements : List Statement -> Result String Reference
processStatements stmts =
    processStatementsHelp stmts
        |> Result.andThen validateRef


reference : Book -> Int -> Int -> Book -> Int -> Int -> Result String Reference
reference startBook startChapter startVerse endBook endChapter endVerse =
    Reference startBook startChapter startVerse endBook endChapter endVerse
        |> validateRef


processStatementsHelp : List Statement -> Result String Reference
processStatementsHelp stmts =
    case stmts of
        -- Gen
        [ BookName bk ] ->
            reference
                bk
                1
                1
                bk
                (numChapters bk)
                (numVerses bk (numChapters bk))

        -- Gen 1
        [ BookName bk, Num ch ] ->
            if numChapters bk == 1 then
                reference
                    bk
                    1
                    ch
                    bk
                    1
                    ch

            else
                reference
                    bk
                    ch
                    1
                    bk
                    ch
                    (numVerses bk 1)

        -- Gen 1:5
        [ BookName bk, Num ch, Colon, Num vs ] ->
            reference
                bk
                ch
                vs
                bk
                ch
                vs

        -- Gen 1:5-20
        [ BookName bk, Num ch, Colon, Num vs, Dash, Num endVs ] ->
            reference
                bk
                ch
                vs
                bk
                ch
                endVs

        -- Gen 1 - 2:5
        [ BookName bk, Num ch, Colon, Num vs, Dash, Num endCh, Colon, Num endVs ] ->
            reference
                bk
                ch
                vs
                bk
                endCh
                endVs

        -- Gen 1-2
        [ BookName bk, Num ch, Dash, Num endCh ] ->
            if numChapters bk == 1 then
                reference
                    bk
                    1
                    ch
                    bk
                    1
                    endCh

            else
                reference
                    bk
                    ch
                    1
                    bk
                    endCh
                    (numVerses bk endCh)

        -- Gen 1 - Exodus 5
        [ BookName startBk, Num ch, Dash, BookName endBk, Num endCh ] ->
            if (numChapters endBk == 1) && (numChapters startBk == 1) then
                reference
                    startBk
                    1
                    ch
                    endBk
                    1
                    endCh

            else if numChapters endBk == 1 then
                reference
                    startBk
                    ch
                    1
                    endBk
                    1
                    endCh

            else if numChapters startBk == 1 then
                reference
                    startBk
                    1
                    ch
                    endBk
                    endCh
                    (numVerses endBk endCh)

            else
                reference
                    startBk
                    ch
                    1
                    endBk
                    endCh
                    (numVerses endBk endCh)

        -- Gen 1:1 - Exodus 5
        [ BookName startBk, Num ch, Colon, Num vs, Dash, BookName endBk, Num endCh ] ->
            if numChapters endBk == 1 then
                reference
                    startBk
                    ch
                    vs
                    endBk
                    1
                    endCh

            else
                reference
                    startBk
                    ch
                    vs
                    endBk
                    endCh
                    (numVerses endBk endCh)

        -- Gen 1:1 - Exodus 5:5
        [ BookName startBk, Num ch, Colon, Num vs, Dash, BookName endBk, Num endCh, Colon, Num endVs ] ->
            reference
                startBk
                ch
                vs
                endBk
                endCh
                endVs

        -- Gen 1 - Exodus 2:5
        [ BookName startBk, Num ch, Dash, BookName endBk, Num endCh, Colon, Num endVs ] ->
            if numChapters startBk == 1 then
                reference
                    startBk
                    1
                    ch
                    endBk
                    endCh
                    endVs

            else
                reference
                    startBk
                    ch
                    1
                    endBk
                    endCh
                    endVs

        -- Genesis - Revelation 5
        [ BookName startBk, Dash, BookName endBk, Num endCh ] ->
            if numChapters endBk == 1 then
                reference
                    startBk
                    1
                    1
                    endBk
                    1
                    endCh

            else
                reference
                    startBk
                    1
                    1
                    endBk
                    endCh
                    (numVerses endBk endCh)

        -- Genesis - Revelation
        [ BookName startBk, Dash, BookName endBk ] ->
            reference
                startBk
                1
                1
                endBk
                (numChapters endBk)
                (numVerses endBk (numChapters endBk))

        [] ->
            Err "No reference found"

        _ ->
            Err <| "No valid reference found"


parser : P.Parser (List Statement)
parser =
    P.loop [] statementsHelp


statementsHelp : List Statement -> P.Parser (P.Step (List Statement) (List Statement))
statementsHelp revStmts =
    P.oneOf
        [ P.succeed (\stmt -> P.Loop (stmt :: revStmts))
            |. P.spaces
            |= statement
            |. P.spaces
        , P.succeed ()
            |> P.map (\_ -> P.Done (List.reverse revStmts))
        ]


statement : P.Parser Statement
statement =
    P.oneOf
        [ P.map BookName (P.oneOf bookTokensList)
        , P.map (\_ -> Dash) (P.symbol "-")
        , P.map (\_ -> Colon) (P.symbol ":")
        , P.map Num P.int
        ]


pBook : P.Parser Book
pBook =
    P.oneOf bookTokensList


bookTokensList : List (P.Parser Book)
bookTokensList =
    List.concatMap tokensToParsers bookTokens


tokensToParsers : ( Book, List String ) -> List (P.Parser Book)
tokensToParsers ( book, tokens ) =
    List.map (\t -> P.map (\_ -> book) (P.token t)) tokens


bookTokens : List ( Book, List String )
bookTokens =
    [ ( Genesis, [ "genesis", "gen" ] )
    , ( Exodus, [ "exodus", "exod" ] )
    , ( Leviticus, [ "leviticus", "lev" ] )
    , ( Numbers, [ "numbers", "num" ] )
    , ( Deuteronomy, [ "deuteronomy", "deut" ] )
    , ( Joshua, [ "joshua", "josh" ] )
    , ( Judges, [ "judges", "judg" ] )
    , ( Ruth, [ "ruth" ] )
    , ( Samuel_1, [ "1 samuel", "i samuel", "1 sam", "i sam", "1sam" ] )
    , ( Samuel_2, [ "2 samuel", "ii samuel", "2 sam", "ii sam", "2sam" ] )
    , ( Kings_1, [ "1 kings", "i kings", "1 kgs", "i kgs", "1kings" ] )
    , ( Kings_2, [ "2 kings", "ii kings", "2 kgs", "ii kgs", "2kings" ] )
    , ( Chronicles_1, [ "1 chronicles", "i chronicles", "1 chr", "i chr", "1chr" ] )
    , ( Chronicles_2, [ "2 chronicles", "ii chronicles", "2 chr", "ii chr", "2chr" ] )
    , ( Ezra, [ "ezra" ] )
    , ( Nehemiah, [ "nehemiah", "neh" ] )
    , ( Esther, [ "esther", "esth" ] )
    , ( Job, [ "job" ] )
    , ( Psalms, [ "psalm", "ps" ] )
    , ( Proverbs, [ "proverbs", "prov" ] )
    , ( Ecclesiastes, [ "ecclesiastes", "eccl", "ecc" ] )
    , ( Song_of_Solomon, [ "song of solomon", "song of songs", "song" ] )
    , ( Isaiah, [ "isaiah", "isa" ] )
    , ( Jeremiah, [ "jeremiah", "jer" ] )
    , ( Lamentations, [ "lamentations", "lam" ] )
    , ( Ezekiel, [ "ezekiel", "ezek" ] )
    , ( Daniel, [ "daniel", "dan" ] )
    , ( Hosea, [ "hosea", "hos" ] )
    , ( Joel, [ "joel" ] )
    , ( Amos, [ "amos" ] )
    , ( Obadiah, [ "obadiah", "obad" ] )
    , ( Jonah, [ "jonah" ] )
    , ( Micah, [ "micah", "mic" ] )
    , ( Nahum, [ "nahum", "nah" ] )
    , ( Habakkuk, [ "habakkuk", "hab" ] )
    , ( Zephaniah, [ "zephaniah", "zeph" ] )
    , ( Haggai, [ "haggai", "hag" ] )
    , ( Zechariah, [ "zechariah", "zech" ] )
    , ( Malachi, [ "malachi", "mal" ] )
    , ( Matthew, [ "matthew", "matt" ] )
    , ( Mark, [ "mark" ] )
    , ( Luke, [ "luke" ] )
    , ( Acts, [ "acts" ] )
    , ( Romans, [ "romans", "rom" ] )
    , ( Corinthians_1, [ "1 corinthians", "i corinthians", "1 cor", "i cor", "1cor" ] )
    , ( Corinthians_2, [ "2 corinthians", "ii corinthians", "2 cor", "ii cor", "2cor" ] )
    , ( Galatians, [ "galatians", "gal" ] )
    , ( Ephesians, [ "ephesians", "eph" ] )
    , ( Colossians, [ "colossians", "col" ] )
    , ( Thessalonians_1, [ "1 thessalonians", "i thessalonians", "1 thess", "i thess", "1thess" ] )
    , ( Thessalonians_2, [ "2 thessalonians", "ii thessalonians", "2 thess", "ii thess", "2thess" ] )
    , ( Timothy_1, [ "1 timothy", "i timothy", "1 tim", "i tim", "1tim" ] )
    , ( Timothy_2, [ "2 timothy", "ii timothy", "2 tim", "ii tim", "2tim" ] )
    , ( Titus, [ "titus", "tit" ] )
    , ( Philemon, [ "philemon", "phlm" ] )
    , ( Hebrews, [ "hebrews", "heb" ] )
    , ( James, [ "james", "jas" ] )
    , ( Peter_1, [ "1 peter", "i peter", "1 pet", "i pet", "1pet" ] )
    , ( Peter_2, [ "2 peter", "ii peter", "2 pet", "ii pet", "2pet" ] )
    , ( John_1, [ "1 john", "i john" ] )
    , ( John_2, [ "2 john", "ii john" ] )
    , ( John_3, [ "3 john", "iii john" ] )
    , ( Jude, [ "jude" ] )
    , ( Revelation, [ "revelation", "rev" ] )
    , ( John, [ "john" ] )
    , ( Philippians, [ "philippians", "phil" ] )
    ]



-- Validate


validateRef : Reference -> Result String Reference
validateRef ref =
    validateBookOrder ref
        |> Result.andThen validateChapterOrder
        |> Result.andThen validateVerseOrder
        |> Result.andThen validateChapterBounds
        |> Result.andThen validateVerseBounds


validateBookOrder : Reference -> Result String Reference
validateBookOrder ({ startBook, endBook } as ref) =
    if Book.toInt startBook > Book.toInt endBook then
        Err "End book must come after start book"

    else
        Ok ref


validateChapterOrder : Reference -> Result String Reference
validateChapterOrder ({ startBook, startChapter, endBook, endChapter } as ref) =
    if (startBook == endBook) && (startChapter > endChapter) then
        Err "End chapter must come after start chapter"

    else
        Ok ref


validateVerseOrder : Reference -> Result String Reference
validateVerseOrder ({ startBook, startChapter, endBook, endChapter, startVerse, endVerse } as ref) =
    if (startBook == endBook) && (startChapter == endChapter) && (startVerse > endVerse) then
        Err "End verse must come after start verse"

    else
        Ok ref


validateChapterBounds : Reference -> Result String Reference
validateChapterBounds ({ startBook, startChapter, endChapter, endBook } as ref) =
    if numChapters startBook < startChapter then
        Err <| Book.toString startBook ++ " only has " ++ String.fromInt (numChapters startBook) ++ " chapters"

    else if numChapters endBook < endChapter then
        Err <| Book.toString endBook ++ " only has " ++ String.fromInt (numChapters endBook) ++ " chapters"

    else
        Ok ref


validateVerseBounds : Reference -> Result String Reference
validateVerseBounds ({ startBook, startChapter, startVerse, endBook, endChapter, endVerse } as ref) =
    if numVerses startBook startChapter < startVerse then
        Err <| validateVerseBoundsMsg startBook startChapter startVerse

    else if numVerses endBook endChapter < endVerse then
        Err <| validateVerseBoundsMsg endBook endChapter endVerse

    else
        Ok ref


validateVerseBoundsMsg : Book -> Int -> Int -> String
validateVerseBoundsMsg book chapter verse =
    if numChapters book == 1 then
        Book.toString book ++ " only has " ++ String.fromInt (numVerses book chapter) ++ " verses"

    else
        Book.toString book ++ " " ++ String.fromInt chapter ++ " only has " ++ String.fromInt (numVerses book chapter) ++ " verses"
