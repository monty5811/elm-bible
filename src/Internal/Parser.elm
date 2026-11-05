module Internal.Parser exposing (aliases, parse, validateRef)

import Internal.Book as Book exposing (Book(..), numChapters, numVerses)
import Internal.Reference exposing (Reference)
import Parser as P exposing ((|.), (|=))



-- Parse


{-| Parse a reference. Case and whitespace don't matter: the input is
lowercased and every run of whitespace (tabs, non-breaking spaces...) becomes a
single space before parsing.
-}
parse : String -> Result String Reference
parse str =
    P.run parser (normalize str)
        |> Result.mapError deadEndsToString
        |> Result.andThen processStatements


normalize : String -> String
normalize =
    String.toLower >> String.words >> String.join " "


deadEndsToString : List P.DeadEnd -> String
deadEndsToString deadEnds =
    deadEnds
        |> List.filterMap
            (\{ problem } ->
                case problem of
                    P.Problem message ->
                        Just message

                    _ ->
                        Nothing
            )
        |> List.head
        |> Maybe.withDefault "Could not understand the reference"


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
                    (numVerses bk ch)

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


{-| Reads statements until it meets something that isn't one. Anything after
that is ignored. No statement consumes input before failing, so stopping
early is never a parse error.
-}
statementsHelp : List Statement -> P.Parser (P.Step (List Statement) (List Statement))
statementsHelp revStmts =
    P.oneOf
        [ P.succeed (\stmt -> P.Loop (stmt :: revStmts))
            |= statement
            |. P.chompWhile ((==) ' ')
        , P.succeed ()
            |> P.map (\_ -> P.Done (List.reverse revStmts))
        ]


statement : P.Parser Statement
statement =
    P.oneOf
        [ P.succeed BookName
            |= pBook
            |. P.oneOf [ P.symbol ".", P.succeed () ]
        , P.map (\_ -> Dash) (P.oneOf [ P.symbol "-", P.symbol "—", P.symbol "–" ])
        , P.map (\_ -> Colon) (P.symbol ":")
        , P.map Num number
        ]


{-| A run of digits. Leading zeros are fine (`05` is 5). A `.` between two
numbers is rejected rather than ignored, so `John 3.16` is an error instead
of quietly becoming John 3.
-}
number : P.Parser Int
number =
    P.getChompedString (P.chompIf Char.isDigit |. P.chompWhile Char.isDigit)
        |> P.andThen
            (\digits ->
                P.oneOf
                    [ P.succeed ()
                        |. P.backtrackable (P.symbol ".")
                        |. P.chompIf Char.isDigit
                        |> P.andThen (\_ -> P.problem "Use a colon between chapter and verse, as in John 3:16")
                    , P.succeed (String.toInt digits |> Maybe.withDefault 0)
                    ]
            )


pBook : P.Parser Book
pBook =
    P.oneOf bookTokensList


{-| Every alias, longest first, so that `song of songs` is tried before
`song`. An alias only matches if no letter follows it: `psalms` isn't read
as `psalm` plus `s`, but `genesis1` is still Genesis 1.
-}
bookTokensList : List (P.Parser Book)
bookTokensList =
    aliases
        |> List.sortBy (\( token, _ ) -> negate (String.length token))
        |> List.map
            (\( token, book ) ->
                P.backtrackable
                    (P.succeed book
                        |. P.token token
                        |. notFollowedByLetter
                    )
            )


notFollowedByLetter : P.Parser ()
notFollowedByLetter =
    P.succeed (\offset source -> String.slice offset (offset + 2) source)
        |= P.getOffset
        |= P.getSource
        |> P.andThen
            (\next ->
                case String.uncons next of
                    Just ( c, _ ) ->
                        if isLetter c then
                            P.problem "not a book name"

                        else
                            P.succeed ()

                    Nothing ->
                        P.succeed ()
            )


isLetter : Char -> Bool
isLetter c =
    Char.isAlpha c || Char.toLower c /= Char.toUpper c


{-| Every alias with its book. The aliases must be lowercase, with single
spaces.
-}
aliases : List ( String, Book )
aliases =
    List.concatMap (\( book, tokens ) -> List.map (\t -> ( t, book )) tokens) bookTokens


bookTokens : List ( Book, List String )
bookTokens =
    [ ( Genesis, [ "genesis", "gen" ] )
    , ( Exodus, [ "exodus", "exod", "exo" ] )
    , ( Leviticus, [ "leviticus", "lev" ] )
    , ( Numbers, [ "numbers", "num" ] )
    , ( Deuteronomy, [ "deuteronomy", "deut" ] )
    , ( Joshua, [ "joshua", "josh", "jos", "jsh" ] )
    , ( Judges, [ "judges", "judg", "jdgs", "jdg" ] )
    , ( Ruth, [ "ruth", "rth" ] )
    , ( Samuel_1, numbered 1 [ "samuel", "sam" ] )
    , ( Samuel_2, numbered 2 [ "samuel", "sam" ] )
    , ( Kings_1, numbered 1 [ "kings", "kgs" ] )
    , ( Kings_2, numbered 2 [ "kings", "kgs" ] )
    , ( Chronicles_1, numbered 1 [ "chronicles", "chron", "chr" ] )
    , ( Chronicles_2, numbered 2 [ "chronicles", "chron", "chr" ] )
    , ( Ezra, [ "ezra", "ezr" ] )
    , ( Nehemiah, [ "nehemiah", "neh" ] )
    , ( Esther, [ "esther", "esth", "est" ] )
    , ( Job, [ "job" ] )
    , ( Psalms, [ "psalms", "psalm", "pslm", "psm", "pss", "psa", "ps" ] )
    , ( Proverbs, [ "proverbs", "prov", "prv", "pro" ] )
    , ( Ecclesiastes, [ "ecclesiastes", "eccles", "eccl", "ecc", "qoheleth", "qoh" ] )
    , ( Song_of_Solomon, [ "song of solomon", "song of songs", "song of sol", "song", "sos", "canticles", "cant" ] )
    , ( Isaiah, [ "isaiah", "isa" ] )
    , ( Jeremiah, [ "jeremiah", "jer" ] )
    , ( Lamentations, [ "lamentations", "lam" ] )
    , ( Ezekiel, [ "ezekiel", "ezek", "eze", "ezk" ] )
    , ( Daniel, [ "daniel", "dan" ] )
    , ( Hosea, [ "hosea", "hos" ] )
    , ( Joel, [ "joel" ] )
    , ( Amos, [ "amos" ] )
    , ( Obadiah, [ "obadiah", "obad" ] )
    , ( Jonah, [ "jonah", "jnh", "jon" ] )
    , ( Micah, [ "micah", "mic" ] )
    , ( Nahum, [ "nahum", "nah" ] )
    , ( Habakkuk, [ "habakkuk", "hab" ] )
    , ( Zephaniah, [ "zephaniah", "zeph", "zep" ] )
    , ( Haggai, [ "haggai", "hag" ] )
    , ( Zechariah, [ "zechariah", "zech", "zec" ] )
    , ( Malachi, [ "malachi", "mal" ] )
    , ( Matthew, [ "matthew", "matt", "mat" ] )
    , ( Mark, [ "mark", "mrk" ] )
    , ( Luke, [ "luke", "luk" ] )
    , ( John, [ "john", "jhn", "joh" ] )
    , ( Acts, [ "acts", "act" ] )
    , ( Romans, [ "romans", "rom" ] )
    , ( Corinthians_1, numbered 1 [ "corinthians", "cor" ] )
    , ( Corinthians_2, numbered 2 [ "corinthians", "cor" ] )
    , ( Galatians, [ "galatians", "gal" ] )
    , ( Ephesians, [ "ephesians", "ephes", "eph" ] )
    , ( Philippians, [ "philippians", "phil", "php" ] )
    , ( Colossians, [ "colossians", "col" ] )
    , ( Thessalonians_1, numbered 1 [ "thessalonians", "thess", "thes" ] )
    , ( Thessalonians_2, numbered 2 [ "thessalonians", "thess", "thes" ] )
    , ( Timothy_1, numbered 1 [ "timothy", "tim" ] )
    , ( Timothy_2, numbered 2 [ "timothy", "tim" ] )
    , ( Titus, [ "titus", "tit" ] )
    , ( Philemon, [ "philemon", "philem", "phlm", "phm" ] )
    , ( Hebrews, [ "hebrews", "heb" ] )
    , ( James, [ "james", "jas" ] )
    , ( Peter_1, numbered 1 [ "peter", "pet" ] )
    , ( Peter_2, numbered 2 [ "peter", "pet" ] )
    , ( John_1, numbered 1 [ "john", "jhn", "joh" ] )
    , ( John_2, numbered 2 [ "john", "jhn", "joh" ] )
    , ( John_3, numbered 3 [ "john", "jhn", "joh" ] )
    , ( Jude, [ "jude", "jud" ] )
    , ( Revelation, [ "revelation", "revelations", "rev" ] )
    ]


{-| `numbered 1 [ "sam" ]` is `1 sam`, `1sam`, `i sam`, `1st sam` and
`first sam`.
-}
numbered : Int -> List String -> List String
numbered n names =
    let
        ( roman, ordinal, word ) =
            case n of
                1 ->
                    ( "i", "1st", "first" )

                2 ->
                    ( "ii", "2nd", "second" )

                _ ->
                    ( "iii", "3rd", "third" )

        digit =
            String.fromInt n
    in
    List.concatMap
        (\name ->
            [ digit ++ " " ++ name
            , digit ++ name
            , roman ++ " " ++ name
            , ordinal ++ " " ++ name
            , word ++ " " ++ name
            ]
        )
        names



-- Validate


validateRef : Reference -> Result String Reference
validateRef ref =
    validateBookOrder ref
        |> Result.andThen validateChapterBounds
        |> Result.andThen validateChapterOrder
        |> Result.andThen validateVerseBounds
        |> Result.andThen validateVerseOrder


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
    if startChapter < 1 || endChapter < 1 then
        Err "Chapter numbers start at 1"

    else if numChapters startBook < startChapter then
        Err <| Book.toString startBook ++ " only has " ++ count (numChapters startBook) "chapter"

    else if numChapters endBook < endChapter then
        Err <| Book.toString endBook ++ " only has " ++ count (numChapters endBook) "chapter"

    else
        Ok ref


validateVerseBounds : Reference -> Result String Reference
validateVerseBounds ({ startBook, startChapter, startVerse, endBook, endChapter, endVerse } as ref) =
    if startVerse < 1 || endVerse < 1 then
        Err "Verse numbers start at 1"

    else if numVerses startBook startChapter < startVerse then
        Err <| validateVerseBoundsMsg startBook startChapter

    else if numVerses endBook endChapter < endVerse then
        Err <| validateVerseBoundsMsg endBook endChapter

    else
        Ok ref


validateVerseBoundsMsg : Book -> Int -> String
validateVerseBoundsMsg book chapter =
    if numChapters book == 1 then
        Book.toString book ++ " only has " ++ count (numVerses book chapter) "verse"

    else
        Book.toString book ++ " " ++ String.fromInt chapter ++ " only has " ++ count (numVerses book chapter) "verse"


count : Int -> String -> String
count n noun =
    if n == 1 then
        "1 " ++ noun

    else
        String.fromInt n ++ " " ++ noun ++ "s"
