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

This is a top-level value, so the sorting and the parsers are built once,
when the program starts, not on every parse.

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

`bookTokens` is written out as plain strings, with no helper generating
them, so that other tools can read the table from the source and a diff
shows every spelling that changes.

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
    , ( Samuel_1, [ "1 samuel", "1samuel", "i samuel", "1st samuel", "first samuel", "1 sam", "1sam", "i sam", "1st sam", "first sam" ] )
    , ( Samuel_2, [ "2 samuel", "2samuel", "ii samuel", "2nd samuel", "second samuel", "2 sam", "2sam", "ii sam", "2nd sam", "second sam" ] )
    , ( Kings_1, [ "1 kings", "1kings", "i kings", "1st kings", "first kings", "1 kgs", "1kgs", "i kgs", "1st kgs", "first kgs" ] )
    , ( Kings_2, [ "2 kings", "2kings", "ii kings", "2nd kings", "second kings", "2 kgs", "2kgs", "ii kgs", "2nd kgs", "second kgs" ] )
    , ( Chronicles_1, [ "1 chronicles", "1chronicles", "i chronicles", "1st chronicles", "first chronicles", "1 chron", "1chron", "i chron", "1st chron", "first chron", "1 chr", "1chr", "i chr", "1st chr", "first chr" ] )
    , ( Chronicles_2, [ "2 chronicles", "2chronicles", "ii chronicles", "2nd chronicles", "second chronicles", "2 chron", "2chron", "ii chron", "2nd chron", "second chron", "2 chr", "2chr", "ii chr", "2nd chr", "second chr" ] )
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
    , ( Corinthians_1, [ "1 corinthians", "1corinthians", "i corinthians", "1st corinthians", "first corinthians", "1 cor", "1cor", "i cor", "1st cor", "first cor" ] )
    , ( Corinthians_2, [ "2 corinthians", "2corinthians", "ii corinthians", "2nd corinthians", "second corinthians", "2 cor", "2cor", "ii cor", "2nd cor", "second cor" ] )
    , ( Galatians, [ "galatians", "gal" ] )
    , ( Ephesians, [ "ephesians", "ephes", "eph" ] )
    , ( Philippians, [ "philippians", "phil", "php" ] )
    , ( Colossians, [ "colossians", "col" ] )
    , ( Thessalonians_1, [ "1 thessalonians", "1thessalonians", "i thessalonians", "1st thessalonians", "first thessalonians", "1 thess", "1thess", "i thess", "1st thess", "first thess", "1 thes", "1thes", "i thes", "1st thes", "first thes" ] )
    , ( Thessalonians_2, [ "2 thessalonians", "2thessalonians", "ii thessalonians", "2nd thessalonians", "second thessalonians", "2 thess", "2thess", "ii thess", "2nd thess", "second thess", "2 thes", "2thes", "ii thes", "2nd thes", "second thes" ] )
    , ( Timothy_1, [ "1 timothy", "1timothy", "i timothy", "1st timothy", "first timothy", "1 tim", "1tim", "i tim", "1st tim", "first tim" ] )
    , ( Timothy_2, [ "2 timothy", "2timothy", "ii timothy", "2nd timothy", "second timothy", "2 tim", "2tim", "ii tim", "2nd tim", "second tim" ] )
    , ( Titus, [ "titus", "tit" ] )
    , ( Philemon, [ "philemon", "philem", "phlm", "phm" ] )
    , ( Hebrews, [ "hebrews", "heb" ] )
    , ( James, [ "james", "jas" ] )
    , ( Peter_1, [ "1 peter", "1peter", "i peter", "1st peter", "first peter", "1 pet", "1pet", "i pet", "1st pet", "first pet" ] )
    , ( Peter_2, [ "2 peter", "2peter", "ii peter", "2nd peter", "second peter", "2 pet", "2pet", "ii pet", "2nd pet", "second pet" ] )
    , ( John_1, [ "1 john", "1john", "i john", "1st john", "first john", "1 jhn", "1jhn", "i jhn", "1st jhn", "first jhn", "1 joh", "1joh", "i joh", "1st joh", "first joh" ] )
    , ( John_2, [ "2 john", "2john", "ii john", "2nd john", "second john", "2 jhn", "2jhn", "ii jhn", "2nd jhn", "second jhn", "2 joh", "2joh", "ii joh", "2nd joh", "second joh" ] )
    , ( John_3, [ "3 john", "3john", "iii john", "3rd john", "third john", "3 jhn", "3jhn", "iii jhn", "3rd jhn", "third jhn", "3 joh", "3joh", "iii joh", "3rd joh", "third joh" ] )
    , ( Jude, [ "jude", "jud" ] )
    , ( Revelation, [ "revelation", "revelations", "rev" ] )
    ]



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
        Err <| chapterBoundsMsg startBook

    else if numChapters endBook < endChapter then
        Err <| chapterBoundsMsg endBook

    else
        Ok ref


chapterBoundsMsg : Book -> String
chapterBoundsMsg book =
    if book == Psalms then
        "There are only " ++ String.fromInt (numChapters book) ++ " Psalms"

    else
        Book.toString book ++ " only has " ++ count (numChapters book) "chapter"


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
