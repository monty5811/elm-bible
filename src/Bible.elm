module Bible
    exposing
        ( Book(..)
        , Reference(Reference)
        , format
        , fromString
        , numChapters
        , numVerses
        , reference
        )

{-| A library for working with Bible references.

@docs format, fromString


# Helpers

@docs reference, numChapters, numVerses


# Types

@docs Reference, Book

-}

import Array
import Regex


{-| Describes a Bible Reference

    `Reference book startChapter startVerse endChapter endVerse`

-}
type Reference
    = Reference Book Int Int Int Int


{-| Nicely formatted `Reference`.

    format (Reference Genesis 1 1 1 1 ) --> "Genesis 1:1"

    format (Reference Genesis 1 1 11 5 ) --> "Genesis 1:1-11:5"

-}
format : Reference -> String
format ref =
    formatHelp ref


{-| Parse any references in a string.

    fromString "Gen 1:1" --> [ Ok <| Reference Genesis 1 1 1 1]

    fromString "Gen 1:1, Exodus 1, Luke 10:16, Acts 28:100" -->
        [Ok <| Reference Genesis 1 1 1 1
        ,Ok <| Reference Exodus 1 1 1 22
        ,Ok <| Reference  Luke 10 16 10 16
        ,Err "Invalid end verse"
        ]

-}
fromString : String -> List (Result String Reference)
fromString str =
    fromStringHelp str


{-| Generate a `Reference` and check it is valid

    reference Genesis 1 1 1 1 --> Ok <| Reference Genesis 1 1 1 1

    reference Genesis 1 1 51 1 --> Err "End chapter invalid"

-}
reference : Book -> Int -> Int -> Int -> Int -> Result String Reference
reference book sc sv ec ev =
    processRef <| RawRef (Just book) sc (Just sv) (Just ec) (Just ev)


{-| Check how many chapters are in a `Book`

    numChapters Genesis --> 50

    numChapters Jude --> 1

-}
numChapters : Book -> Int
numChapters book =
    numVersesHelp book |> List.length


{-| Check how many verses are in a chapter of a `Book`

    numVerses Genesis 50 --> 26

    numVerses Psalms 119 --> 176

-}
numVerses : Book -> Int -> Int
numVerses book chap =
    numVersesHelp book
        |> Array.fromList
        |> Array.get (chap - 1)
        |> Maybe.withDefault 0


formatHelp : Reference -> String
formatHelp (Reference book startChapter startVerse endChapter endVerse) =
    let
        base =
            bookToString book ++ " "
    in
    if numChapters book == 1 then
        renderSingleChapterBook book startVerse endVerse
    else if startChapter == endChapter then
        if startVerse == endVerse then
            base ++ toString startChapter ++ ":" ++ toString startVerse
        else
            base ++ toString startChapter ++ ":" ++ toString startVerse ++ "-" ++ toString endVerse
    else
        base ++ toString startChapter ++ ":" ++ toString startVerse ++ "-" ++ toString endChapter ++ ":" ++ toString endVerse


renderSingleChapterBook : Book -> Int -> Int -> String
renderSingleChapterBook book sv ev =
    if sv == ev then
        bookToString book ++ " " ++ toString sv
    else
        bookToString book ++ " " ++ toString sv ++ "-" ++ toString ev


matchToRef : Regex.Match -> Result String Reference
matchToRef { submatches } =
    submatchesToRef submatches


submatchesToRef : List (Maybe String) -> Result String Reference
submatchesToRef ls =
    case ls of
        [ Just book, startChap, startVerse, endChap, endVerse ] ->
            parseMatches ( book, startChap, startVerse, endChap, endVerse )
                |> processRef

        _ ->
            Err "No valid reference found"


type alias RawRef =
    { book : Maybe Book
    , startChap : Int
    , startVerse : Maybe Int
    , endChap : Maybe Int
    , endVerse : Maybe Int
    }


processRef : RawRef -> Result String Reference
processRef raw =
    case raw.book of
        Nothing ->
            -- no book: invalid
            Err "No Valid Book Name Found"

        Just book ->
            if numChapters book == 1 then
                processRefSingleChapBookHelp raw book
            else
                processRefHelp raw book


processRefSingleChapBookHelp : RawRef -> Book -> Result String Reference
processRefSingleChapBookHelp raw book =
    case raw.startVerse of
        Just sv ->
            processRefHelp raw book

        Nothing ->
            case raw.endChap of
                Just ec ->
                    processRefHelp raw book

                Nothing ->
                    processRefHelp { raw | startChap = 1, startVerse = Just raw.startChap } book


processRefHelp : RawRef -> Book -> Result String Reference
processRefHelp raw book =
    if chapInBounds book raw.startChap then
        case raw.startVerse of
            Just sv ->
                if verseInBounds book raw.startChap sv then
                    case raw.endChap of
                        Nothing ->
                            -- no end chapter, assume same as start chapter and call recursively:
                            processRef { raw | endChap = Just raw.startChap }

                        Just ec ->
                            if chapInBounds book ec then
                                case raw.endVerse of
                                    Nothing ->
                                        -- no end verse
                                        if ec /= raw.startChap then
                                            -- use end of end chapter
                                            Ok <| Reference book raw.startChap sv ec (numVerses book ec)
                                        else
                                            -- use start verse
                                            Ok <| Reference book raw.startChap sv ec sv

                                    Just ev ->
                                        if verseInBounds book ec ev then
                                            -- end verse valid, everthing should be valid by now:
                                            Ok <| Reference book raw.startChap sv ec ev
                                        else
                                            Err "End verse invalid"
                            else
                                Err "End chapter invalid"
                else
                    Err "Start verse invalid"

            Nothing ->
                Ok <| Reference book raw.startChap 1 raw.startChap (numVerses book raw.startChap)
    else
        Err "Start chapter invalid"


parseMatches : ( String, Maybe String, Maybe String, Maybe String, Maybe String ) -> RawRef
parseMatches ( book, startChap, startVerse, endChap, endVerse ) =
    { book = (bookFromString >> Result.toMaybe) book
    , startChap = maybeStringToMaybeInt startChap |> Maybe.withDefault -1
    , startVerse = maybeStringToMaybeInt startVerse
    , endChap = maybeStringToMaybeInt endChap
    , endVerse = maybeStringToMaybeInt endVerse
    }


chapInBounds : Book -> Int -> Bool
chapInBounds book chap =
    chap > 0 && chap <= numChapters book


verseInBounds : Book -> Int -> Int -> Bool
verseInBounds book chap verse =
    verse > 0 && verse <= numVerses book chap


maybeStringToMaybeInt : Maybe String -> Maybe Int
maybeStringToMaybeInt str =
    case str of
        Nothing ->
            Nothing

        Just s ->
            String.toInt s |> Result.toMaybe


{-| Parse a string into a `List Reference`
-}
fromStringHelp : String -> List (Result String Reference)
fromStringHelp str =
    Regex.find Regex.All bibleRe str
        |> List.map matchToRef


{-| -}
type Book
    = Genesis
    | Exodus
    | Leviticus
    | Numbers
    | Deuteronomy
    | Joshua
    | Judges
    | Ruth
    | Samuel_1
    | Samuel_2
    | Kings_1
    | Kings_2
    | Chronicles_2
    | Chronicles_1
    | Ezra
    | Nehemiah
    | Esther
    | Job
    | Psalms
    | Proverbs
    | Ecclesiastes
    | Song_of_Solomon
    | Isaiah
    | Jeremiah
    | Lamentations
    | Ezekiel
    | Daniel
    | Hosea
    | Joel
    | Amos
    | Obadiah
    | Jonah
    | Micah
    | Nahum
    | Habakkuk
    | Zephaniah
    | Haggai
    | Zechariah
    | Malachi
    | Matthew
    | Mark
    | Luke
    | John
    | Acts
    | Romans
    | Corinthians_1
    | Corinthians_2
    | Galatians
    | Ephesians
    | Philippians
    | Colossians
    | Thessalonians_1
    | Thessalonians_2
    | Timothy_1
    | Timothy_2
    | Titus
    | Philemon
    | Hebrews
    | James
    | Peter_1
    | Peter_2
    | John_1
    | John_2
    | John_3
    | Jude
    | Revelation


bookFromString : String -> Result String Book
bookFromString str =
    List.map (matchBook str) bookRegexes
        |> List.filterMap identity
        |> List.head
        |> Result.fromMaybe ("Failed to find a book that matches " ++ str)


matchBook : String -> ( Book, String ) -> Maybe Book
matchBook str2match ( book, reStr ) =
    let
        matches =
            str2match
                |> Regex.find (Regex.AtMost 1) (Regex.caseInsensitive <| Regex.regex reStr)
                |> List.head
    in
    case matches of
        Nothing ->
            Nothing

        Just _ ->
            Just book


bookToString : Book -> String
bookToString book =
    case book of
        Psalms ->
            "Psalm"

        Samuel_1 ->
            "1 Samuel"

        Samuel_2 ->
            "2 Samuel"

        Kings_1 ->
            "1 Kings"

        Kings_2 ->
            "2 Kings"

        Chronicles_1 ->
            "1 Chronicles"

        Chronicles_2 ->
            "2 Chronicles"

        Song_of_Solomon ->
            "Song of Solomon"

        Corinthians_1 ->
            "1 Corinthians"

        Corinthians_2 ->
            "2 Corinthians"

        Thessalonians_1 ->
            "1 Thessalonians"

        Thessalonians_2 ->
            "2 Thessalonians"

        Timothy_1 ->
            "1 Timothy"

        Timothy_2 ->
            "2 Timothy"

        Peter_1 ->
            "1 Peter"

        Peter_2 ->
            "2 Peter"

        John_1 ->
            "1 John"

        John_2 ->
            "2 John"

        John_3 ->
            "3 John"

        _ ->
            toString book


{-|

    Groups:
     * Book Title
     * Chapter Num
     * Verse Num
     * End Chapter Num
     * End Verse Num

-}
bibleRe : Regex.Regex
bibleRe =
    "\\b("
        ++ bookRe
        ++ ")\\s*"
        ++ "(\\d{1,3})(?:\\s*:\\s*(\\d{1,3}))?(?:\\s*[-–—]\\s*(\\d{1,3}(?=\\s*:\\s*))?(?:\\s*:\\s*)?(\\d{1,3})?)?"
        |> Regex.regex
        |> Regex.caseInsensitive


bookRe : String
bookRe =
    bookRegexes
        |> List.map (\( _, re ) -> re)
        |> String.join "|"


numVersesHelp : Book -> List Int
numVersesHelp book =
    List.filter (\( x, _ ) -> x == book) data
        |> List.head
        |> Maybe.map (Tuple.second >> .chapters)
        |> Maybe.withDefault []


bookRegexes : List ( Book, String )
bookRegexes =
    List.map extractRegex data


extractRegex : ( Book, BookData ) -> ( Book, String )
extractRegex ( book, bookData ) =
    ( book, bookData.regex )


type alias BookData =
    { niceName : String
    , shortName : String
    , regex : String
    , chapters : List Int
    }


data : List ( Book, BookData )
data =
    [ ( Genesis, BookData "Genesis" "Gen" "Gen(?:esis)?" [ 31, 25, 24, 26, 32, 22, 24, 22, 29, 32, 32, 20, 18, 24, 21, 16, 27, 33, 38, 18, 34, 24, 20, 67, 34, 35, 46, 22, 35, 43, 55, 32, 20, 31, 29, 43, 36, 30, 23, 23, 57, 38, 34, 34, 28, 34, 31, 22, 33, 26 ] )
    , ( Exodus, BookData "Exodus" "Exod" "Exod(?:us)?" [ 22, 25, 22, 31, 23, 30, 25, 32, 35, 29, 10, 51, 22, 31, 27, 36, 16, 27, 25, 26, 36, 31, 33, 18, 40, 37, 21, 43, 46, 38, 18, 35, 23, 35, 35, 38, 29, 31, 43, 38 ] )
    , ( Leviticus, BookData "Leviticus" "Lev" "Lev(?:iticus)?" [ 17, 16, 17, 35, 19, 30, 38, 36, 24, 20, 47, 8, 59, 57, 33, 34, 16, 30, 37, 27, 24, 33, 44, 23, 55, 46, 34 ] )
    , ( Numbers, BookData "Numbers" "Num" "Num(?:bers)?" [ 54, 34, 51, 49, 31, 27, 89, 26, 23, 36, 35, 16, 33, 45, 41, 50, 13, 32, 22, 29, 35, 41, 30, 25, 18, 65, 23, 31, 40, 16, 54, 42, 56, 29, 34, 13 ] )
    , ( Deuteronomy, BookData "Deuteronomy" "Deut" "Deut(?:eronomy)?" [ 46, 37, 29, 49, 33, 25, 26, 20, 29, 22, 32, 32, 18, 29, 23, 22, 20, 22, 21, 20, 23, 30, 25, 22, 19, 19, 26, 68, 29, 20, 30, 52, 29, 12 ] )
    , ( Joshua, BookData "Joshua" "Josh" "Josh(?:ua)?" [ 18, 24, 17, 24, 15, 27, 26, 35, 27, 43, 23, 24, 33, 15, 63, 10, 18, 28, 51, 9, 45, 34, 16, 33 ] )
    , ( Judges, BookData "Judges" "Judg" "Judg(?:es)?" [ 36, 23, 31, 24, 31, 40, 25, 35, 57, 18, 40, 15, 25, 20, 20, 31, 13, 31, 30, 48, 25 ] )
    , ( Ruth, BookData "Ruth" "Ruth" "Ruth" [ 22, 23, 18, 22 ] )
    , ( Samuel_1, BookData "1 Samuel" "1Sam" "(?:1|I)(?:\\s)?Sam(?:uel)?" [ 28, 36, 21, 22, 12, 21, 17, 22, 27, 27, 15, 25, 23, 52, 35, 23, 58, 30, 24, 42, 15, 23, 29, 22, 44, 25, 12, 25, 11, 31, 13 ] )
    , ( Samuel_2, BookData "2 Samuel" "2Sam" "(?:2|II)(?:\\s)?Sam(?:uel)?" [ 27, 32, 39, 12, 25, 23, 29, 18, 13, 19, 27, 31, 39, 33, 37, 23, 29, 33, 43, 26, 22, 51, 39, 25 ] )
    , ( Kings_1, BookData "1 Kings" "1Kgs" "(?:1|I)(?:\\s)?K(?:in)?gs" [ 53, 46, 28, 34, 18, 38, 51, 66, 28, 29, 43, 33, 34, 31, 34, 34, 24, 46, 21, 43, 29, 53 ] )
    , ( Kings_2, BookData "2 Kings" "2Kgs" "(?:2|II)(?:\\s)?K(?:in)?gs" [ 18, 25, 27, 44, 27, 33, 20, 29, 37, 36, 21, 21, 25, 29, 38, 20, 41, 37, 37, 21, 26, 20, 37, 20, 30 ] )
    , ( Chronicles_1, BookData "1 Chronicles" "1Chr" "(?:1|I)(?:\\s)?Chr(?:o)?(?:nicles)?" [ 54, 55, 24, 43, 26, 81, 40, 40, 44, 14, 47, 40, 14, 17, 29, 43, 27, 17, 19, 8, 30, 19, 32, 31, 31, 32, 34, 21, 30 ] )
    , ( Chronicles_2, BookData "2 Chronicles" "2Chr" "(?:2|II)(?:\\s)?Chr(?:o)?(?:nicles)?" [ 17, 18, 17, 22, 14, 42, 22, 18, 31, 19, 23, 16, 22, 15, 19, 14, 19, 34, 11, 37, 20, 12, 21, 27, 28, 23, 9, 27, 36, 27, 21, 33, 25, 33, 27, 23 ] )
    , ( Ezra, BookData "Ezra" "Ezra" "Ezra" [ 11, 70, 13, 24, 17, 22, 28, 36, 15, 44 ] )
    , ( Nehemiah, BookData "Nehemiah" "Neh" "Neh(?:emiah)?" [ 11, 20, 32, 23, 19, 19, 73, 18, 38, 39, 36, 47, 31 ] )
    , ( Esther, BookData "Esther" "Esth" "Esth(?:er)?" [ 22, 23, 15, 17, 14, 14, 10, 17, 32, 3 ] )
    , ( Job, BookData "Job" "Job" "Job" [ 22, 13, 26, 21, 27, 30, 21, 22, 35, 22, 20, 25, 28, 22, 35, 22, 16, 21, 29, 29, 34, 30, 17, 25, 6, 14, 23, 28, 25, 31, 40, 22, 33, 37, 16, 33, 24, 41, 30, 24, 34, 17 ] )
    , ( Psalms, BookData "Psalms" "Ps" "Ps(?:a)?(?:lm(?:s)?)?" [ 6, 12, 8, 8, 12, 10, 17, 9, 20, 18, 7, 8, 6, 7, 5, 11, 15, 50, 14, 9, 13, 31, 6, 10, 22, 12, 14, 9, 11, 12, 24, 11, 22, 22, 28, 12, 40, 22, 13, 17, 13, 11, 5, 26, 17, 11, 9, 14, 20, 23, 19, 9, 6, 7, 23, 13, 11, 11, 17, 12, 8, 12, 11, 10, 13, 20, 7, 35, 36, 5, 24, 20, 28, 23, 10, 12, 20, 72, 13, 19, 16, 8, 18, 12, 13, 17, 7, 18, 52, 17, 16, 15, 5, 23, 11, 13, 12, 9, 9, 5, 8, 28, 22, 35, 45, 48, 43, 13, 31, 7, 10, 10, 9, 8, 18, 19, 2, 29, 176, 7, 8, 9, 4, 8, 5, 6, 5, 6, 8, 8, 3, 18, 3, 3, 21, 26, 9, 8, 24, 13, 10, 7, 12, 15, 21, 10, 20, 14, 9, 6 ] )
    , ( Proverbs, BookData "Proverbs" "Prov" "Prov(?:erbs)?" [ 33, 22, 35, 27, 23, 35, 27, 36, 18, 32, 31, 28, 25, 35, 33, 33, 28, 24, 29, 30, 31, 29, 35, 34, 28, 28, 27, 28, 27, 33, 31 ] )
    , ( Ecclesiastes, BookData "Ecclesiastes" "Eccl" "Ecc(?:l)?(?:esiastes)?" [ 18, 26, 22, 16, 20, 12, 29, 17, 18, 20, 10, 14 ] )
    , ( Song_of_Solomon, BookData "Song of Solomon" "Song" "Song(?: of Solomon)?" [ 17, 17, 11, 16, 16, 13, 13, 14 ] )
    , ( Isaiah, BookData "Isaiah" "Isa" "Isa(?:iah)?" [ 31, 22, 26, 6, 30, 13, 25, 22, 21, 34, 16, 6, 22, 32, 9, 14, 14, 7, 25, 6, 17, 25, 18, 23, 12, 21, 13, 29, 24, 33, 9, 20, 24, 17, 10, 22, 38, 22, 8, 31, 29, 25, 28, 28, 25, 13, 15, 22, 26, 11, 23, 15, 12, 17, 13, 12, 21, 14, 21, 22, 11, 12, 19, 12, 25, 24 ] )
    , ( Jeremiah, BookData "Jeremiah" "Jer" "Jer(?:emiah)?" [ 19, 37, 25, 31, 31, 30, 34, 22, 26, 25, 23, 17, 27, 22, 21, 21, 27, 23, 15, 18, 14, 30, 40, 10, 38, 24, 22, 17, 32, 24, 40, 44, 26, 22, 19, 32, 21, 28, 18, 16, 18, 22, 13, 30, 5, 28, 7, 47, 39, 46, 64, 34 ] )
    , ( Lamentations, BookData "Lamentations" "Lam" "Lam(?:entations)?" [ 22, 22, 66, 22, 22 ] )
    , ( Ezekiel, BookData "Ezekiel" "Ezek" "Ezek(?:iel)?" [ 28, 10, 27, 17, 17, 14, 27, 18, 11, 22, 25, 28, 23, 23, 8, 63, 24, 32, 14, 49, 32, 31, 49, 27, 17, 21, 36, 26, 21, 26, 18, 32, 33, 31, 15, 38, 28, 23, 29, 49, 26, 20, 27, 31, 25, 24, 23, 35 ] )
    , ( Daniel, BookData "Daniel" "Dan" "Dan(?:iel)?" [ 21, 49, 30, 37, 31, 28, 28, 27, 27, 21, 45, 13 ] )
    , ( Hosea, BookData "Hosea" "Hos" "Hos(?:ea)?" [ 11, 23, 5, 19, 15, 11, 16, 14, 17, 15, 12, 14, 16, 9 ] )
    , ( Joel, BookData "Joel" "Joel" "Joel" [ 20, 32, 21 ] )
    , ( Amos, BookData "Amos" "Amos" "Amos" [ 15, 16, 15, 13, 27, 14, 17, 14, 15 ] )
    , ( Obadiah, BookData "Obadiah" "Obad" "Obad(?:iah)?" [ 21 ] )
    , ( Jonah, BookData "Jonah" "Jonah" "Jonah" [ 17, 10, 10, 11 ] )
    , ( Micah, BookData "Micah" "Mic" "Mic(?:ah)?" [ 16, 13, 12, 13, 15, 16, 20 ] )
    , ( Nahum, BookData "Nahum" "Nah" "Nah(?:um)?" [ 15, 13, 19 ] )
    , ( Habakkuk, BookData "Habakkuk" "Hab" "Hab(?:akkuk)?" [ 17, 20, 19 ] )
    , ( Zephaniah, BookData "Zephaniah" "Zeph" "Zeph(?:aniah)?" [ 18, 15, 20 ] )
    , ( Haggai, BookData "Haggai" "Hag" "Hag(?:gai)?" [ 15, 23 ] )
    , ( Zechariah, BookData "Zechariah" "Zech" "Zech(?:ariah)?" [ 21, 13, 10, 14, 11, 15, 14, 23, 17, 12, 17, 14, 9, 21 ] )
    , ( Malachi, BookData "Malachi" "Mal" "Mal(?:achi)?" [ 14, 17, 18, 6 ] )
    , ( Matthew, BookData "Matthew" "Matt" "Matt(?:hew)?" [ 25, 23, 17, 25, 48, 34, 29, 34, 38, 42, 30, 50, 58, 36, 39, 28, 27, 35, 30, 34, 46, 46, 39, 51, 46, 75, 66, 20 ] )
    , ( Mark, BookData "Mark" "Mark" "Mark" [ 45, 28, 35, 41, 43, 56, 37, 38, 50, 52, 33, 44, 37, 72, 47, 20 ] )
    , ( Luke, BookData "Luke" "Luke" "Luke" [ 80, 52, 38, 44, 39, 49, 50, 56, 62, 42, 54, 59, 35, 35, 32, 31, 37, 43, 48, 47, 38, 71, 56, 53 ] )
    , ( Acts, BookData "Acts" "Acts" "Acts" [ 26, 47, 26, 37, 42, 15, 60, 40, 43, 48, 30, 25, 52, 28, 41, 40, 34, 28, 41, 38, 40, 30, 35, 27, 27, 32, 44, 31 ] )
    , ( Romans, BookData "Romans" "Rom" "Rom(?:ans)?" [ 32, 29, 31, 25, 21, 23, 25, 39, 33, 21, 36, 21, 14, 23, 33, 27 ] )
    , ( Corinthians_1, BookData "1 Corinthians" "1Cor" "(?:1|I)(?:\\s)?Cor(?:inthians)?" [ 31, 16, 23, 21, 13, 20, 40, 13, 27, 33, 34, 31, 13, 40, 58, 24 ] )
    , ( Corinthians_2, BookData "2 Corinthians" "2Cor" "(?:2|II)(?:\\s)?Cor(?:inthians)?" [ 24, 17, 18, 18, 21, 18, 16, 24, 15, 18, 33, 21, 14 ] )
    , ( Galatians, BookData "Galatians" "Gal" "Gal(?:atians)?" [ 24, 21, 29, 31, 26, 18 ] )
    , ( Ephesians, BookData "Ephesians" "Eph" "Eph(?:esians)?" [ 23, 22, 21, 32, 33, 24 ] )
    , ( Colossians, BookData "Colossians" "Col" "Col(?:ossians)?" [ 29, 23, 25, 18 ] )
    , ( Thessalonians_1, BookData "1 Thessalonians" "1Thess" "(?:1|I)(?:\\s)?Thess(?:alonians)?" [ 10, 20, 13, 18, 28 ] )
    , ( Thessalonians_2, BookData "2 Thessalonians" "2Thess" "(?:2|II)(?:\\s)?Thess(?:alonians)?" [ 12, 17, 18 ] )
    , ( Timothy_1, BookData "1 Timothy" "1Tim" "(?:1|I)(?:\\s)?Tim(?:othy)?" [ 20, 15, 16, 16, 25, 21 ] )
    , ( Timothy_2, BookData "2 Timothy" "2Tim" "(?:2|II)(?:\\s)?Tim(?:othy)?" [ 18, 26, 17, 22 ] )
    , ( Titus, BookData "Titus" "Titus" "Tit(?:us)?" [ 16, 15, 15 ] )
    , ( Philemon, BookData "Philemon" "Phlm" "Phlm|Phile(?:mon)?" [ 25 ] )
    , ( Hebrews, BookData "Hebrews" "Heb" "Heb(?:rews)?" [ 14, 18, 19, 16, 14, 20, 28, 13, 28, 39, 40, 29, 25 ] )
    , ( James, BookData "James" "Jas" "Ja(?:me)?s" [ 27, 26, 18, 17, 20 ] )
    , ( Peter_1, BookData "1 Peter" "1Pet" "(?:1|I)(?:\\s)?Pet(?:er)?" [ 25, 25, 22, 19, 14 ] )
    , ( Peter_2, BookData "2 Peter" "2Pet" "(?:2|II)(?:\\s)?Pet(?:er)?" [ 21, 22, 18 ] )
    , ( John_1, BookData "1 John" "1John" "(?:(?:1|I)(?:\\s)?)John" [ 10, 29, 24, 21, 21 ] )
    , ( John_2, BookData "2 John" "2John" "(?:(?:2|II)(?:\\s)?)John" [ 13 ] )
    , ( John_3, BookData "3 John" "3John" "(?:(?:3|III)(?:\\s)?)John" [ 14 ] )
    , ( Jude, BookData "Jude" "Jude" "Jude" [ 25 ] )
    , ( Revelation, BookData "Revelation" "Rev" "Rev(?:elation)?(?:\\sof Jesus Christ)?" [ 20, 29, 22, 11, 14, 17, 17, 13, 21, 11, 19, 17, 18, 20, 8, 21, 18, 24, 21, 15, 27, 21 ] )

    -- move these to the bottom to hack around greedy regex
    , ( John, BookData "John" "John" "(?!(?:1|2|3|I)\\s)(?!(?:1|2|3|I))John" [ 51, 25, 36, 54, 47, 71, 53, 59, 41, 42, 57, 50, 38, 31, 27, 33, 26, 40, 42, 31, 25 ] )
    , ( Philippians, BookData "Philippians" "Phil" "Phil(?:ippians)?" [ 30, 30, 21, 23 ] )
    ]
