module Internal.Reference exposing (Reference)

import Internal.Book as Book exposing (Book(..), numChapters, numVerses)


type alias Reference =
    { startBook : Book
    , startChapter : Int
    , startVerse : Int
    , endBook : Book
    , endChapter : Int
    , endVerse : Int
    }
