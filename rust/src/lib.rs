//! Parse, validate, format and encode English Bible references.
//!
//! ```
//! use elm_bible_rs::{Book, Reference};
//!
//! let r: Reference = "Gen 1:1-5".parse().unwrap();
//! assert_eq!(r.to_string(), "Genesis 1:1-5");
//! assert_eq!(r.encode(), (1_001_001, 1_001_005));
//! assert_eq!(r.start_book(), Book::Genesis);
//!
//! let r = Reference::decode(19_023_001, 19_023_006).unwrap();
//! assert_eq!(r.to_string(), "Psalm 23:1-6");
//! ```
//!
//! # Compatibility
//!
//! This is a port of the Elm package `monty5811/elm-bible`, which lives in the
//! same repository, and elm-bible-rs X.Y.Z matches elm-bible X.Y.Z. Every
//! input gives the same result and the same error message as that Elm code.
//! `COMPAT.md` lists the few places this crate can't match. Change behaviour
//! in the Elm and here together.
//!
//! # Encoding
//!
//! A verse is encoded as `book * 1_000_000 + chapter * 1_000 + verse`, so
//! Genesis 1:1 is `1_001_001` and Revelation 22:21 is `66_022_021`. A
//! reference is a `(start, end)` pair of these, which sorts and range-checks
//! as plain integers.

use std::fmt;
use std::str::FromStr;

#[rustfmt::skip]
mod tables;
pub use tables::Book;
use tables::{ALIASES, ALL, BOOKS, TOKENS};

#[cfg(feature = "python")]
mod python;

impl Book {
    /// All 66 books, in canonical order.
    pub fn all() -> &'static [Book] {
        &ALL
    }

    /// The book with this number (1 = Genesis, 66 = Revelation).
    pub fn from_number(number: i64) -> Option<Book> {
        if (1..=ALL.len() as i64).contains(&number) {
            Some(ALL[number as usize - 1])
        } else {
            None
        }
    }

    /// 1 for Genesis to 66 for Revelation.
    pub fn number(self) -> u8 {
        self as u8
    }

    /// The name used in a formatted reference: "Genesis", "1 Samuel", and
    /// "Psalm" (singular, as in "Psalm 23"). Elm: `Book.toString`.
    pub fn name(self) -> &'static str {
        BOOKS[self as usize - 1].name
    }

    /// The book's own name, for headings and lists: the same as
    /// [`name`](Book::name) except "Psalms". Elm: `Book.toStringBookName`.
    pub fn title(self) -> &'static str {
        match self {
            Book::Psalms => "Psalms",
            _ => self.name(),
        }
    }

    pub fn chapters(self) -> u16 {
        self.verse_counts().len() as u16
    }

    /// Verses in each chapter; index 0 is chapter 1.
    pub fn verse_counts(self) -> &'static [u16] {
        BOOKS[self as usize - 1].verses
    }

    /// Verses in `chapter`, or 0 when the book has no such chapter (as Elm's
    /// `numVerses` does).
    pub fn verses(self, chapter: i64) -> u16 {
        usize::try_from(chapter)
            .ok()
            .and_then(|c| c.checked_sub(1))
            .and_then(|i| self.verse_counts().get(i))
            .copied()
            .unwrap_or(0)
    }

    pub fn is_old_testament(self) -> bool {
        self <= Book::Malachi
    }

    /// The lowercase spellings the parser accepts for this book, e.g.
    /// "genesis" and "gen", in the Elm table's order (Elm:
    /// `Bible.Book.aliases`). An alias only matches when no letter follows
    /// it, so "psalms" is never "psalm" plus "s".
    pub fn aliases(self) -> impl Iterator<Item = &'static str> {
        ALIASES
            .iter()
            .filter(move |(_, b)| *b == self)
            .map(|(t, _)| *t)
    }
}

/// Why a string or encoded pair is not a valid reference. `Display` gives
/// the exact message the Elm code produces.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Error {
    /// A number followed by `.` and a digit, as in "John 3.16".
    Syntax,
    /// Nothing recognisable at the start of the text.
    Empty,
    /// Books, numbers, colons and dashes in an order that isn't a reference.
    Unrecognised,
    /// An encoded verse whose book number is not 1 to 66.
    InvalidBookNumber,
    BooksOutOfOrder,
    ChaptersOutOfOrder,
    VersesOutOfOrder,
    /// A chapter number below 1.
    ZeroChapter,
    /// A verse number below 1.
    ZeroVerse,
    ChapterOutOfRange {
        book: Book,
    },
    VerseOutOfRange {
        book: Book,
        chapter: i32,
    },
}

impl Error {
    /// A stable, machine-friendly name for the error kind.
    pub fn code(&self) -> &'static str {
        match self {
            Error::Syntax => "syntax",
            Error::Empty => "empty",
            Error::Unrecognised => "unrecognised",
            Error::InvalidBookNumber => "invalid_book_number",
            Error::BooksOutOfOrder => "books_out_of_order",
            Error::ChaptersOutOfOrder => "chapters_out_of_order",
            Error::VersesOutOfOrder => "verses_out_of_order",
            Error::ZeroChapter => "zero_chapter",
            Error::ZeroVerse => "zero_verse",
            Error::ChapterOutOfRange { .. } => "chapter_out_of_range",
            Error::VerseOutOfRange { .. } => "verse_out_of_range",
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::Syntax => f.write_str("Use a colon between chapter and verse, as in John 3:16"),
            Error::Empty => f.write_str("No reference found"),
            Error::Unrecognised => f.write_str("No valid reference found"),
            Error::InvalidBookNumber => f.write_str("Invalid book number"),
            Error::BooksOutOfOrder => f.write_str("End book must come after start book"),
            Error::ChaptersOutOfOrder => f.write_str("End chapter must come after start chapter"),
            Error::VersesOutOfOrder => f.write_str("End verse must come after start verse"),
            Error::ZeroChapter => f.write_str("Chapter numbers start at 1"),
            Error::ZeroVerse => f.write_str("Verse numbers start at 1"),
            Error::ChapterOutOfRange { book: Book::Psalms } => {
                write!(f, "There are only {} Psalms", Book::Psalms.chapters())
            }
            Error::ChapterOutOfRange { book } => {
                write!(
                    f,
                    "{} only has {}",
                    book.name(),
                    count(book.chapters(), "chapter")
                )
            }
            Error::VerseOutOfRange { book, chapter } => {
                let verses = count(book.verses(*chapter as i64), "verse");
                if book.chapters() == 1 {
                    write!(f, "{} only has {}", book.name(), verses)
                } else {
                    write!(f, "{} {} only has {}", book.name(), chapter, verses)
                }
            }
        }
    }
}

/// Elm's `count`: "1 verse", "2 verses".
fn count(n: u16, noun: &str) -> String {
    if n == 1 {
        format!("1 {noun}")
    } else {
        format!("{n} {noun}s")
    }
}

impl std::error::Error for Error {}

/// A validated passage from a start verse to an end verse, inclusive.
///
/// Validation is the Elm package's: books, then chapters, then verses must
/// not run backwards, and every chapter and verse must be between 1 and the
/// end of its book or chapter.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Reference {
    start_book: Book,
    start_chapter: i32,
    start_verse: i32,
    end_book: Book,
    end_chapter: i32,
    end_verse: i32,
}

impl Reference {
    /// Parse text such as "Genesis 1", "Gen 1:1-20", "1 Cor 13" or
    /// "Genesis 1 - Exodus 5:20". Case and whitespace are ignored, and so is
    /// anything after the reference. See `COMPAT.md` for what else is (and
    /// isn't) accepted.
    pub fn parse(text: &str) -> Result<Reference, Error> {
        let statements = lex(&normalize(text))?;
        let raw = from_statements(&statements)?;
        raw.validate()
    }

    /// Build a reference from an encoded `(start, end)` pair. Fails if either
    /// value is outside 1_000_000..=66_999_999 or the reference isn't valid.
    pub fn decode(start: i64, end: i64) -> Result<Reference, Error> {
        match (decode_verse(start), decode_verse(end)) {
            (Some((sb, sc, sv)), Some((eb, ec, ev))) => RawRef {
                start_book: sb,
                start_chapter: sc as i64,
                start_verse: sv as i64,
                end_book: eb,
                end_chapter: ec as i64,
                end_verse: ev as i64,
            }
            .validate(),
            _ => Err(Error::InvalidBookNumber),
        }
    }

    /// The `(start, end)` encoding of this reference.
    pub fn encode(&self) -> (i64, i64) {
        (self.start(), self.end())
    }

    pub fn start(&self) -> i64 {
        encode_verse(self.start_book, self.start_chapter, self.start_verse)
    }

    pub fn end(&self) -> i64 {
        encode_verse(self.end_book, self.end_chapter, self.end_verse)
    }

    pub fn start_book(&self) -> Book {
        self.start_book
    }
    pub fn start_chapter(&self) -> i32 {
        self.start_chapter
    }
    pub fn start_verse(&self) -> i32 {
        self.start_verse
    }
    pub fn end_book(&self) -> Book {
        self.end_book
    }
    pub fn end_chapter(&self) -> i32 {
        self.end_chapter
    }
    pub fn end_verse(&self) -> i32 {
        self.end_verse
    }
}

impl FromStr for Reference {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Reference::parse(s)
    }
}

/// The canonical text of a reference: "Genesis 1:1", "Genesis 1:1-11",
/// "Genesis 1:1-12:2", "Jude 3-5", "Genesis 1:1 - Exodus 2:4". Single-chapter
/// books never show a chapter number.
impl fmt::Display for Reference {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let r = self;
        let (sb, eb) = (r.start_book.name(), r.end_book.name());
        if r.start_book == r.end_book {
            if r.start_book.chapters() == 1 {
                if r.start_verse == r.end_verse {
                    write!(f, "{} {}", sb, r.start_verse)
                } else {
                    write!(f, "{} {}-{}", sb, r.start_verse, r.end_verse)
                }
            } else if r.start_chapter == r.end_chapter {
                if r.start_verse == r.end_verse {
                    write!(f, "{} {}:{}", sb, r.start_chapter, r.start_verse)
                } else {
                    write!(
                        f,
                        "{} {}:{}-{}",
                        sb, r.start_chapter, r.start_verse, r.end_verse
                    )
                }
            } else {
                write!(
                    f,
                    "{} {}:{}-{}:{}",
                    sb, r.start_chapter, r.start_verse, r.end_chapter, r.end_verse
                )
            }
        } else {
            match (r.start_book.chapters() == 1, r.end_book.chapters() == 1) {
                (true, true) => write!(f, "{} {} - {} {}", sb, r.start_verse, eb, r.end_verse),
                (true, false) => write!(
                    f,
                    "{} {} - {} {}:{}",
                    sb, r.start_verse, eb, r.end_chapter, r.end_verse
                ),
                (false, true) => write!(
                    f,
                    "{} {}:{} - {} {}",
                    sb, r.start_chapter, r.start_verse, eb, r.end_verse
                ),
                (false, false) => write!(
                    f,
                    "{} {}:{} - {} {}:{}",
                    sb, r.start_chapter, r.start_verse, eb, r.end_chapter, r.end_verse
                ),
            }
        }
    }
}

/// Parse a reference. Shorthand for [`Reference::parse`].
pub fn parse(text: &str) -> Result<Reference, Error> {
    Reference::parse(text)
}

/// Format an encoded `(start, end)` pair. Shorthand for
/// `Reference::decode(start, end)?.to_string()`.
pub fn format(start: i64, end: i64) -> Result<String, Error> {
    Reference::decode(start, end).map(|r| r.to_string())
}

// ---------------------------------------------------------------------------
// Encoding

fn encode_verse(book: Book, chapter: i32, verse: i32) -> i64 {
    book.number() as i64 * 1_000_000 + chapter as i64 * 1_000 + verse as i64
}

/// Elm's `decodeHelpInt`: anything outside Genesis to Revelation is
/// rejected first, then book = num // 1000000, chapter = remainderBy 1000
/// (num // 1000), verse = remainderBy 1000 num.
fn decode_verse(num: i64) -> Option<(Book, i32, i32)> {
    if !(1_000_000..=66_999_999).contains(&num) {
        return None;
    }
    let book = Book::from_number(num / 1_000_000)?;
    let chapter = (num / 1_000 % 1_000) as i32;
    let verse = (num % 1_000) as i32;
    Some((book, chapter, verse))
}

// ---------------------------------------------------------------------------
// Validation

/// A candidate reference before validation. Parsed numbers can be any size
/// (they saturate rather than overflow), so this holds i64s.
struct RawRef {
    start_book: Book,
    start_chapter: i64,
    start_verse: i64,
    end_book: Book,
    end_chapter: i64,
    end_verse: i64,
}

impl RawRef {
    /// Elm's `validateRef`: book order, chapter bounds, chapter order, verse
    /// bounds, verse order, in that order (it decides which message you get).
    fn validate(self) -> Result<Reference, Error> {
        let r = &self;
        if r.start_book > r.end_book {
            return Err(Error::BooksOutOfOrder);
        }
        if r.start_chapter < 1 || r.end_chapter < 1 {
            return Err(Error::ZeroChapter);
        }
        if (r.start_book.chapters() as i64) < r.start_chapter {
            return Err(Error::ChapterOutOfRange { book: r.start_book });
        }
        if (r.end_book.chapters() as i64) < r.end_chapter {
            return Err(Error::ChapterOutOfRange { book: r.end_book });
        }
        let same_book = r.start_book == r.end_book;
        if same_book && r.start_chapter > r.end_chapter {
            return Err(Error::ChaptersOutOfOrder);
        }
        // Chapters are now 1 to 150, so they fit an i32.
        if r.start_verse < 1 || r.end_verse < 1 {
            return Err(Error::ZeroVerse);
        }
        if (r.start_book.verses(r.start_chapter) as i64) < r.start_verse {
            return Err(Error::VerseOutOfRange {
                book: r.start_book,
                chapter: r.start_chapter as i32,
            });
        }
        if (r.end_book.verses(r.end_chapter) as i64) < r.end_verse {
            return Err(Error::VerseOutOfRange {
                book: r.end_book,
                chapter: r.end_chapter as i32,
            });
        }
        if same_book && r.start_chapter == r.end_chapter && r.start_verse > r.end_verse {
            return Err(Error::VersesOutOfOrder);
        }
        // Every value is now between 1 and a chapter or verse count.
        Ok(Reference {
            start_book: self.start_book,
            start_chapter: self.start_chapter as i32,
            start_verse: self.start_verse as i32,
            end_book: self.end_book,
            end_chapter: self.end_chapter as i32,
            end_verse: self.end_verse as i32,
        })
    }
}

// ---------------------------------------------------------------------------
// Parsing
//
// A faithful port of the Elm parser. The input is normalised (lowercased,
// trimmed, whitespace runs collapsed to one space), then elm/parser loops over
//
//     succeed identity |= statement |. chompWhile ((==) ' ')
//
// with `statement = oneOf [book alias then an optional ".", "-" / "—" / "–",
// ":", digits]`. It stops at the first thing that isn't a statement and
// ignores the rest, so trailing text such as `(ESV)` doesn't matter.

#[derive(Clone, Copy, Debug, PartialEq)]
enum Statement {
    BookName(Book),
    Num(i64),
    Dash,
    Colon,
}

const DASHES: [&str; 3] = ["-", "\u{2014}", "\u{2013}"];

/// JavaScript's `\s` (and `String.prototype.trim`), which Elm's
/// `String.words` splits on. Not the same set as `char::is_whitespace`: it
/// has U+FEFF and lacks U+0085.
fn is_js_whitespace(c: char) -> bool {
    matches!(
        c,
        '\t' | '\n' | '\u{0B}' | '\u{0C}' | '\r' | ' ' | '\u{A0}' | '\u{1680}' | '\u{2000}'
            ..='\u{200A}'
                | '\u{2028}'
                | '\u{2029}'
                | '\u{202F}'
                | '\u{205F}'
                | '\u{3000}'
                | '\u{FEFF}'
    )
}

/// Elm's `normalize`: `String.toLower >> String.words >> String.join " "`.
fn normalize(text: &str) -> String {
    let lower = text.to_lowercase();
    lower
        .split(is_js_whitespace)
        .filter(|w| !w.is_empty())
        .collect::<Vec<_>>()
        .join(" ")
}

/// Elm's `isLetter`: `Char.isAlpha c || Char.toLower c /= Char.toUpper c`.
fn is_letter(c: char) -> bool {
    c.is_ascii_alphabetic() || !c.to_lowercase().eq(c.to_uppercase())
}

fn lex(text: &str) -> Result<Vec<Statement>, Error> {
    let bytes = text.as_bytes();
    let mut statements = Vec::new();
    let mut pos = 0;
    // No statement consumes input and then fails, except a number followed
    // by ".digit", which fails the whole parse.
    while let Some((stmt, mut next)) = statement(text, pos)? {
        statements.push(stmt);
        while next < bytes.len() && bytes[next] == b' ' {
            next += 1;
        }
        pos = next;
    }
    Ok(statements)
}

fn statement(text: &str, pos: usize) -> Result<Option<(Statement, usize)>, Error> {
    let rest = &text[pos..];
    if let Some((token, book)) = TOKENS
        .iter()
        .find(|(t, _)| rest.starts_with(t) && !rest[t.len()..].starts_with(is_letter))
    {
        let mut next = pos + token.len();
        if text[next..].starts_with('.') {
            next += 1;
        }
        return Ok(Some((Statement::BookName(*book), next)));
    }
    if let Some(dash) = DASHES.iter().find(|d| rest.starts_with(*d)) {
        return Ok(Some((Statement::Dash, pos + dash.len())));
    }
    if rest.starts_with(':') {
        return Ok(Some((Statement::Colon, pos + 1)));
    }
    number(text.as_bytes(), pos)
}

/// Elm's `number`: a run of ASCII digits, leading zeros allowed. Followed by
/// "." and a digit it is an error, not a number.
fn number(s: &[u8], pos: usize) -> Result<Option<(Statement, usize)>, Error> {
    let mut end = pos;
    let mut value: i64 = 0;
    while end < s.len() && s[end].is_ascii_digit() {
        // Elm accumulates in doubles; saturating gives the same answer to
        // every comparison the validator makes.
        value = value
            .saturating_mul(10)
            .saturating_add((s[end] - b'0') as i64);
        end += 1;
    }
    if end == pos {
        return Ok(None);
    }
    if s.get(end) == Some(&b'.') && s.get(end + 1).is_some_and(u8::is_ascii_digit) {
        return Err(Error::Syntax);
    }
    Ok(Some((Statement::Num(value), end)))
}

/// Elm's `processStatementsHelp`: the twelve reference shapes.
fn from_statements(stmts: &[Statement]) -> Result<RawRef, Error> {
    use Statement::*;
    let one = |b: Book| b.chapters() == 1;
    let last_chapter = |b: Book| b.chapters() as i64;
    let last_verse = |b: Book, ch: i64| b.verses(ch) as i64;
    let r = |sb, sc, sv, eb, ec, ev| RawRef {
        start_book: sb,
        start_chapter: sc,
        start_verse: sv,
        end_book: eb,
        end_chapter: ec,
        end_verse: ev,
    };
    Ok(match *stmts {
        // Gen
        [BookName(b)] => r(b, 1, 1, b, last_chapter(b), last_verse(b, last_chapter(b))),
        // Gen 1 (or Jude 5: a verse)
        [BookName(b), Num(ch)] => {
            if one(b) {
                r(b, 1, ch, b, 1, ch)
            } else {
                r(b, ch, 1, b, ch, last_verse(b, ch))
            }
        }
        // Gen 1:5
        [BookName(b), Num(ch), Colon, Num(vs)] => r(b, ch, vs, b, ch, vs),
        // Gen 1:5-20
        [BookName(b), Num(ch), Colon, Num(vs), Dash, Num(ev)] => r(b, ch, vs, b, ch, ev),
        // Gen 1:5-2:5
        [
            BookName(b),
            Num(ch),
            Colon,
            Num(vs),
            Dash,
            Num(ec),
            Colon,
            Num(ev),
        ] => r(b, ch, vs, b, ec, ev),
        // Gen 1-2 (or Jude 3-5)
        [BookName(b), Num(ch), Dash, Num(ec)] => {
            if one(b) {
                r(b, 1, ch, b, 1, ec)
            } else {
                r(b, ch, 1, b, ec, last_verse(b, ec))
            }
        }
        // Gen 1 - Exodus 5
        [BookName(sb), Num(ch), Dash, BookName(eb), Num(ec)] => match (one(sb), one(eb)) {
            (true, true) => r(sb, 1, ch, eb, 1, ec),
            (false, true) => r(sb, ch, 1, eb, 1, ec),
            (true, false) => r(sb, 1, ch, eb, ec, last_verse(eb, ec)),
            (false, false) => r(sb, ch, 1, eb, ec, last_verse(eb, ec)),
        },
        // Gen 1:1 - Exodus 5
        [
            BookName(sb),
            Num(ch),
            Colon,
            Num(vs),
            Dash,
            BookName(eb),
            Num(ec),
        ] => {
            if one(eb) {
                r(sb, ch, vs, eb, 1, ec)
            } else {
                r(sb, ch, vs, eb, ec, last_verse(eb, ec))
            }
        }
        // Gen 1:1 - Exodus 5:5
        [
            BookName(sb),
            Num(ch),
            Colon,
            Num(vs),
            Dash,
            BookName(eb),
            Num(ec),
            Colon,
            Num(ev),
        ] => r(sb, ch, vs, eb, ec, ev),
        // Gen 1 - Exodus 2:5
        [
            BookName(sb),
            Num(ch),
            Dash,
            BookName(eb),
            Num(ec),
            Colon,
            Num(ev),
        ] => {
            if one(sb) {
                r(sb, 1, ch, eb, ec, ev)
            } else {
                r(sb, ch, 1, eb, ec, ev)
            }
        }
        // Genesis - Revelation 5
        [BookName(sb), Dash, BookName(eb), Num(ec)] => {
            if one(eb) {
                r(sb, 1, 1, eb, 1, ec)
            } else {
                r(sb, 1, 1, eb, ec, last_verse(eb, ec))
            }
        }
        // Genesis - Revelation
        [BookName(sb), Dash, BookName(eb)] => r(
            sb,
            1,
            1,
            eb,
            last_chapter(eb),
            last_verse(eb, last_chapter(eb)),
        ),
        [] => return Err(Error::Empty),
        _ => return Err(Error::Unrecognised),
    })
}

#[cfg(test)]
mod tests;
