//! Property-based tests. `compat/corpus.py diff` checks exact answers against
//! the Elm; these check general laws over random inputs.

use elm_bible_rs::{Book, Error, Reference, format, parse};
use proptest::prelude::*;

fn encode(book: Book, chapter: i64, verse: i64) -> i64 {
    book.number() as i64 * 1_000_000 + chapter * 1_000 + verse
}

/// A real verse: chapter and verse both at least 1 and within the book.
fn verse() -> impl Strategy<Value = i64> {
    (0..66usize).prop_flat_map(|i| {
        let book = Book::all()[i];
        (1..=book.chapters() as i64).prop_flat_map(move |ch| {
            (1..=book.verses(ch) as i64).prop_map(move |v| encode(book, ch, v))
        })
    })
}

/// A passage between two real verses, in canonical order.
fn passage() -> impl Strategy<Value = Reference> {
    (verse(), verse()).prop_map(|(a, b)| {
        let (start, end) = if a <= b { (a, b) } else { (b, a) };
        Reference::decode(start, end).expect("any ordered pair of real verses is valid")
    })
}

/// Text that looks a bit like a reference, so random inputs reach the
/// interesting paths rather than failing on the first character.
fn reference_like() -> impl Strategy<Value = String> {
    let piece = prop_oneof![
        prop::sample::select(Book::all().to_vec()).prop_map(|b| b
            .aliases()
            .next()
            .unwrap()
            .to_string()),
        prop::sample::select(Book::all().to_vec()).prop_map(|b| b.name().to_string()),
        (0u32..200).prop_map(|n| n.to_string()),
        prop::sample::select(vec![
            " ", " ", ":", "-", " - ", "\u{2013}", "\u{2014}", "\t", ".", "e", "0", ";", ",", "x"
        ])
        .prop_map(str::to_string),
    ];
    prop::collection::vec(piece, 0..8).prop_map(|v| v.concat())
}

proptest! {
    #![proptest_config(ProptestConfig::with_cases(2_000))]

    // ----- Laws for real passages -----

    #[test]
    fn formatted_passages_parse_back_to_themselves(r in passage()) {
        prop_assert_eq!(parse(&r.to_string()), Ok(r));
    }

    #[test]
    fn encoding_round_trips(r in passage()) {
        let (start, end) = r.encode();
        prop_assert!(start <= end);
        prop_assert_eq!(Reference::decode(start, end), Ok(r));
        prop_assert_eq!(format(start, end), Ok(r.to_string()));
    }

    #[test]
    fn parsing_ignores_ascii_case(r in passage(), upper in any::<bool>()) {
        let text = r.to_string();
        let cased = if upper { text.to_ascii_uppercase() } else { text.to_ascii_lowercase() };
        prop_assert_eq!(parse(&cased), Ok(r));
    }

    #[test]
    fn spacing_around_punctuation_does_not_matter(r in passage(), pad in "[ ]{0,3}") {
        let text = r.to_string().replace(" - ", "-").replace('-', &format!("{pad}-{pad}")).replace(':', &format!("{pad}:{pad}"));
        prop_assert_eq!(parse(&text), Ok(r));
    }

    #[test]
    fn every_alias_names_its_book(i in 0..66usize, pick in any::<prop::sample::Index>()) {
        let book = Book::all()[i];
        let aliases: Vec<_> = book.aliases().collect();
        let alias = pick.get(&aliases);
        let r = parse(&format!("{alias} 1")).unwrap();
        prop_assert_eq!(r.start_book(), book);
        prop_assert_eq!(r.end_book(), book);
    }

    // ----- Laws for any input -----

    #[test]
    fn parse_never_panics_and_results_are_consistent(text in any::<String>()) {
        check_any_parse(&text)?;
    }

    #[test]
    fn parse_reference_like_text_is_consistent(text in reference_like()) {
        check_any_parse(&text)?;
    }

    #[test]
    fn decode_never_panics(start in any::<i64>(), end in any::<i64>()) {
        if let Ok(r) = Reference::decode(start, end) {
            // Whatever decodes formats, and re-encodes inside the canon.
            let _ = r.to_string();
            prop_assert!(r.start_book() <= r.end_book());
        }
    }

    #[test]
    fn decode_inverts_encode_for_in_range_values(start in 1_000_000i64..67_000_000, end in 1_000_000i64..67_000_000) {
        if let Ok(r) = Reference::decode(start, end) {
            prop_assert_eq!(r.encode(), (start, end));
        }
    }

    // ----- Input normalisation -----

    /// En and em dashes are hyphens.
    #[test]
    fn all_dashes_are_equal(text in reference_like(), dash in prop::sample::select(vec!["\u{2013}", "\u{2014}"])) {
        prop_assert_eq!(parse(&text.replace('-', dash)), parse(&text));
    }

    /// Parsing stops at the first character that can't start a book,
    /// number, colon or dash, and ignores the rest. `;` is one.
    #[test]
    fn text_after_a_semicolon_is_ignored(text in reference_like(), tail in any::<String>()) {
        prop_assert_eq!(parse(&format!("{text};{tail}")), parse(&text));
    }

    /// Case and whitespace are normalised away: any run of whitespace is a
    /// single space.
    #[test]
    fn any_whitespace_is_a_space(text in reference_like(), ws in prop::sample::select(vec!["\t", "\u{a0}", "  ", "\n ", "\u{3000}", "\u{feff}"])) {
        prop_assert_eq!(parse(&text.replace(' ', ws)), parse(&text));
        prop_assert_eq!(parse(&format!("{ws}{text}{ws}")), parse(&text));
    }

    /// Chapter and verse 0 are rejected, whether parsed or decoded.
    #[test]
    fn zero_chapters_and_verses_are_rejected(r in passage()) {
        let (start, end) = r.encode();
        let book = r.start_book().number() as i64 * 1_000_000;
        prop_assert_eq!(Reference::decode(book + start % 1_000, end), Err(Error::ZeroChapter));
        prop_assert_eq!(Reference::decode(start - start % 1_000, end), Err(Error::ZeroVerse));
    }
}

/// What must hold for every parse result: a reference is valid by its own
/// rules and survives decoding; an error has a message.
fn check_any_parse(text: &str) -> Result<(), TestCaseError> {
    match parse(text) {
        Ok(r) => {
            let (start, end) = r.encode();
            prop_assert!(start <= end, "{text:?} gave {r} ({start}, {end})");
            prop_assert_eq!(Reference::decode(start, end), Ok(r));
            prop_assert!(r.start_chapter() >= 1 && r.start_verse() >= 1);
            prop_assert!(r.end_chapter() >= 1 && r.end_verse() >= 1);
            prop_assert!(r.start_chapter() <= r.start_book().chapters() as i32);
            prop_assert!(r.end_verse() <= r.end_book().verses(r.end_chapter() as i64) as i32);
            // The canonical text means the same passage.
            let reparsed = parse(&r.to_string()).unwrap();
            prop_assert_eq!(reparsed, r, "{:?} -> {} -> {}", text, r, reparsed);
        }
        Err(e) => {
            prop_assert!(!e.to_string().is_empty());
            prop_assert!(
                e != Error::InvalidBookNumber,
                "only decoding reports book numbers"
            );
        }
    }
    Ok(())
}
