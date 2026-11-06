//! Hand-picked cases. The exhaustive comparison with the Elm code is
//! `compat/corpus.py diff`, which runs in CI.

use super::*;

fn p(text: &str) -> Result<(i64, i64), String> {
    Reference::parse(text)
        .map(|r| r.encode())
        .map_err(|e| e.to_string())
}

fn f(start: i64, end: i64) -> Result<String, String> {
    format(start, end).map_err(|e| e.to_string())
}

#[test]
fn documented_shapes() {
    assert_eq!(p("Genesis"), Ok((1_001_001, 1_050_026)));
    assert_eq!(p("Genesis 1"), Ok((1_001_001, 1_001_031)));
    assert_eq!(p("Genesis 1:1"), Ok((1_001_001, 1_001_001)));
    assert_eq!(p("Genesis 1:1-20"), Ok((1_001_001, 1_001_020)));
    assert_eq!(p("Genesis 1:20-2:24"), Ok((1_001_020, 1_002_024)));
    assert_eq!(p("Genesis 1-5"), Ok((1_001_001, 1_005_032)));
    assert_eq!(p("Genesis 1 - Exodus 5"), Ok((1_001_001, 2_005_023)));
    assert_eq!(p("Genesis 1:1 - Exodus 5:20"), Ok((1_001_001, 2_005_020)));
    assert_eq!(p("Genesis 1:1 - Exodus 5"), Ok((1_001_001, 2_005_023)));
    assert_eq!(p("Genesis 1 - Exodus 5:20"), Ok((1_001_001, 2_005_020)));
    assert_eq!(p("Genesis - Exodus 5"), Ok((1_001_001, 2_005_023)));
    assert_eq!(p("Genesis - Revelation"), Ok((1_001_001, 66_022_021)));
}

#[test]
fn single_chapter_books_take_verses() {
    assert_eq!(p("Jude 3"), Ok((65_001_003, 65_001_003)));
    assert_eq!(p("Jude 3-5"), Ok((65_001_003, 65_001_005)));
    assert_eq!(f(65_001_003, 65_001_005).unwrap(), "Jude 3-5");
    assert_eq!(p("Jude 1 - Revelation 2"), Ok((65_001_001, 66_002_029)));
    assert_eq!(p("Obad 3 - Jude 4"), Ok((31_001_003, 65_001_004)));
}

#[test]
fn formats() {
    assert_eq!(f(1_001_001, 1_001_001).unwrap(), "Genesis 1:1");
    assert_eq!(f(1_001_001, 1_001_011).unwrap(), "Genesis 1:1-11");
    assert_eq!(f(1_001_001, 1_012_002).unwrap(), "Genesis 1:1-12:2");
    assert_eq!(f(1_001_001, 2_002_004).unwrap(), "Genesis 1:1 - Exodus 2:4");
    assert_eq!(f(19_023_001, 19_023_006).unwrap(), "Psalm 23:1-6");
    assert_eq!(
        f(65_001_001, 66_002_003).unwrap(),
        "Jude 1 - Revelation 2:3"
    );
    assert_eq!(f(1_001_001, 65_001_003).unwrap(), "Genesis 1:1 - Jude 3");
    assert_eq!(f(57_001_001, 65_001_003).unwrap(), "Philemon 1 - Jude 3");
}

#[test]
fn errors() {
    assert_eq!(p(""), Err("No reference found".into()));
    assert_eq!(p("hello"), Err("No reference found".into()));
    assert_eq!(p("1 2"), Err("No valid reference found".into()));
    assert_eq!(p("Gen 51"), Err("Genesis only has 50 chapters".into()));
    assert_eq!(p("Gen 1:32"), Err("Genesis 1 only has 31 verses".into()));
    assert_eq!(p("Jude 26"), Err("Jude only has 25 verses".into()));
    assert_eq!(
        p("Exod 1 - Gen 2"),
        Err("End book must come after start book".into())
    );
    assert_eq!(
        p("Gen 2-1"),
        Err("End chapter must come after start chapter".into())
    );
    assert_eq!(
        p("Gen 1:5-4"),
        Err("End verse must come after start verse".into())
    );
    assert_eq!(f(0, 1_001_001), Err("Invalid book number".into()));
    assert_eq!(f(67_001_001, 67_001_001), Err("Invalid book number".into()));
}

#[test]
fn new_errors() {
    assert_eq!(p("Gen 0"), Err("Chapter numbers start at 1".into()));
    assert_eq!(p("Gen 1:0"), Err("Verse numbers start at 1".into()));
    assert_eq!(p("Jude 0"), Err("Verse numbers start at 1".into()));
    assert_eq!(p("Ps 151"), Err("There are only 150 Psalms".into()));
    assert_eq!(p("Obad 2"), Ok((31_001_002, 31_001_002)));
    assert_eq!(p("Obad 1:22"), Err("Obadiah only has 21 verses".into()));
    assert_eq!(
        p("John 3.16"),
        Err("Use a colon between chapter and verse, as in John 3:16".into())
    );
    assert_eq!(
        f(1_000_000, 1_000_000),
        Err("Chapter numbers start at 1".into())
    );
    assert_eq!(
        f(1_001_000, 1_001_001),
        Err("Verse numbers start at 1".into())
    );
}

#[test]
fn lenient_input() {
    // Trailing text after the reference is ignored.
    assert_eq!(p("John 3:16; Rom 5:8"), p("John 3:16"));
    assert_eq!(p("John 3:16 (ESV)"), p("John 3:16"));
    assert_eq!(p("John 3:16."), p("John 3:16"));
    assert_eq!(p("Gen 1 esv"), p("Gen 1"));
    assert_eq!(p("Gen 1e5"), p("Gen 1"));
    assert_eq!(p("Gen 0x1"), Err("Chapter numbers start at 1".into()));
    // Whitespace of any kind, and a dot after the book.
    assert_eq!(p("Gen\t1 :\u{a0}5"), p("Gen 1:5"));
    assert_eq!(p("  Ps. 23 "), p("Psalm 23"));
    assert_eq!(p("Psalms 23"), p("Psalm 23"));
    assert_eq!(p("  "), Err("No reference found".into()));
    // An alias can't be followed by a letter, but can by a digit.
    assert_eq!(p("Gens 1"), Err("No reference found".into()));
    assert_eq!(p("Genesis1"), p("Genesis 1"));
    assert_eq!(p("First Cor 13"), p("1 Corinthians 13"));
    // Leading zeros are fine.
    assert_eq!(p("Gen 01:005"), p("Gen 1:5"));
    // Dashes: hyphen, en and em.
    assert_eq!(p("Matt 5\u{2013}7"), p("Matt 5-7"));
    assert_eq!(p("Matt 5\u{2014}7"), p("Matt 5-7"));
    // Huge numbers saturate instead of overflowing.
    assert_eq!(
        p("Gen 99999999999999999999999999"),
        Err("Genesis only has 50 chapters".into())
    );
}

#[test]
fn decode_rejects_out_of_range_values() {
    let wrapped = 4_294_967_296_i64 * 1_000_000 + 1_001_001;
    assert_eq!(f(wrapped, wrapped), Err("Invalid book number".into()));
    assert_eq!(f(-1_001_001, 1_001_001), Err("Invalid book number".into()));
    assert_eq!(f(999_999, 1_001_001), Err("Invalid book number".into()));
    assert_eq!(
        f(66_022_021, 66_999_999),
        Err("Revelation only has 22 chapters".into())
    );
}

#[test]
fn books() {
    assert_eq!(Book::all().len(), 66);
    assert_eq!(Book::from_number(19), Some(Book::Psalms));
    assert_eq!(Book::Psalms.name(), "Psalm");
    assert_eq!(Book::Psalms.title(), "Psalms");
    assert_eq!(Book::Psalms.verses(119), 176);
    assert_eq!(Book::Psalms.verses(0), 0);
    assert_eq!(Book::Psalms.verses(151), 0);
    // Past u32: on a 32-bit target a plain cast would wrap this to chapter 1.
    assert_eq!(Book::Psalms.verses(4_294_967_297), 0);
    assert_eq!(Book::Psalms.verses(i64::MIN), 0);
    assert_eq!(Book::Psalms.verses(i64::MAX), 0);
    assert!(Book::Malachi.is_old_testament());
    assert!(!Book::Matthew.is_old_testament());
    assert_eq!(
        Book::Genesis.aliases().collect::<Vec<_>>(),
        ["genesis", "gen"]
    );
    assert_eq!(
        Book::Psalms.aliases().collect::<Vec<_>>(),
        ["psalms", "psalm", "pslm", "psm", "pss", "psa", "ps"]
    );
    let verses: usize = Book::all()
        .iter()
        .flat_map(|b| b.verse_counts())
        .map(|&v| v as usize)
        .sum();
    assert_eq!(verses, 31_102);
}
