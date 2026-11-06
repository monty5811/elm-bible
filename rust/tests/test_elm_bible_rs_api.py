"""The Python API's shape: types, errors, books."""

import elm_bible_rs
import pytest
from elm_bible_rs import Book, InvalidReference, Reference


def test_parse_and_format() -> None:
    ref = elm_bible_rs.parse("gen 1:1-5")
    assert isinstance(ref, Reference)
    assert str(ref) == ref.format() == "Genesis 1:1-5"
    assert ref.encode() == (ref.start, ref.end) == (1_001_001, 1_001_005)
    assert (ref.start_book, ref.start_chapter, ref.start_verse) == (Book.Genesis, 1, 1)
    assert (ref.end_book, ref.end_chapter, ref.end_verse) == (Book.Genesis, 1, 5)
    assert Reference.parse("Genesis 1:1-5") == ref
    assert elm_bible_rs.format(*ref.encode()) == "Genesis 1:1-5"
    assert repr(ref) == "<Reference Genesis 1:1-5 (1001001, 1001005)>"


def test_references_are_values() -> None:
    a = elm_bible_rs.parse("Gen 1")
    b = elm_bible_rs.decode(1_001_001, 1_001_031)
    assert a == b and hash(a) == hash(b)
    assert len({a, b, elm_bible_rs.parse("Gen 2")}) == 2
    with pytest.raises(AttributeError):
        a.start = 5  # type: ignore[misc]


@pytest.mark.parametrize(
    ("text", "code", "message"),
    [
        ("", "empty", "No reference found"),
        ("Gen 1 2", "unrecognised", "No valid reference found"),
        ("John 3.16", "syntax", "Use a colon between chapter and verse, as in John 3:16"),
        ("Gen 0", "zero_chapter", "Chapter numbers start at 1"),
        ("Gen 1:0", "zero_verse", "Verse numbers start at 1"),
        ("Ps 151", "chapter_out_of_range", "There are only 150 Psalms"),
        ("Gen 51", "chapter_out_of_range", "Genesis only has 50 chapters"),
        ("Gen 1:32", "verse_out_of_range", "Genesis 1 only has 31 verses"),
        ("Jude 26", "verse_out_of_range", "Jude only has 25 verses"),
        ("Exod 1 - Gen 1", "books_out_of_order", "End book must come after start book"),
        ("Gen 2-1", "chapters_out_of_order", "End chapter must come after start chapter"),
        ("Gen 1:2-1", "verses_out_of_order", "End verse must come after start verse"),
    ],
)
def test_parse_errors(text: str, code: str, message: str) -> None:
    with pytest.raises(InvalidReference) as info:
        elm_bible_rs.parse(text)
    assert str(info.value) == message
    assert info.value.code == code
    assert isinstance(info.value, ValueError)


def test_decode_errors() -> None:
    with pytest.raises(InvalidReference, match="^Invalid book number$") as info:
        elm_bible_rs.format(67_001_001, 67_001_001)
    assert info.value.code == "invalid_book_number"
    with pytest.raises(OverflowError):
        elm_bible_rs.format(2**64, 2**64)


def test_books() -> None:
    assert len(elm_bible_rs.BOOKS) == 66
    assert elm_bible_rs.BOOKS == Book.all()
    assert [b.number for b in elm_bible_rs.BOOKS] == list(range(1, 67))
    psalms = Book.from_number(19)
    assert psalms is Book.Psalms
    assert (psalms.name, psalms.title, str(psalms)) == ("Psalm", "Psalms", "Psalms")
    assert psalms.chapters == 150 == len(psalms.verse_counts)
    assert psalms.verses(119) == 176 and psalms.verses(0) == 0 and psalms.verses(151) == 0
    assert psalms.aliases == ("psalms", "psalm", "pslm", "psm", "pss", "psa", "ps")
    assert Book.Malachi.testament == "OT" and Book.Matthew.testament == "NT"
    assert Book.Genesis < Book.Exodus and sorted([Book.Jude, Book.Genesis]) == [Book.Genesis, Book.Jude]
    assert Book.Genesis == Book.Genesis and int(Book.Revelation) == 66
    assert sum(sum(b.verse_counts) for b in elm_bible_rs.BOOKS) == 31_102
    with pytest.raises(ValueError, match="no book number 67"):
        Book.from_number(67)
