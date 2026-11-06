"""Property-based tests of the Python bindings.

tests/properties.rs covers the parser's laws in Rust; these check what the
binding layer adds: exception types and codes, value semantics (==, hash,
ordering) and the int conversions at the Python boundary.
"""

import elm_bible_rs
from elm_bible_rs import BOOKS, Book, InvalidReference, Reference
from hypothesis import given, settings
from hypothesis import strategies as st

CODES = {
    "syntax",
    "empty",
    "unrecognised",
    "invalid_book_number",
    "books_out_of_order",
    "chapters_out_of_order",
    "verses_out_of_order",
    "zero_chapter",
    "zero_verse",
    "chapter_out_of_range",
    "verse_out_of_range",
}


@st.composite
def verses(draw: st.DrawFn) -> int:
    book = draw(st.sampled_from(BOOKS))
    chapter = draw(st.integers(1, book.chapters))
    verse = draw(st.integers(1, book.verses(chapter)))
    return book.number * 1_000_000 + chapter * 1_000 + verse


@st.composite
def passages(draw: st.DrawFn) -> Reference:
    a, b = sorted([draw(verses()), draw(verses())])
    return elm_bible_rs.decode(a, b)


reference_like = st.lists(
    st.one_of(
        st.sampled_from(BOOKS).map(lambda b: b.name),
        st.sampled_from(BOOKS).flatmap(lambda b: st.sampled_from(b.aliases)),
        st.integers(0, 200).map(str),
        st.sampled_from([" ", ":", "-", " - ", "–", "—", "\t", ".", "e", ";", "x"]),
    ),
    max_size=8,
).map("".join)


@settings(max_examples=500)
@given(passages())
def test_passages_round_trip_through_text_and_integers(ref: Reference) -> None:
    assert elm_bible_rs.parse(str(ref)) == ref
    assert elm_bible_rs.decode(*ref.encode()) == ref
    assert elm_bible_rs.format(ref.start, ref.end) == ref.format() == str(ref)
    assert ref.start_book <= ref.end_book
    assert (ref.start_book.number, ref.start_chapter, ref.start_verse) == (
        ref.start // 1_000_000,
        ref.start // 1_000 % 1_000,
        ref.start % 1_000,
    )


@settings(max_examples=500)
@given(passages(), passages())
def test_equality_and_hash_follow_the_encoding(a: Reference, b: Reference) -> None:
    assert (a == b) == (a.encode() == b.encode())
    if a == b:
        assert hash(a) == hash(b)
    assert len({a, b}) == len({a.encode(), b.encode()})


@settings(max_examples=1_000)
@given(st.one_of(st.text(), reference_like))
def test_parse_returns_a_reference_or_raises_invalid_reference(text: str) -> None:
    try:
        ref = elm_bible_rs.parse(text)
    except InvalidReference as e:
        assert isinstance(e, ValueError)
        assert e.code in CODES
        assert e.code != "invalid_book_number"
        assert str(e)
    else:
        assert ref.start <= ref.end
        assert elm_bible_rs.decode(*ref.encode()) == ref


@settings(max_examples=1_000)
@given(st.integers(-(2**63), 2**63 - 1), st.integers(-(2**63), 2**63 - 1))
def test_decode_takes_any_64_bit_pair(start: int, end: int) -> None:
    try:
        ref = elm_bible_rs.decode(start, end)
    except InvalidReference as e:
        assert e.code in CODES
    else:
        assert isinstance(str(ref), str)
        assert isinstance(ref.start_book, Book)


@given(st.one_of(st.integers(min_value=2**63), st.integers(max_value=-(2**63) - 1)))
def test_decode_rejects_integers_beyond_64_bits(n: int) -> None:
    for args in [(n, 1_001_001), (1_001_001, n)]:
        try:
            elm_bible_rs.decode(*args)
        except OverflowError:
            pass
        else:
            raise AssertionError(f"{args} should not convert")


@given(st.sampled_from(BOOKS), st.sampled_from(BOOKS))
def test_books_order_by_number(a: Book, b: Book) -> None:
    assert (a < b) == (a.number < b.number)
    assert (a == b) == (a is b)
    assert Book.from_number(a.number) is a
    assert a.chapters == len(a.verse_counts)
    assert all(a.verses(c + 1) == n for c, n in enumerate(a.verse_counts))
    assert a.verses(0) == a.verses(a.chapters + 1) == a.verses(2**32 + 1) == a.verses(-(2**63)) == 0


@given(st.sampled_from(BOOKS))
def test_books_equal_and_hash_as_their_numbers(book: Book) -> None:
    n = book.number
    assert book == n and hash(book) == hash(n)
    assert {book: "x"}.get(n) == "x" and {n: "x"}.get(book) == "x"
    assert len({book, n}) == 1
