"""Parse, validate, format and encode English Bible references.

    >>> import elm_bible_rs
    >>> ref = elm_bible_rs.parse("Gen 1:1-5")
    >>> str(ref), ref.encode()
    ('Genesis 1:1-5', (1001001, 1001005))
    >>> elm_bible_rs.format(19023001, 19023006)
    'Psalm 23:1-6'

A Rust port of the elm-bible Elm package: same results, same error messages.
"""

from elm_bible_rs._elm_bible_rs import BOOKS, Book, InvalidReference, Reference, __version__, decode, format, parse

__all__ = ["BOOKS", "Book", "InvalidReference", "Reference", "__version__", "decode", "format", "parse"]
