from typing import Literal, TypeAlias, final

__version__: str

@final
class Book:
    """A book of the 66-book Protestant canon. Compare and sort by number."""

    Genesis: Book
    Exodus: Book
    Leviticus: Book
    Numbers: Book
    Deuteronomy: Book
    Joshua: Book
    Judges: Book
    Ruth: Book
    Samuel1: Book
    Samuel2: Book
    Kings1: Book
    Kings2: Book
    Chronicles1: Book
    Chronicles2: Book
    Ezra: Book
    Nehemiah: Book
    Esther: Book
    Job: Book
    Psalms: Book
    Proverbs: Book
    Ecclesiastes: Book
    SongOfSolomon: Book
    Isaiah: Book
    Jeremiah: Book
    Lamentations: Book
    Ezekiel: Book
    Daniel: Book
    Hosea: Book
    Joel: Book
    Amos: Book
    Obadiah: Book
    Jonah: Book
    Micah: Book
    Nahum: Book
    Habakkuk: Book
    Zephaniah: Book
    Haggai: Book
    Zechariah: Book
    Malachi: Book
    Matthew: Book
    Mark: Book
    Luke: Book
    John: Book
    Acts: Book
    Romans: Book
    Corinthians1: Book
    Corinthians2: Book
    Galatians: Book
    Ephesians: Book
    Philippians: Book
    Colossians: Book
    Thessalonians1: Book
    Thessalonians2: Book
    Timothy1: Book
    Timothy2: Book
    Titus: Book
    Philemon: Book
    Hebrews: Book
    James: Book
    Peter1: Book
    Peter2: Book
    John1: Book
    John2: Book
    John3: Book
    Jude: Book
    Revelation: Book

    @staticmethod
    def all() -> tuple[Book, ...]:
        """All 66 books, in canonical order."""

    @staticmethod
    def from_number(number: int) -> Book:
        """The book with this number (1 = Genesis, 66 = Revelation). Raises ValueError otherwise."""

    @property
    def number(self) -> int: ...
    @property
    def name(self) -> str:
        """The name used in references: "Genesis", "Psalm"."""

    @property
    def title(self) -> str:
        """The book's own name, for headings: "Genesis", "Psalms"."""

    @property
    def chapters(self) -> int: ...
    @property
    def verse_counts(self) -> tuple[int, ...]:
        """Verses in each chapter; index 0 is chapter 1."""

    def verses(self, chapter: int) -> int:
        """Verses in the chapter, or 0 when there is no such chapter."""

    @property
    def testament(self) -> Literal["OT", "NT"]: ...
    @property
    def aliases(self) -> tuple[str, ...]:
        """The lowercase spellings the parser accepts, in the Elm table's order, e.g. ("genesis", "gen")."""

    def __int__(self) -> int: ...
    def __lt__(self, other: Book) -> bool: ...
    def __le__(self, other: Book) -> bool: ...
    def __gt__(self, other: Book) -> bool: ...
    def __ge__(self, other: Book) -> bool: ...
    def __hash__(self) -> int: ...

BOOKS: tuple[Book, ...]

ErrorCode: TypeAlias = Literal[
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
]

class InvalidReference(ValueError):
    """Not a valid reference. str(err) is the Elm package's message, e.g.
    "Genesis only has 50 chapters"; err.code names the kind of error."""

    code: ErrorCode

@final
class Reference:
    """A validated passage from a start verse to an end verse, inclusive.

    Immutable and hashable; equal when they cover the same verses.
    """

    @staticmethod
    def parse(text: str) -> Reference:
        """Parse text such as "Gen 1:1-5". Raises InvalidReference."""

    @staticmethod
    def decode(start: int, end: int) -> Reference:
        """Build a reference from its (start, end) encoding. Raises InvalidReference."""

    @property
    def start(self) -> int:
        """The encoded first verse: book * 1_000_000 + chapter * 1_000 + verse."""

    @property
    def end(self) -> int:
        """The encoded last verse."""

    def encode(self) -> tuple[int, int]:
        """(start, end)"""

    @property
    def start_book(self) -> Book: ...
    @property
    def start_chapter(self) -> int: ...
    @property
    def start_verse(self) -> int: ...
    @property
    def end_book(self) -> Book: ...
    @property
    def end_chapter(self) -> int: ...
    @property
    def end_verse(self) -> int: ...
    def format(self) -> str:
        """The canonical text, e.g. "Genesis 1:1-5". Same as str(ref)."""

    def __hash__(self) -> int: ...

def parse(text: str) -> Reference:
    """Parse text such as "Gen 1:1-5". Raises InvalidReference."""

def decode(start: int, end: int) -> Reference:
    """Build a reference from its (start, end) encoding. Raises InvalidReference."""

def format(start: int, end: int) -> str:
    """The canonical text of an encoded (start, end) pair. Raises InvalidReference."""
