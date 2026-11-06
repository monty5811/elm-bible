# elm-bible-rs

Parse, validate, format and encode English Bible references, in Rust, with
Python bindings.

```python
>>> import elm_bible_rs
>>> ref = elm_bible_rs.parse("1 cor 13:4-7")
>>> str(ref)
'1 Corinthians 13:4-7'
>>> ref.encode()
(46013004, 46013007)
>>> elm_bible_rs.format(19023001, 19023006)
'Psalm 23:1-6'
>>> elm_bible_rs.parse("Gen 51")
Traceback (most recent call last):
  ...
elm_bible_rs.InvalidReference: Genesis only has 50 chapters
```

elm-bible-rs is a port of the Elm package `monty5811/elm-bible`. It gives the same
answer, and the same error message, as the Elm code for every input, so a
server in Python and a frontend in Elm can share one idea of what a reference
is. [COMPAT.md](COMPAT.md) covers what that includes.

## Installing

```sh
pip install elm-bible-rs
```

The wheels are abi3, for CPython 3.11 and later, on Linux (x86_64, aarch64)
and macOS. Elsewhere pip builds the sdist, which needs a Rust toolchain.

The crate isn't on crates.io. To use it from Rust, depend on the git
repository:

```toml
elm-bible-rs = { git = "https://github.com/monty5811/elm-bible", tag = "rs-2.1.0" }
```

## Versions

**elm-bible-rs X.Y.Z matches elm-bible X.Y.Z**: same parsing, formatting,
validation, book table and error messages. A Python project that also has an
Elm frontend should pin both to the same version and bump them together.

The Rust and Python package is released from `rs-X.Y.Z` tags, separate from
the Elm package's bare `X.Y.Z` tags. If elm-bible-rs needs a fix that doesn't
change behaviour (packaging, say) without a new Elm release, it gets a
post-release on PyPI (`X.Y.Z.post1`, tag `rs-X.Y.Z.post1`), so the version
still names the Elm release it matches.

## What it accepts

- `Genesis`
- `Genesis 1`
- `Genesis 1:1`
- `Genesis 1:1-20`
- `Genesis 1:20-2:24`
- `Genesis 1-5`
- `Genesis 1 - Exodus 5`
- `Genesis 1:1 - Exodus 5`
- `Genesis 1 - Exodus 5:20`
- `Genesis 1:1 - Exodus 5:20`
- `Genesis - Exodus 5`
- `Genesis - Revelation`

Books can be abbreviated, with or without a trailing dot: `Gen`, `1 Sam`,
`I Sam`, `1st Sam`, `First Samuel`, `Ps.`, `Psalms`, `Jhn` and others
(`Book.aliases`). Case and whitespace don't matter, and text after the
reference is ignored (`John 3:16 (ESV)`). Hyphens, en dashes and em dashes all
work as the dash. Chapter and verse numbers start at 1, and `John 3.16` is an
error rather than John 3. In a single-chapter book (Obadiah, Philemon,
2 John, 3 John, Jude), the number is a verse: `Jude 3-5`.

Only English names and the 66-book Protestant canon are supported.

## Encoding

Each verse encodes as `book * 1_000_000 + chapter * 1_000 + verse`, so
Genesis 1:1 is `1001001`. A reference is a `(start, end)` pair, which you can
store as two integer columns. Plain integer comparisons then give sorting,
"which passages touch Romans 8", and so on.

## Python API

```python
elm_bible_rs.parse(text: str) -> Reference              # raises InvalidReference
elm_bible_rs.decode(start: int, end: int) -> Reference  # raises InvalidReference
elm_bible_rs.format(start: int, end: int) -> str        # decode(...).format()

Reference.start, .end, .encode()
Reference.start_book, .start_chapter, .start_verse  # Book, int, int
Reference.end_book, .end_chapter, .end_verse
Reference.format(), str(ref)                        # "Genesis 1:1-5"
                                                    # (immutable, hashable, ==)

Book.Genesis ... Book.Revelation                    # ordered, hashable, int(book)
Book.all(), elm_bible_rs.BOOKS                          # all 66, in order
Book.from_number(19)                                # Book.Psalms
book.number, .name ("Psalm"), .title ("Psalms"), .chapters,
book.verse_counts, .verses(chapter), .testament ("OT" | "NT"), .aliases

InvalidReference(ValueError)                        # str(e): the message,
                                                    # e.code: "chapter_out_of_range", ...
```

The package ships type stubs (`py.typed`).

## Rust API

```rust
let r: elm_bible_rs::Reference = "Gen 1:1-5".parse()?;
assert_eq!(r.to_string(), "Genesis 1:1-5");
let r = elm_bible_rs::Reference::decode(1_001_001, 1_001_005)?;
let text = elm_bible_rs::format(1_001_001, 1_001_005)?;
```

The Python bindings are behind the `python` feature, so the crate has no
dependencies without it.

## Developing

elm-bible-rs lives in `rust/` of the elm-bible repo, next to the Elm package
it ports (`../src`). **A change to `src/` must keep `rust/` passing, in the
same PR.** CI enforces it. [COMPAT.md](COMPAT.md#changing-behaviour) has the
steps.

From the repo root:

```sh
yarn install                                        # elm and node tools, for compat/
cargo test --manifest-path rust/Cargo.toml          # unit and property (proptest) tests
uv venv && uv pip install ./rust --group rust/pyproject.toml:test  # build the extension into a venv
.venv/bin/pytest rust/tests                         # the Python bindings, including hypothesis properties
python3 rust/scripts/gen_tables.py --check          # tables match ../src
.venv/bin/python rust/compat/corpus.py diff --quick # Elm vs elm-bible-rs on the ~4k curated cases
.venv/bin/python rust/compat/corpus.py diff         # Elm vs elm-bible-rs on the full ~510k generated cases
python3 rust/compat/corpus.py changes --from monty5811/elm-bible@2.1.0  # what src/ changes since a release
```

`.github/workflows/ci.yml` runs all of these.
