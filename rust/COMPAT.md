# Compatibility with elm-bible

elm-bible-rs is a port of the Elm package
[`monty5811/elm-bible`](https://package.elm-lang.org/packages/monty5811/elm-bible/latest/),
whose source is in this repo (`../src`). That Elm code is the reference: for
every input, elm-bible-rs gives the same reference, the same formatted text
and the same error message, byte for byte. elm-bible-rs X.Y.Z matches
elm-bible X.Y.Z.

`compat/corpus.py diff` checks this. It generates about 510k cases, runs
them through the real Elm (under node) and through elm-bible-rs, and fails on
any difference. The Elm's answers are the expected results, and the cases
are generated on every run. They include about 4k curated ones (every alias in each shape, every
error message, every example in the README), which
`diff --quick` runs on their own in a few seconds.

## Mapping

| Elm (`Bible`)            | Rust                         | Python                          |
| ------------------------ | ---------------------------- | ------------------------------- |
| `fromString`             | `Reference::parse` / `parse` | `elm_bible_rs.parse`, `Reference.parse` |
| `format`                 | `Display` / `to_string()`    | `str(ref)`, `ref.format()`      |
| `encode`                 | `Reference::encode`          | `ref.encode()`, `ref.start`, `ref.end` |
| `decode`                 | `Reference::decode`          | `elm_bible_rs.decode`, `Reference.decode` |
| `decode >> format`       | `elm_bible_rs::format`           | `elm_bible_rs.format(start, end)`   |
| `toStartBook` etc.       | `start_book()` etc.          | `ref.start_book` etc.           |
| `toStartBookString` etc. | `start_book().name()` etc.   | `ref.start_book.name` etc.      |
| `Bible.Book.name`        | `Book::name` ("Psalm")       | `Book.name`                     |
| `Bible.Book.title`       | `Book::title` ("Psalms")     | `Book.title`, `str(book)`       |
| `Bible.Book.all`, `fromInt`, `toInt` | `Book::all`, `from_number`, `number` | `BOOKS`, `Book.from_number`, `.number` |
| `Bible.Book.numChapters`, `numVerses` | `Book::chapters`, `verses` | `.chapters`, `.verses()`  |
| `Bible.Book.aliases`     | `Book::aliases`              | `Book.aliases`                  |
| `Err String`             | `Error` (`Display` = the Elm string) | `InvalidReference` (`str()` = the Elm string, plus `.code`) |

elm-bible-rs generates its book table and aliases from the Elm source in
`../src` (`scripts/gen_tables.py`), so the data can't drift either.

## Where elm-bible-rs can't match

These are the only known differences from the Elm. Normal input never
reaches them.

- **Integers outside the 64-bit range.** Elm takes any JavaScript number.
  elm-bible-rs takes `i64`, and Python raises `OverflowError` for an int outside
  ±2^63. Every value outside `1000000..=66999999` is rejected anyway, so
  results match: the corpus includes ±2^63 and values past 2^53. Values
  stored in 64-bit integer columns can't reach this.
- **Unicode version.** Lowercasing and the letter test use Rust's Unicode
  tables, and JavaScript uses the engine's. They could differ for characters added in different
  Unicode versions. No book alias can be produced that way except through
  existing mappings such as the Kelvin sign, which both handle.
`compat/allowed_differences.json` is where any accepted difference would go.
It is empty.

## Changing behaviour

The Elm in `../src` and elm-bible-rs must keep agreeing. **A change to `src/`
must keep `rust/` passing, in the same PR.** CI fails otherwise. To change
behaviour:

1. Change the Elm in `src/`, with its tests.
2. If you changed `Internal/Book.elm` or the alias table in
   `Internal/Parser.elm`, run `python3 rust/scripts/gen_tables.py`.
3. Run `python3 rust/compat/corpus.py changes --from monty5811/elm-bible@X.Y.Z`
   against the latest release. It lists every answer your change alters:
   check that it is exactly the change you meant.
4. Make the same change in `rust/`, with tests, until `cargo test` and the
   Python tests pass. If you added an error message, add a case for it to
   `ERROR_CASES` in `corpus.py`, and any input worth pinning to `EXAMPLES`.
5. With elm-bible-rs installed, run `rust/compat/corpus.py diff --quick`
   while you work, then `rust/compat/corpus.py diff` for the full
   comparison, as CI will.

On a PR, CI also prints a summary of every answer the PR's `src/` changes
compared with the latest release (`corpus.py changes`).

When the Elm is released as X.Y.Z, release elm-bible-rs X.Y.Z too: set the
version in `Cargo.toml` and `pyproject.toml`, and push a `rs-X.Y.Z` tag. The
release workflow builds the wheels and publishes them to PyPI.
