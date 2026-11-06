#!/usr/bin/env python3
"""Differential testing of elm-bible-rs against the Elm package it ports.

The Elm package in this repo (../../src) is the reference. This script builds
compat/src/Harness.elm against it, runs it under node and compares its answers
with elm-bible-rs, or with another version of the Elm. The cases are
generated each run, and the Elm's answers are the expected results.

    # Generate the corpus (about 510k cases: every alias at the chapter and
    # verse bounds, every verse, every pair of books, the curated cases below,
    # random near-misses), run it through the Elm and the installed
    # elm-bible-rs module, and report every disagreement. --seed and --fuzz
    # change the random part. --quick runs only the ~4k curated cases, in a
    # few seconds.
    python3 rust/compat/corpus.py diff [--quick] [--seed N] [--fuzz N]

    # Which answers changed between a published version and ../../src? For
    # reviewing a change to the Elm. Always exits 0.
    python3 rust/compat/corpus.py changes --from monty5811/elm-bible@2.1.0

`--elm-package monty5811/elm-bible@2.0.1` builds the harness against a
published version from the Elm package registry instead of ../../src, for
any command.

Known, accepted differences between the Elm and elm-bible-rs go in
compat/allowed_differences.json ({"parse": {input: why}, "format": {...}}).
It is empty: none are known.

Needs node and the Elm compiler from the repo's node_modules (`yarn install`
at the repo root). `diff` also needs elm-bible-rs installed in the running
Python (`uv pip install ./rust`).
"""

from __future__ import annotations

import argparse
import json
import os
import random
import re
import subprocess
import sys
import tempfile
from pathlib import Path

COMPAT = Path(__file__).resolve().parent
CRATE = COMPAT.parent
REPO = CRATE.parent
ELM_SRC = REPO / "src"
ELM = REPO / "node_modules" / ".bin" / "elm"
ALLOWED = COMPAT / "allowed_differences.json"
PACKAGE = "monty5811/elm-bible"
BATCH = 50_000

Result = list  # [start, end] for parse, or a str for format; errors are "!message"


# ---------------------------------------------------------------------------
# Book data, read from the Elm source through the table generator so this
# script doesn't need elm-bible-rs installed to write the corpus.


def load_books(elm_src: Path = ELM_SRC) -> list[dict]:
    sys.path.insert(0, str(CRATE / "scripts"))
    import gen_tables

    book_elm = (elm_src / "Internal" / "Book.elm").read_text()
    parser_elm = (elm_src / "Internal" / "Parser.elm").read_text()
    books = {}
    for constructor, num, name, _chapters, verses in gen_tables.BOOK_DATA.findall(book_elm):
        books[constructor] = {
            "num": int(num),
            "name": name,
            "verses": [int(v) for v in verses.replace(",", " ").split()],
            "aliases": [],
        }
    tokens = gen_tables.TOKENS_LIST.search(parser_elm)
    assert tokens
    for constructor, token_list in gen_tables.TOKEN_ENTRY.findall(tokens.group(1)):
        books[constructor]["aliases"] = re.findall(r'"([^"]*)"', token_list)
    return sorted(books.values(), key=lambda b: b["num"])


def cached_package_src(elm_package: str) -> Path:
    """A published version's src/ in Elm's package cache (filled by building the harness)."""
    name, version = elm_package.split("@")
    home = Path(os.environ.get("ELM_HOME") or Path.home() / ".elm")
    candidates = sorted(home.glob(f"*/packages/{name}/{version}/src"))
    if not candidates:
        raise SystemExit(f"{elm_package} is not in Elm's package cache under {home}")
    return candidates[-1]


def verses_in(book: dict, chapter: int) -> int:
    return book["verses"][chapter - 1] if 1 <= chapter <= len(book["verses"]) else 0


def enc(book: dict, chapter: int, verse: int) -> int:
    return book["num"] * 1_000_000 + chapter * 1_000 + verse


# ---------------------------------------------------------------------------
# Cases


def chapter_bounds(book: dict) -> list[int]:
    n = len(book["verses"])
    return sorted({0, 1, 2, n - 1, n, n + 1} - {-1})


def verse_bounds(book: dict, chapter: int) -> list[int]:
    v = verses_in(book, chapter)
    return sorted({0, 1, v - 1, v, v + 1} - {-1})


def spellings(alias: str) -> list[str]:
    return sorted({alias, alias.upper(), alias.title()})


TINY_INPUTS = ["", " ", "  ", "\t", "\n", "-", ":", "1", "x", "e", ".", "0x"]

# Numbers elm/parser treats specially, and huge ones.
NUMBERS = [
    "0",
    "00",
    "01",
    "007",
    "0x1",
    "0X1",
    "0o7",
    "0b1",
    "1e5",
    "1E5",
    "1e",
    "1e+",
    "1e-2",
    "1.5",
    "1.",
    ".5",
    "2147483647",
    "2147483648",
    "4294967296",
    "9007199254740993",
    "9" * 30,
    "1" + "0" * 400,
]


def number_cases() -> list[str]:
    return [s for n in NUMBERS for s in (f"Gen {n}", f"Gen 1:{n}", f"Jude {n}", f"Gen 1-{n}", f"Gen {n}:1")]


# Spacing, punctuation, the Elm's surprising behaviours and their edge cases.
SPECIAL_INPUTS = [
    "Psalms 23",
    "Psalms",
    "psalm 23",
    "Ps 23",
    "Ps. 23",
    "Matt 5–7",
    "Matt 5—7",
    "Matt 5 – 7",
    "John 3:16; Rom 5:8",
    "John 3:16, 17",
    "John 3:16.",
    "Gen 1 esv",
    "Gen 1 ESV",
    "Gen 1 NIV",
    "Gen\t1",
    "Gen 1\t:1",
    " Gen 1",
    "Gen 1 ",
    "Gen  1 :  1 -  5",
    "Gen\n1:1",
    "Gen\r\n1",
    "GEN 1:1",
    "gEn 1:1",
    "1Sam 1",
    "1 sam 1",
    "I Sam 1",
    "i sam 1",
    "1sam1",
    "Gen1:1",
    "Genesis1",
    "Song of Songs 1",
    "Song of Solomon 1",
    "Song 1",
    "Phil 1",
    "Philemon 1",
    "Phlm 1",
    "Phil 1 - Phlm 2",
    "Jude - Rev",
    "Gen 1:1-Exod 2",
    "Gen 1 Exod 2",
    "Gen - ",
    "Gen 1 -",
    "Gen 1:",
    ":1",
    "-1",
    "Gen -1",
    "Gen 1::1",
    "Gen 1--2",
    "İsa 1",
    "Kings 1",
    "1 Kgs 1",
    "Gen ١",
    "Gen 1",
    "ß 1",
    "Revelation 22:21",
    "Rev 22:22",
    "Rev 23",
    "Gen 0",
    "Gen 0:0",
    "Gen 0:1",
    "Jude 0",
    "Gen 1:0-0",
    # Whitespace normalising, dots after books, "3.16", letters after
    # aliases, ordinals, plural Psalms and the less common aliases.
    "Gen\u00a01:1",
    "Gen\u30001",
    "Gen\ufeff1",
    "Gen\u00851",
    "Gen\u20281",
    "\u00a0Gen 1",
    "Gen. 1",
    "Gen.1",
    "Gen..1",
    "Gen . 1",
    "1 Cor. 13",
    "John 3.16",
    "John 3.16-18",
    "John 3:16.17",
    "John 3. 16",
    "John 3.",
    "John 3.x",
    "Gen 1:1.5",
    "Psalms 151",
    "Psalm 151:1",
    "Ps 0",
    "Gens 1",
    "Genesisx 1",
    "Genesis\u00e9 1",
    "Genesis\u00df 1",
    "Genesis\u01c5 1",
    "Genesis\u0661",
    "genesis_1",
    "1st Cor 13",
    "First Corinthians 13",
    "Third John 2",
    "III John 2",
    "Jhn 3:16",
    "Revelations 1",
    "Song of Sol 2",
    "Canticles 2",
    "Qoh 1",
    "John 3:16 (ESV)",
    "Gen 001:001",
    "Gen 1:01-02",
    "Gen 1:1 - Exod 0",
    "Gen 1:1 - Exod 1:0",
    "Obad 1:22",
    "Obad 0",
    "Jude 1:1",
    "3 John 15",
]

# Bad book numbers, negatives, and values big enough for Elm's `//` to
# wrap at 32 bits or for doubles to lose precision.
FORMAT_SPECIALS = [
    0,
    1,
    999,
    1_000,
    999_999,
    1_000_000,
    1_001_001,
    66_022_021,
    67_000_000,
    -1,
    -1_001_001,
    2**31 * 1_000_000,
    2**32 * 1_000_000 + 1_001_001,
    -(2**32) * 1_000_000 + 1_001_001,
    (2**32 - 1) * 1_000_000 + 1_001_001,
    2**53,
    2**53 + 1,
    2**53 + 1_001_001,
    2**63 - 1,
    -(2**63),
    1_001_001 + 2**32 * 1_000,
    19_000_000 - 2**32 * 1_000_000 * 3,
    4_294_967_296_000_000 * 5 + 40_005_007,
]


def parse_cases(books: list[dict], rng: random.Random, fuzz: int) -> list[str]:
    cases: list[str] = list(TINY_INPUTS)

    # Every alias (in three casings) in every single-book shape, at the
    # chapter and verse bounds.
    for book in books:
        chapters = chapter_bounds(book)
        for alias in book["aliases"]:
            for a in spellings(alias):
                cases.append(a)
                for c in chapters:
                    cases.append(f"{a} {c}")
                    for c2 in chapters:
                        cases.append(f"{a} {c}-{c2}")
                    for v in verse_bounds(book, c):
                        cases.append(f"{a} {c}:{v}")
                        for v2 in verse_bounds(book, c):
                            cases.append(f"{a} {c}:{v}-{v2}")
                        if a == alias:
                            for c2 in chapters:
                                for v2 in verse_bounds(book, c2):
                                    cases.append(f"{a} {c}:{v}-{c2}:{v2}")

    # Two-book shapes, between books with one and many chapters, both ways round.
    by_name = {b["name"]: b for b in books}
    picks = [by_name[n] for n in ("Genesis", "Exodus", "Obadiah", "Psalm", "Philemon", "Jude", "Revelation")]
    for b1 in picks:
        for b2 in picks:
            a1, a2 = b1["aliases"][-1], b2["aliases"][0]
            cases += [f"{a1} - {a2}", f"{a1}-{a2}"]
            for c in chapter_bounds(b1):
                for c2 in chapter_bounds(b2):
                    cases += [f"{a1} {c} - {a2} {c2}", f"{a1} - {a2} {c2}"]
                    for v in verse_bounds(b1, c):
                        cases.append(f"{a1} {c}:{v} - {a2} {c2}")
                        for v2 in verse_bounds(b2, c2):
                            cases.append(f"{a1} {c}:{v} - {a2} {c2}:{v2}")
                    for v2 in verse_bounds(b2, c2):
                        cases.append(f"{a1} {c} - {a2} {c2}:{v2}")

    # Every verse of the Bible, by its formatted name.
    for book in books:
        for c, count in enumerate(book["verses"], start=1):
            for v in range(1, count + 1):
                cases.append(f"{book['name']} {c}:{v}")

    # Every pair of books.
    for b1 in books:
        for b2 in books:
            cases.append(f"{b1['name']} - {b2['name']}")

    # Numbers elm/parser treats specially, and huge ones.
    cases += number_cases()

    # Spacing, punctuation and the edge cases above.
    cases += SPECIAL_INPUTS

    # Random near-misses: take a valid reference and corrupt it.
    alphabet = list(" \t\n.:;,-–—e0123456789xobEsa") + [
        "İ",
        "K",
        "ß",
        " ",
        "\u00a0",
        "\u0085",
        "\ufeff",
        "\u00e9",
        "\u01c5",
    ]
    seeds = [c for c in cases if c and len(c) < 30]
    for _ in range(fuzz):
        chars = list(rng.choice(seeds))
        for _ in range(rng.randint(1, 3)):
            op = rng.randrange(3)
            i = rng.randrange(len(chars) + 1)
            if op == 0:
                chars.insert(i, rng.choice(alphabet))
            elif op == 1 and chars:
                del chars[min(i, len(chars) - 1)]
            elif chars:
                chars[min(i, len(chars) - 1)] = rng.choice(alphabet)
        cases.append("".join(chars))

    return list(dict.fromkeys(cases))


def format_cases(books: list[dict], rng: random.Random, fuzz: int) -> list[tuple[int, int]]:
    cases: list[tuple[int, int]] = []
    # Every verse on its own.
    for book in books:
        for c, count in enumerate(book["verses"], start=1):
            for v in range(1, count + 1):
                cases.append((enc(book, c, v), enc(book, c, v)))
    # Every pair of books, first verse to last verse.
    for b1 in books:
        for b2 in books:
            last = len(b2["verses"])
            cases.append((enc(b1, 1, 1), enc(b2, last, b2["verses"][-1])))
    # Chapter and verse bounds within each book, and runs across chapters.
    for book in books:
        for c in chapter_bounds(book):
            for v in verse_bounds(book, c):
                for c2 in chapter_bounds(book):
                    for v2 in verse_bounds(book, c2):
                        cases.append((enc(book, c, v), enc(book, c2, v2)))
    # Bad book numbers, negatives, and values big enough for Elm's `//` to
    # wrap at 32 bits or for doubles to lose precision.
    specials = FORMAT_SPECIALS
    for s in specials:
        for e in specials:
            cases.append((s, e))
    # Random 64-bit values whose book number wraps into 1..66.
    for _ in range(fuzz // 10):
        k = rng.randint(-(2**31) // 1000, 2**31 // 1000)
        base = k * 2**32 * 1_000_000
        if not -(2**63) <= base <= 2**63 - 2**40:
            continue
        a = base + rng.randint(0, 67_000_000)
        b = base + rng.randint(0, 67_000_000)
        cases.append((a, b))
    return list(dict.fromkeys(cases))


# ---------------------------------------------------------------------------
# Curated cases: a few thousand chosen inputs, always part of `diff` and
# `changes`, and all of `diff --quick`.

# Examples of behaviour worth pinning. README.md's list of accepted shapes is
# read from the file itself (readme_examples).
EXAMPLES = [
    "John 3:16; Rom 5:8",
    "John 3:16, 18",
    "Gen 1 NIV",
    "Gen 1e5",
    "genesis_1",
    "Gen 1:1-Exod",
    "Jn 3:16",
    "Mk 1",
    "Kings 1",
    "Jude 3",
    "Jude 1:3",
    "Jude 2:3",
    "Gen 99999999999999999999",
    "Matt 5 — 7",
    "Ps 23:7",
    "Ps 151",
    "psalms",
    "  ",
    "Gen 01",
    "Gen 0 1",
    "Gen 01:005",
    "Gen 0:0",
    "GEN 1",
    "gEn 1",
    "1 Kgs 1",
    "Genesis1",
    "Genesisé 1",
    "Matt 5–7",
    "Psalms 23",
    "Ps. 23",
    "1st Cor 1",
    "Jhn 1",
    "Revelations 1",
    "John 3.16",
    "John 3:16 (ESV)",
    "Gen 51",
    "1 cor 13:4-7",
    "Gen 1:1-5",
]

# A case for each error message, with the book names and numbers taken out.
# `diff` fails if the Elm never gives one of these, or gives a message that
# isn't listed here.
ERROR_CASES = {
    "No reference found": "Jn 3:16",
    "No valid reference found": "Gen 1:1-Exod",
    "Use a colon between chapter and verse, as in John <n>:<n>": "John 3.16",
    "Invalid book number": (0, 0),
    "End book must come after start book": "Rev 22:21 - Gen 1",
    "End chapter must come after start chapter": "Gen 2-1",
    "End verse must come after start verse": "Gen 1:5-1",
    "Chapter numbers start at <n>": "Gen 0",
    "Verse numbers start at <n>": "Gen 1:0",
    "<Book> only has <n> chapters": "Gen 51",
    "<Book> only has <n> chapter": "Jude 2:1",
    "There are only <n> Psalms": "Ps 151",
    "<Book> <n> only has <n> verses": "Ps 23:7",
    "<Book> only has <n> verses": "Jude 26",
}


def message_template(message: str, books: list[dict]) -> str:
    for name in sorted((b["name"] for b in books), key=len, reverse=True):
        if message.startswith(name + " "):
            message = "<Book>" + message[len(name) :]
            break
    return re.sub(r"\d+", "<n>", message)


def error_templates(results: list[Result], books: list[dict]) -> set[str]:
    return {message_template(r[1:], books) for r in results if isinstance(r, str) and r.startswith("!")}


FORMAT_EXAMPLES = [(19023001, 19023006), (19001001, 19150006)]


def readme_examples() -> list[str]:
    """Every input in README.md's list of accepted shapes."""
    readme = (CRATE / "README.md").read_text()
    accepts = readme.split("## What it accepts", 1)[1].split("\n## ", 1)[0]
    return re.findall(r"^- `([^`]+)`$", accepts, re.MULTILINE)


def curated_parse_cases(books: list[dict]) -> list[str]:
    by_name = {b["name"]: b for b in books}
    cases: list[str] = list(TINY_INPUTS)

    cases += readme_examples() + EXAMPLES
    cases += [c for c in ERROR_CASES.values() if isinstance(c, str)]

    # Every alias once in each shape. In a single-chapter book the first
    # number is a verse, so the numbers are kept small enough to be valid.
    for book in books:
        last = len(book["verses"])
        c2 = min(2, last)
        v2 = min(3, book["verses"][0])
        for alias in book["aliases"]:
            cases += [
                alias,
                f"{alias} 1",
                f"{alias} 1:1",
                f"{alias} 1:1-{v2}",
                f"{alias} 1-{c2}",
                f"{alias} 1:1-{c2}:1",
            ]
        # The same shapes with the alias in other cases and with a dot.
        alias = book["aliases"][-1]
        cases += [alias.upper(), f"{alias.title()} 1:1", f"{alias}. 1"]

    # Each book's own chapter and verse messages.
    for book in books:
        cases += [f"{book['name']} {len(book['verses']) + 1}", f"{book['name']} 1:{book['verses'][0] + 1}"]

    # Chapter and verse bounds: 0, 1, the last and the last + 1.
    for name in ("Obadiah", "Philemon", "2 John", "Jude", "Genesis", "Psalm", "Isaiah", "Revelation"):
        book = by_name[name]
        last = len(book["verses"])
        for c in (0, 1, last, last + 1):
            cases.append(f"{name} {c}")
            cases.append(f"{name} 1-{c}")
            for v in (0, 1, verses_in(book, c), verses_in(book, c) + 1):
                cases.append(f"{name} {c}:{v}")
            cases.append(f"{name} 1:1-{c}:{max(verses_in(book, c), 1)}")

    # Two-book ranges, both ways round, between books with one and many chapters.
    picks = [by_name[n] for n in ("Genesis", "Exodus", "Obadiah", "Psalm", "Jude", "Revelation")]
    for b1 in picks:
        for b2 in picks:
            a1, a2 = b1["aliases"][-1], b2["aliases"][0]
            cases += [
                f"{a1} - {a2}",
                f"{a1}-{a2}",
                f"{a1} 1 - {a2} 1",
                f"{a1} 1:1 - {a2} 1:2",
                f"{a1} - {a2} 1",
                f"{a1} 1 - {a2} 1:2",
                f"{a1} 1:1 - {a2} 1",
            ]

    cases += number_cases()
    cases += SPECIAL_INPUTS
    return list(dict.fromkeys(cases))


def curated_format_cases(books: list[dict]) -> list[tuple[int, int]]:
    by_name = {b["name"]: b for b in books}
    cases: list[tuple[int, int]] = []
    cases += FORMAT_EXAMPLES
    cases += [c for c in ERROR_CASES.values() if isinstance(c, tuple)]
    # Every book whole, and its first verse.
    for book in books:
        last = len(book["verses"])
        cases.append((enc(book, 1, 1), enc(book, last, book["verses"][-1])))
        cases.append((enc(book, 1, 1), enc(book, 1, 1)))
    # Chapter and verse bounds, as the start and as the end.
    for name in ("Obadiah", "Jude", "Genesis", "Psalm", "Revelation"):
        book = by_name[name]
        last = len(book["verses"])
        for c in (0, 1, last, last + 1):
            for v in (0, 1, verses_in(book, c), verses_in(book, c) + 1):
                cases.append((enc(book, c, v), enc(book, c, v)))
                cases.append((enc(book, 1, 1), enc(book, c, v)))
                cases.append((enc(book, c, v), enc(book, last, book["verses"][-1])))
        cases.append((enc(book, 1, 2), enc(book, 1, 1)))
        cases.append((enc(book, 2, 1), enc(book, 1, 1)))
    # Two-book ranges, both ways round.
    picks = [by_name[n] for n in ("Genesis", "Obadiah", "Psalm", "Jude", "Revelation")]
    for b1 in picks:
        for b2 in picks:
            cases.append((enc(b1, 1, 1), enc(b2, 1, 1)))
    for start in FORMAT_SPECIALS:
        for end in FORMAT_SPECIALS:
            cases.append((start, end))
    return list(dict.fromkeys(cases))


# ---------------------------------------------------------------------------
# Running the Elm


def build_harness(out_dir: Path, elm_package: str | None) -> Path:
    """Compile Harness.elm against ../../src, or against a published version."""
    output = out_dir / "harness.js"
    if elm_package:
        project = out_dir / "project"
        project.mkdir()
        elm_json = json.loads((COMPAT / "elm.json").read_text())
        name, version = elm_package.split("@")
        elm_json["source-directories"] = [str(COMPAT / "src")]
        elm_json["dependencies"]["direct"][name] = version
        (project / "elm.json").write_text(json.dumps(elm_json, indent=4))
        main = str(COMPAT / "src" / "Harness.elm")
    else:
        project = COMPAT
        main = "src/Harness.elm"
    subprocess.run(
        [str(ELM), "make", main, "--optimize", "--output", str(output)],
        cwd=project,
        check=True,
        stdout=subprocess.DEVNULL,
    )
    return output


def run_elm(harness: Path, parse: list[str], fmt: list[tuple[int, int]]) -> tuple[list[Result], list[Result]]:
    def unwrap(r: dict) -> Result:
        return r["ok"] if "ok" in r else "!" + r["err"]

    parsed: list[Result] = []
    formatted: list[Result] = []
    for i in range(0, max(len(parse), len(fmt)), BATCH):
        batch = {"parse": parse[i : i + BATCH], "format": [list(p) for p in fmt[i : i + BATCH]]}
        out = subprocess.run(
            ["node", str(COMPAT / "run-elm.js"), str(harness)],
            input=json.dumps(batch),
            capture_output=True,
            text=True,
            check=True,
        )
        result = json.loads(out.stdout)
        if "error" in result:
            raise SystemExit(result["error"])
        parsed += [unwrap(r) for r in result["parse"]]
        formatted += [unwrap(r) for r in result["format"]]
    return parsed, formatted


def run_elm_bible_rs(parse: list[str], fmt: list[tuple[int, int]]) -> tuple[list[Result], list[Result]]:
    import elm_bible_rs

    def p(text: str) -> Result:
        try:
            return list(elm_bible_rs.parse(text).encode())
        except elm_bible_rs.InvalidReference as e:
            return "!" + str(e)

    def f(pair: tuple[int, int]) -> Result:
        try:
            return elm_bible_rs.format(*pair)
        except elm_bible_rs.InvalidReference as e:
            return "!" + str(e)

    return [p(t) for t in parse], [f(x) for x in fmt]


def with_round_trips(harness: Path, parse: list[str], fmt: list[tuple[int, int]]) -> tuple[list[str], list, list]:
    """Run the cases, then parse every formatted reference that isn't already an input."""
    parsed, formatted = run_elm(harness, parse, fmt)
    seen = set(parse)
    extra = [x for x in dict.fromkeys(f for f in formatted if not f.startswith("!")) if x not in seen]
    extra_parsed, _ = run_elm(harness, extra, [])
    return parse + extra, parsed + extra_parsed, formatted


def full_cases(books: list[dict], seed: int, fuzz: int) -> tuple[list[str], list[tuple[int, int]]]:
    rng = random.Random(seed)
    return parse_cases(books, rng, fuzz), format_cases(books, rng, fuzz)


def compare(label: str, inputs: list, expected: list, actual: list, allowed: dict) -> int:
    bad = 0
    for i, e, a in zip(inputs, expected, actual):
        if e != a:
            key = i if isinstance(i, str) else f"{i[0]},{i[1]}"
            if key in allowed:
                continue
            bad += 1
            if bad <= 20:
                print(f"  {label} {i!r}: elm={e!r} other={a!r}")
    print(f"{label}: {len(inputs)} cases, {bad} unexplained differences")
    return bad


def show(result: Result) -> str:
    if isinstance(result, str):
        return f"error: {result[1:]}" if result.startswith("!") else result
    return f"({result[0]}, {result[1]})"


def summarise_changes(
    label: str, inputs: list, old: list, new: list, books: list[dict], examples: int, max_kinds: int = 40
) -> list[str]:
    """Group changed answers by the kind of change, with a few examples of each."""

    def kind(r: Result) -> str:
        if isinstance(r, str) and r.startswith("!"):
            return "error: " + message_template(r[1:], books)
        return "ok"

    groups: dict[tuple[str, str], list[str]] = {}
    for i, o, n in zip(inputs, old, new):
        if o != n:
            groups.setdefault((kind(o), kind(n)), []).append(f"`{i!r}`: {show(o)} -> {show(n)}")
    total = sum(len(v) for v in groups.values())
    lines = [f"**{label}**: {total} of {len(inputs)} answers changed", ""]
    kinds = sorted(groups.items(), key=lambda kv: -len(kv[1]))
    for (o, n), rows in kinds[:max_kinds]:
        lines.append(f"- {len(rows)} x {o} -> {n}")
        lines += [f"    - {r}" for r in rows[:examples]]
    if len(kinds) > max_kinds:
        rest = sum(len(rows) for _, rows in kinds[max_kinds:])
        lines.append(f"- ... and {rest} more answers, of {len(kinds) - max_kinds} more kinds")
    return lines


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("command", choices=["diff", "changes"])
    parser.add_argument("--quick", action="store_true", help="diff: only the curated cases")
    parser.add_argument("--seed", type=int, default=1444, help="seed for the full corpus's random cases")
    parser.add_argument("--fuzz", type=int, default=60_000, help="random near-miss inputs to add")
    parser.add_argument("--elm-package", help="e.g. monty5811/elm-bible@2.0.1 instead of ../../src")
    parser.add_argument("--from", dest="from_package", help="changes: the old version, e.g. monty5811/elm-bible@2.1.0")
    parser.add_argument("--examples", type=int, default=5, help="changes: examples to show of each kind")
    parser.add_argument("--markdown", type=Path, help="changes: also append the summary to this file")
    args = parser.parse_args()

    allowed = json.loads(ALLOWED.read_text())
    with tempfile.TemporaryDirectory() as tmp:
        harness = build_harness(Path(tmp), args.elm_package)
        books = load_books(cached_package_src(args.elm_package) if args.elm_package else ELM_SRC)

        if args.command == "changes":
            if not args.from_package:
                parser.error("changes needs --from NAME@VERSION")
            old_dir = Path(tmp) / "old"
            old_dir.mkdir()
            old_harness = build_harness(old_dir, args.from_package)
            old_books = {b["num"]: b for b in load_books(cached_package_src(args.from_package))}
            for book in books:
                book["aliases"] += [a for a in old_books[book["num"]]["aliases"] if a not in book["aliases"]]
            parse, fmt = full_cases(books, args.seed, args.fuzz)
            parse += curated_parse_cases(books)
            fmt += curated_format_cases(books)
            parse, fmt = list(dict.fromkeys(parse)), list(dict.fromkeys(fmt))
            new_parsed, new_formatted = run_elm(harness, parse, fmt)
            old_parsed, old_formatted = run_elm(old_harness, parse, fmt)
            new_label = args.elm_package or "src/"
            lines = [f"### Behaviour changes from {args.from_package} to {new_label}", ""]
            lines += summarise_changes("parse", parse, old_parsed, new_parsed, books, args.examples)
            lines += [""]
            lines += summarise_changes("format", fmt, old_formatted, new_formatted, books, args.examples)
            text = "\n".join(lines) + "\n"
            print(text)
            if args.markdown:
                with open(args.markdown, "a", encoding="utf-8") as f:
                    f.write(text)
            return 0

        # diff
        parse, fmt = curated_parse_cases(books), curated_format_cases(books)
        if not args.quick:
            full_parse, full_fmt = full_cases(books, args.seed, args.fuzz)
            parse, fmt = list(dict.fromkeys(full_parse + parse)), list(dict.fromkeys(full_fmt + fmt))
        parse, parsed, formatted = with_round_trips(harness, parse, fmt)
        ours_parsed, ours_formatted = run_elm_bible_rs(parse, fmt)
        bad = compare("parse", parse, parsed, ours_parsed, allowed["parse"])
        bad += compare("format", fmt, formatted, ours_formatted, allowed["format"])
        if not args.elm_package:
            found = error_templates(parsed + formatted, books)
            if found != set(ERROR_CASES):
                print("ERROR_CASES in corpus.py is out of date.")
                print(f"  Never given by the Elm: {sorted(set(ERROR_CASES) - found)}")
                print(f"  Given by the Elm but not listed: {sorted(found - set(ERROR_CASES))}")
                bad += 1
        if bad:
            rerun = "--quick" if args.quick else f"--seed {args.seed} --fuzz {args.fuzz}"
            print(f"Reproduce with: python3 rust/compat/corpus.py diff {rerun}")
        return 1 if bad else 0


if __name__ == "__main__":
    sys.exit(main())
