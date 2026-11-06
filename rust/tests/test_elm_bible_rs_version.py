"""elm-bible-rs X.Y.Z matches elm-bible X.Y.Z (README.md, "Versions")."""

import json
import re
import tomllib
from pathlib import Path

import elm_bible_rs

CRATE = Path(__file__).resolve().parent.parent


def release(version: str) -> str:
    """X.Y.Z, without a PyPI post-release suffix (X.Y.Z.post1)."""
    return re.sub(r"\.post\d+$", "", version)


def test_versions_match_the_elm_package() -> None:
    elm = json.loads((CRATE.parent / "elm.json").read_text())["version"]
    cargo = tomllib.loads((CRATE / "Cargo.toml").read_text())["package"]["version"]
    python = tomllib.loads((CRATE / "pyproject.toml").read_text())["project"]["version"]
    assert release(python) == cargo == elm
    assert elm_bible_rs.__version__ == cargo
