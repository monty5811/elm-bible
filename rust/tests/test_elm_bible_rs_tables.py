"""src/tables.rs is generated from the Elm source in ../src; fail if someone edited one side only."""

import subprocess
import sys
from pathlib import Path

SCRIPT = Path(__file__).resolve().parent.parent / "scripts" / "gen_tables.py"


def test_tables_match_the_elm_source() -> None:
    result = subprocess.run([sys.executable, str(SCRIPT), "--check"], check=False, capture_output=True, text=True)
    assert result.returncode == 0, result.stderr
