#!/usr/bin/env python3
"""Convert a meeting Word doc (.docx) to website-ready content.md via pandoc.

Pandoc's GFM writer falls back to HTML tables when a cell has block content
(paragraphs / lists), which is common in these Word notes. There is no flag
that forces pipe tables in one shot without losing those tables.

Workaround (two pandoc passes):
  1. docx → pandoc markdown without raw HTML  → grid / multiline tables
  2. that markdown → gfm                      → pipe tables

Examples:
  ./docx-to-content-md.py trial-team/20260918/"ATLS Weekly Updates 18 SEP.docx"
  ./docx-to-content-md.py path/to/notes.docx -o path/to/content.md \\
      --title "Meeting notes — Trial team — 18 September 2026"
"""
from __future__ import annotations

import argparse
import re
import shutil
import subprocess
import sys
from pathlib import Path


SECTION_TITLES = {
    "batch ii: patient inclusion": "Batch 2 patient inclusion",
    "batch ii data": "Batch 2 data",
    "batch iii progress": "Batch 3 progress",
    "decisions and actions": "Decisions and actions",
    "weekly study updates": "Trial team",
}


def require_pandoc() -> None:
    if not shutil.which("pandoc"):
        raise SystemExit("pandoc is required but was not found on PATH")


def pandoc(args: list[str], stdin: str | None = None) -> str:
    result = subprocess.run(
        ["pandoc", *args],
        input=stdin,
        check=True,
        capture_output=True,
        text=True,
    )
    return result.stdout


def docx_to_gfm(docx: Path) -> str:
    """Two-pass conversion so tables become GFM pipes, not HTML."""
    require_pandoc()
    intermediate = pandoc(
        [str(docx), "-t", "markdown-raw_html", "--wrap=none", "--columns=9999"]
    )
    return pandoc(
        ["-f", "markdown", "-t", "gfm", "--wrap=none", "--columns=9999"],
        stdin=intermediate,
    )


def pretty_date(raw: str) -> str | None:
    m = re.match(r"(\d{1,2})\s*([A-Za-z]{3,9})\.?\s*(\d{2,4})", raw.strip())
    if not m:
        return None
    day = int(m.group(1))
    mon = m.group(2)[:3].title()
    year = int(m.group(3))
    if year < 100:
        year += 2000
    months = {
        "Jan": "January",
        "Feb": "February",
        "Mar": "March",
        "Apr": "April",
        "May": "May",
        "Jun": "June",
        "Jul": "July",
        "Aug": "August",
        "Sep": "September",
        "Oct": "October",
        "Nov": "November",
        "Dec": "December",
    }
    return f"{day} {months.get(mon, m.group(2).title())} {year}"


def infer_title(md: str, override: str | None, source: Path) -> str:
    if override:
        return override
    m = re.search(r"^#\s+(.+)$", md, re.M)
    if not m:
        return f"Meeting notes — {source.stem}"
    raw = m.group(1)
    raw = raw.replace("\\[", "[").replace("\\]", "]")
    parts = re.findall(r"\[([^]]+)\]", raw)
    if len(parts) >= 2:
        meeting, date = parts[0], parts[1]
    elif len(parts) == 1:
        meeting, date = parts[0], ""
    else:
        return f"Meeting notes — {raw.strip()}"
    meeting = SECTION_TITLES.get(meeting.strip().lower(), meeting.strip())
    if meeting.lower() in {"tt", "trial team", "weekly study updates"}:
        meeting = "Trial team"
    if meeting.lower() in {"tmg", "tmg meeting"}:
        meeting = "TMG"
    date = pretty_date(date) or date.strip()
    return f"Meeting notes — {meeting} — {date}" if date else f"Meeting notes — {meeting}"


def clean_heading_text(text: str) -> str:
    text = text.replace("\\[", "").replace("\\]", "")
    text = text.strip().strip("[]")
    text = re.sub(r"^\d+\.\s*", "", text)
    return SECTION_TITLES.get(text.lower(), text)


def polish(md: str, title: str) -> str:
    lines = md.splitlines()
    out: list[str] = [f"# {title}", ""]
    section_no = 0
    saw_h1 = False

    for line in lines:
        if line.startswith("# ") and not saw_h1:
            saw_h1 = True
            continue
        if line.startswith("## "):
            section_no += 1
            text = clean_heading_text(line[3:])
            out.append(f"## {section_no}. {text}")
            continue
        # Drop heading attribute junk if present.
        line = re.sub(r"\s*\{#[^}]+\}\s*$", "", line)
        # Prefer en dash in period headers like Sep-Oct.
        if line.startswith("|"):
            line = re.sub(r"\b([A-Za-z]{3})-([A-Za-z]{3})\b", r"\1–\2", line)
            line = line.replace("(N= ", "(N = ").replace("(N=", "(N = ")
            line = re.sub(r"Total\s*\((\d+)\)", r"Total (N = \1)", line)
            line = line.replace("SpO2", "SpO₂").replace("Variables", "Variable")
        out.append(line)

    text = "\n".join(out).rstrip() + "\n"
    text = re.sub(r"\n{3,}", "\n\n", text)
    return text


def convert(docx: Path, title: str | None = None) -> str:
    md = docx_to_gfm(docx)
    resolved = infer_title(md, title, docx)
    return polish(md, resolved)


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("docx", type=Path, help="Source .docx meeting notes")
    parser.add_argument(
        "-o",
        "--output",
        type=Path,
        help="Output markdown path (default: <docx-dir>/content.md)",
    )
    parser.add_argument(
        "--title",
        help='Override document title, e.g. "Meeting notes — Trial team — 18 September 2026"',
    )
    args = parser.parse_args(argv)

    docx = args.docx.expanduser().resolve()
    if not docx.is_file():
        print(f"error: file not found: {docx}", file=sys.stderr)
        return 1

    md = convert(docx, title=args.title)
    out = args.output.expanduser().resolve() if args.output else docx.parent / "content.md"
    out.write_text(md, encoding="utf-8")
    print(f"Wrote {out}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
