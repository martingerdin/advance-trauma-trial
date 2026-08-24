#!/usr/bin/env python3
"""Remove Quarto's bulky default webfonts from a self-contained Reveal HTML.

Keeps small brand WOFF2 embeds (Quicksand / EB Garamond / Roboto) and strips
large default theme fonts (Source Sans Pro, News Cycle, Lato, etc.).
"""
from __future__ import annotations

import re
import sys
from pathlib import Path

# Drop Quarto theme font packs; keep compact WOFF2 brand fonts.
MAX_DATA_LINK_BYTES = 80_000  # ~80 KB
MAX_FONT_DATA_BYTES = 80_000


def slim(html: str) -> str:
    def drop_large_data_link(match: re.Match[str]) -> str:
        tag = match.group(0)
        lower = tag.lower()
        if len(tag) >= MAX_DATA_LINK_BYTES and "data:" in lower:
            return ""
        # Also drop font CSS packs that mention Quarto defaults even if smaller
        if "data:text/css" in lower and any(
            name in tag
            for name in (
                "Source%20Sans",
                "Source Sans",
                "News%20Cycle",
                "News Cycle",
                "Lato",
            )
        ):
            return ""
        return tag

    html = re.sub(
        r"<link\b[^>]*\bhref=\"data:[^\"]+\"[^>]*>\s*",
        drop_large_data_link,
        html,
        flags=re.IGNORECASE,
    )

    def replace_font_url(match: re.Match[str]) -> str:
        blob = match.group(0)
        if len(blob) >= MAX_FONT_DATA_BYTES:
            return "local('Arial')"
        # Keep compact woff2 data URIs
        if "data:font/woff2" in blob.lower() or "data:application/font-woff2" in blob.lower():
            return blob
        if len(blob) >= 20_000:
            return "local('Arial')"
        return blob

    html = re.sub(
        r"url\(\s*['\"]?data:font/[^)]+\)",
        replace_font_url,
        html,
        flags=re.IGNORECASE,
    )

    # Point leftover Quarto theme font names at our stacks
    replacements = {
        '"Source Sans Pro"': '"Roboto", system-ui, sans-serif',
        "'Source Sans Pro'": '"Roboto", system-ui, sans-serif',
        '"News Cycle"': '"Quicksand", system-ui, sans-serif',
        "'News Cycle'": '"Quicksand", system-ui, sans-serif',
        '"Lato"': '"Roboto", system-ui, sans-serif',
        "'Lato'": '"Roboto", system-ui, sans-serif',
    }
    for old, new in replacements.items():
        html = html.replace(old, new)

    return html


def main() -> None:
    path = Path(sys.argv[1] if len(sys.argv) > 1 else "presentation.html")
    before = path.stat().st_size
    path.write_text(slim(path.read_text(encoding="utf-8")), encoding="utf-8")
    after = path.stat().st_size
    print(f"{path}: {before / 1024 / 1024:.2f} MB -> {after / 1024 / 1024:.2f} MB")


if __name__ == "__main__":
    main()
