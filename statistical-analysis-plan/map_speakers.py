#!/usr/bin/env python3
"""Map chunk-based speaker labels (A#00min, B#12min, ...) to real names.

Speaker labels restart in every 12-minute chunk, so the mapping is per chunk.
Derived from the self-introductions at the start of the meeting and content
cues (the triage doc attributes specific comments to KH/JK/JM/AO).
"""
import pathlib
import sys

# label -> name per chunk suffix
MAPPING = {
    "00min": {"A": "Karla", "B": "Martin", "C": "Unknown", "D": "Arpita", "E": "Jess", "F": "James"},
    "12min": {"A": "James", "B": "James", "C": "Jess", "D": "Martin", "E": "Karla", "F": "Karla", "G": "Arpita"},
    "24min": {"A": "Martin", "B": "Jess", "C": "Karla", "D": "James"},
    "36min": {"A": "Jess", "B": "Karla", "C": "Unknown", "D": "Martin", "E": "James"},
    "48min": {"A": "Martin", "B": "Jess", "C": "Karla", "D": "James"},
    "60min": {"A": "Martin", "B": "Jess"},
}

HEADER = """\
# Diarized transcript — SAP finalisation meeting, 2026-11-10
# Source: audio1595844097.m4a (trimmed: leading 12:15 of silence removed)
# Speakers: Martin (chair), Karla (KH), Jess (JK), James (JM), Arpita (guest, George Institute India)
# Note: labels restart per 12-min chunk; a few brief interjections (marked "Unknown") could not be attributed reliably.
# Spelling/ASR fixes applied: Peter/Al-Pita/Orpita/Avita/Evita -> Arpita; Carla -> Karla;
#   cholesterol->cluster; step-wide/stepwise/stepped wedge -> stepped-wedge; Georgia->George Institute.
"""

# (old, new) applied to the spoken text, in order
CORRECTIONS = [
    ("Hi, I'm Arpita", "Hi, I'm Karla"),  # Karla's garbled self-intro (speaker is Karla per diarization anchor)
    ("Peter", "Arpita"),
    ("Al-Pita", "Arpita"),
    ("Orpita", "Arpita"),
    ("Avita", "Arpita"),
    ("Evita", "Arpita"),
    ("Carla", "Karla"),
    ("Georgia Institute for Global Health", "George Institute for Global Health"),
    ("Georgia Institute", "George Institute"),
    ("cholesterol", "cluster"),
    ("step-wide", "stepped-wedge"),
    ("stepwise", "stepped-wedge"),
    ("stepped wedge", "stepped-wedge"),
]


def main():
    src = pathlib.Path(sys.argv[1])
    dst = pathlib.Path(sys.argv[2]) if len(sys.argv) > 2 else src.with_name(src.stem + ".named.txt")
    lines = [HEADER]
    for line in src.read_text(encoding="utf-8").splitlines():
        if not line.strip():
            continue
        # format: [HH:MM:SS] A#00min: text
        if "] " in line:
            ts, rest = line.split("] ", 1)
            ts = ts.lstrip("[")
            label, text = rest.split(": ", 1)
            chunk = label.split("#")[-1]
            name = MAPPING.get(chunk, {}).get(label.split("#")[0], label)
            for old, new in CORRECTIONS:
                text = text.replace(old, new)
            lines.append(f"[{ts}] {name}: {text}")
        else:
            lines.append(line)
    dst.write_text("\n".join(lines) + "\n", encoding="utf-8")
    print(f"Wrote {dst}")


if __name__ == "__main__":
    main()