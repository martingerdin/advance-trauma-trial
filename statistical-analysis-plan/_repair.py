#!/usr/bin/env python3
"""Repair the broken NA normalisation lines in sdmc_report_prepared_data.R."""
import sys

PATH = "/Users/martin/Documents/advance-trauma-trial/functions/sdmc_report_prepared_data.R"

with open(PATH, encoding="utf-8") as fh:
    text = fh.read()

old = """    ## readr parses empty cells as NA; normalise to "" so blank cells stay blank.
    csv[is.na(csv)] <-
    ## Report blank shells unchanged when no stratum values are filled yet."""
new = """    ## readr parses empty cells as NA; normalise to "" so blank cells stay blank.
    csv[] <- lapply(csv, function(x) {
        x[is.na(x)] <- ""
        x
    })
    ## Report blank shells unchanged when no stratum values are filled yet."""

n = text.count(old)
if n != 1:
    print(f"PATTERN NOT FOUND (expected 1, got {n})")
    sys.exit(1)
text = text.replace(old, new)

with open(PATH, "w", encoding="utf-8") as fh:
    fh.write(text)
print("reader repaired")