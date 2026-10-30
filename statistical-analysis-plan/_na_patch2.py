#!/usr/bin/env python3
"""Fix NA normalisation for tibble in sdmc_report_prepared_data.R."""
import sys

PATH = "/Users/martin/Documents/advance-trauma-trial/functions/sdmc_report_prepared_data.R"

with open(PATH, encoding="utf-8") as fh:
    text = fh.read()

old = """    ## readr parses empty cells as NA; normalise to "" so blank cells stay blank.
    csv[is.na(csv)] <- "" """
new = """    ## readr parses empty cells as NA; normalise to "" so blank cells stay blank.
    csv[] <- lapply(csv, function(x) {
        x[is.na(x)] <- ""
        x
    })"""

n = text.count(old)
if n != 1:
    print(f"PATTERN NOT FOUND (expected 1, got {n})")
    sys.exit(1)
text = text.replace(old, new)

with open(PATH, "w", encoding="utf-8") as fh:
    fh.write(text)
print("NA normalisation fixed")