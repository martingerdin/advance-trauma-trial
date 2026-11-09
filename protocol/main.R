## Setup
library(lubridate)
library(noacsr)
library(dotenv)
library(gt)
noacsr::source_all_functions()

## Attach global variables
attach(global_variables())

## Pool all eligible ATLS studies, including those excluded from the
## published meta-analysis because of their SIGN quality rating.
meta.analysis <- conduct_meta_analysis(programme = "ATLS", include.low.quality = TRUE)
attach(meta.analysis)

## Get pilot study results
preliminary.results <- get_preliminary_results(use.saved = TRUE)
attach(preliminary.results)
