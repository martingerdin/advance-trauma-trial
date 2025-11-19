## Setup
library(lubridate)
library(noacsr)
library(dotenv)
library(gt)
noacsr::source_all_functions()

## Attach global variables
attach(global_variables())

## Conduct meta-analysis
meta.analysis <- conduct_meta_analysis()
attach(meta.analysis)

## Get pilot study results
preliminary.results <- get_preliminary_results(use.saved = TRUE)
attach(preliminary.results)