## Setup
library(lubridate)
library(noacsr)
noacsr::source_all_functions()

## Attach global variables
attach(global_variables())

## Conduct meta-analysis
meta.analysis <- conduct_meta_analysis()
attach(meta.analysis)
