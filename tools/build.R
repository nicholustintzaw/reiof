# ---------------------------------------------------------------------------- #
# tools/build.R - one-time setup and the regular rebuild loop.
#
# RUN THIS FIRST. man/ and NAMESPACE were written by hand because roxygen2 was
# not available in the offline environment where the package was assembled.
# The roxygen comments in R/ are the source of truth; this regenerates the
# documentation from them.
# ---------------------------------------------------------------------------- #

## 1. one-time: install the development tooling
# install.packages(c("devtools", "roxygen2", "pkgdown"))

## 2. regenerate man/ and NAMESPACE from the roxygen comments in R/
# devtools::document()

## 3. rebuild the datasets from data-raw/eiof_cutoffs.csv
#    (only needed if the CSV changes, e.g. if Intake issues a correction)
# source("data-raw/build_data.R")

## 4. test and check
# devtools::test()
# devtools::check()

## 5. build the website locally (CI also builds it on every push to main)
# pkgdown::build_site()
