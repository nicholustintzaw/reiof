# reiof (development version)

First working version. Implements Tables 3, 4a-4g, 5a and 5b of the Intake
Environmental Impacts of Foods methods paper (January 2026).

* `eiof_age_sex_group()` builds the eight-category demographic group that every
  cut-off is scaled by, and `eiof_as_sex_factor()` converts a numeric sex code
  into the recommended factor input.
* `eiof_absolute_threshold()` (Table 3) and `eiof_relative_band()`
  (Tables 4a-4g) for all seven environmental impact indicators.
* `eiof_hesd()` (Table 5a) and `eiof_hdlei()` (Table 5b), each returning the two
  criteria as separate sub-indicators as well as the combined measure.
* `eiof_check_data()` and five component checks covering variable type,
  plausibility, numerical precision and unit magnitude.
* Cut-offs ship as `eiof_cutoffs`, a 616-row long-format dataset built from the
  same source file as the companion Stata implementation.

## Known limitations

* The relative benchmark cut-offs for eutrophication potential carry an
  inferred rescaling. See `vignette("reiof")`, section "A caution on
  eutrophication". Confirm with Intake before publishing eutrophication results.
* `man/` and `NAMESPACE` were written by hand because roxygen2 was not available
  in the environment where the package was first assembled. The roxygen comments
  in `R/` are the source of truth; run `devtools::document()` to regenerate.
