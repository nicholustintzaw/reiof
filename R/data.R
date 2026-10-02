#' Published cut-offs for every dietary environmental impact metric
#'
#' Every cut-off in Tables 3 and 4a-4g of the Intake methods paper, in long
#' format: one row per metric, indicator, demographic group and table row.
#'
#' Tables 5a and 5b are deliberately **not** stored here. They are composites
#' that reuse values already present - the HESD criterion is
#' `metric == "abs" & indicator == "biodiv"`, and the HDLEI criterion is
#' `metric == "rel" & indicator == "biodiv" & percentile == 30`. A second copy
#' could drift from the first.
#'
#' @section The eutrophication rescaling:
#' `value` equals `value_published` everywhere except the relative benchmarks
#' for eutrophication, where `scale_to_db_unit` is 0.001.
#'
#' The paper states that eutrophication potential is measured in kg PO4(3-)eq.
#' The Table 3 absolute threshold is consistent with that, at 0.0077 to 0.0267.
#' The Table 4f relative benchmarks for the same quantity are 5.3 to 48.7,
#' about a thousand times larger. Read as grams, Table 4f places the absolute
#' threshold between the 40th and 50th percentile of the global reference
#' distribution, which is where the total-land and pasture-land thresholds also
#' sit. Read as kilograms, every diet on earth would fall below the 10th
#' percentile, which is not a coherent reading of a percentile distribution.
#'
#' This is an inference from internal consistency, **not a documented
#' conversion**. Confirm with Intake before publishing any eutrophication
#' result. Every other indicator reconciles between the two tables without
#' adjustment.
#'
#' @format A data frame with 616 rows and 15 columns:
#' \describe{
#'   \item{metric}{`"abs"` for Table 3, `"rel"` for Tables 4a-4g}
#'   \item{table}{Source table: `"3"`, `"4a"` ... `"4g"`}
#'   \item{indicator}{Short indicator name used by the `eiof_*` functions}
#'   \item{indicator_label}{Human-readable indicator name}
#'   \item{unit}{Unit of `value`, per capita per day}
#'   \item{demogroup}{Demographic group, 1-8}
#'   \item{demogroup_label}{Group name, e.g. "Male 10+ yr"}
#'   \item{sex}{Factor, `"male"` or `"female"`}
#'   \item{age_min, age_max}{Age bounds in years; 999 means no upper bound}
#'   \item{row_order}{Position in the source table}
#'   \item{percentile}{10-90 for relative rows 1-9; `NA` for absolute rows and
#'     for row 10, which repeats P90 as a lower bound}
#'   \item{value}{**The cut-off the metric functions compare against**}
#'   \item{value_published}{The value exactly as printed in the paper}
#'   \item{scale_to_db_unit}{Multiplier linking the two; 1 except as described
#'     above}
#' }
#'
#' @source Intake-EIOD-Methods-Paper-Jan2026-Final.pdf, Tables 3 and 4a-4g
#'   (pages 16-19). Transcribed programmatically; see `data-raw/build_data.R`.
#'   The same source file drives a companion Stata implementation.
#'
#' @examples
#' data(eiof_cutoffs)
#' str(eiof_cutoffs)
#'
#' # the HESD biodiversity criterion
#' subset(eiof_cutoffs, metric == "abs" & indicator == "biodiv",
#'        c(demogroup_label, value))
"eiof_cutoffs"


#' The seven environmental impact indicators
#'
#' Indicator names accepted by the `eiof_*` functions, with their labels and
#' the units your impact values must be in.
#'
#' @format A data frame with 7 rows and 3 columns:
#' \describe{
#'   \item{indicator}{Short name, as passed to the `eiof_*` functions}
#'   \item{indicator_label}{Human-readable name}
#'   \item{unit}{Required unit, per capita per day}
#' }
#'
#' @examples
#' data(eiof_indicators)
#' eiof_indicators
"eiof_indicators"


#' Synthetic respondent-day data for examples
#'
#' Six hundred simulated respondent-days with age, sex, the seven impact totals
#' and a GDQS score, for use in examples, vignettes and tests.
#'
#' **These are not real survey data.** The values were drawn from lognormal
#' distributions chosen to be roughly plausible. No prevalence computed from
#' this dataset carries any substantive meaning.
#'
#' A small amount of missingness is included in `biodiv` and `gdqs` so examples
#' show how it propagates.
#'
#' @format A data frame with 600 rows and 11 columns:
#' \describe{
#'   \item{id}{Row identifier}
#'   \item{age}{Age in years, 0.5 to 65}
#'   \item{sex}{Factor with levels `"male"` and `"female"`}
#'   \item{landtotal, landarable, landpasture}{Land use, m2*year}
#'   \item{ghg}{Greenhouse gas emissions, kg CO2eq}
#'   \item{eutroph}{Eutrophication potential, kg PO4(3-)eq}
#'   \item{water}{Water use, litres}
#'   \item{biodiv}{Biodiversity loss, PDF*year}
#'   \item{gdqs}{Global Diet Quality Score}
#' }
#'
#' @seealso A deliberately broken copy ships as
#'   `system.file("extdata", "eiof_broken.csv", package = "reiof")` and is used
#'   in `vignette("troubleshooting")`.
#'
#' @examples
#' data(eiof_demo)
#' head(eiof_demo)
"eiof_demo"
