#' reiof: Dietary Environmental Impact Metrics from the Intake EIOF Framework
#'
#' Calculates the dietary environmental impact metrics defined in Intake's
#' Environmental Impacts of Foods methods paper (January 2026).
#'
#' @section What this package is not:
#' `reiof` **does not contain the Environmental Impacts of Foods database.** It
#' ships the published metric *cut-offs* and applies them to impact values you
#' have already derived. Matching food items to impact factors, applying
#' edible-portion, processing and cooking-yield adjustments, and summing to the
#' respondent-day is upstream work that this package starts after. It also does
#' not calculate the Global Diet Quality Score; the composite metrics take a
#' GDQS value as input.
#'
#' @section The three metric families:
#' \describe{
#'   \item{Absolute threshold (Table 3)}{[eiof_absolute_threshold()] - is the
#'     diet below the per capita daily share of a planetary boundary?}
#'   \item{Relative benchmark (Tables 4a-4g)}{[eiof_relative_band()] - where
#'     does the diet sit in a fixed global reference distribution?}
#'   \item{Composite (Tables 5a, 5b)}{[eiof_hesd()] and [eiof_hdlei()] -
#'     diet quality combined with environmental impact, each also returning its
#'     two criteria as separate sub-indicators.}
#' }
#' All of them need the eight-category demographic group from
#' [eiof_age_sex_group()].
#'
#' @section Check your data first:
#' [eiof_check_data()] covers variable type, plausibility, numerical precision
#' and unit magnitude. Running it first is cheap and catches the failure modes
#' that otherwise produce confidently wrong numbers.
#'
#' @section Three things to know before reporting results:
#' \enumerate{
#'   \item The relative benchmark cut-offs for **eutrophication** carry an
#'     inferred rescaling that Intake has not confirmed. See
#'     `vignette("reiof")`.
#'   \item A **low relative band is not evidence of sustainability**. For
#'     greenhouse gases even band 1 exceeds the planetary boundary; for water
#'     even band 10 is below it.
#'   \item **HDLEI is nested inside HESD** - every HDLEI diet is also a HESD
#'     diet. They are not two independent dimensions.
#' }
#'
#' @section Dependencies:
#' None. `reiof` imports nothing, so it installs with no dependency tree. The
#' consequence is a plain vector API rather than tidy evaluation: pass
#' `df$biodiv`, not `biodiv`.
#'
#' @docType package
#' @name reiof-package
#' @aliases reiof
#' @keywords internal
"_PACKAGE"
