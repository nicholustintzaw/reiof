#' Absolute threshold metric (Table 3)
#'
#' Codes a one-day diet `1` if its per capita environmental impact falls below
#' the demographic group's share of the per capita daily planetary boundary,
#' and `0` if it does not.
#'
#' @section Interpretation:
#' "x% of one-day diets fall below the absolute per capita daily threshold
#' established for \[indicator\]."
#'
#' This is a pass/fail against a planetary boundary share. It says nothing
#' about how the diet compares with other diets; for that, see
#' [eiof_relative_band()]. The two metric families answer different questions
#' and will not agree.
#'
#' @section The comparison is strict:
#' The published cut-offs carry a `<` sign, so a diet sitting exactly on the
#' threshold is *not* below it and is coded `0`.
#'
#' @section Missing values:
#' A missing impact returns `NA`, not `0`. This matters more than it looks: in
#' R, `NA < threshold` is `NA`, but a careless implementation using
#' `ifelse(value < threshold, 1, 0)` on a comparison that has already been
#' coerced can turn missing data into an apparent threshold breach. Here the
#' guard is explicit.
#'
#' @param value Numeric vector of per capita daily impact, already summed
#'   across everything the respondent consumed that day, in the unit shown by
#'   [eiof_list_indicators()].
#' @param group Integer vector of demographic groups 1-8, from
#'   [eiof_age_sex_group()].
#' @param indicator One of `"landtotal"`, `"landarable"`, `"landpasture"`,
#'   `"ghg"`, `"eutroph"`, `"water"`, `"biodiv"`.
#'
#' @return An integer vector: `1` below the threshold, `0` at or above it, `NA`
#'   where the impact or the group is missing.
#'
#' @seealso [eiof_relative_band()], [eiof_lookup_cutoff()],
#'   [eiof_check_data()]
#'
#' @examples
#' data(eiof_demo)
#' grp <- eiof_age_sex_group(eiof_demo$age, eiof_demo$sex)
#'
#' ghg_ok <- eiof_absolute_threshold(eiof_demo$ghg, grp, "ghg")
#' table(ghg_ok, useNA = "ifany")
#'
#' # all seven indicators
#' for (ind in eiof_indicators$indicator) {
#'   x <- eiof_absolute_threshold(eiof_demo[[ind]], grp, ind)
#'   cat(sprintf("%-12s %5.1f%% below threshold\n", ind, 100 * mean(x, na.rm = TRUE)))
#' }
#'
#' @export
eiof_absolute_threshold <- function(value, group, indicator) {

  .eiof_check_indicator(indicator)
  .eiof_check_value(value)
  .eiof_check_group(group, n = length(value))

  thr <- .eiof_abs_thresholds(indicator)
  out <- rep(NA_integer_, length(value))

  for (g in 1:8) {
    idx <- which(!is.na(group) & group == g & !is.na(value))
    if (length(idx)) {
      out[idx] <- as.integer(value[idx] < thr[g])
    }
  }
  out
}
