#' Relative benchmark metric (Tables 4a-4g)
#'
#' Places a one-day diet in one of ten decile bands of a global reference
#' distribution of per capita daily dietary environmental impacts.
#'
#' @section What the comparison is against:
#' This does **not** rank respondents against each other in your survey. Each
#' diet is compared against a fixed external reference distribution that is the
#' same for every analysis anywhere in the world. Intake built it by taking
#' per-country average dietary impact per person per day at each decile from
#' the Planet-Based Diets Database, population-weighting to a global percentile
#' distribution using 2010 country populations, and scaling the result to each
#' demographic group by its share of average energy requirements.
#'
#' The practical consequence: **do not compute this with `quantile()`,
#' `cut()` or `ntile()`**. Those rank your sample against itself and will place
#' roughly 10% of observations in every band by construction. That is a
#' different statistic with no cross-country meaning. The cut-points come from
#' the published tables, not from your data.
#'
#' If your whole sample has low impacts, your whole sample lands in low bands.
#' That is intended, and is the reason the metric is comparable across
#' countries and over time.
#'
#' @section A low band is not evidence of sustainability:
#' The reference distribution describes diets as they are, not as they would
#' need to be. For greenhouse gas emissions the Table 3 absolute
#' planetary-boundary threshold lies *below* the 10th percentile of the
#' reference distribution, so a diet in band 1 still exceeds the planetary
#' boundary. For water use the threshold sits *above* the 90th percentile, so
#' essentially every diet passes. Report the two metric families together and
#' never one as a proxy for the other.
#'
#' @section Band definitions:
#' \tabular{rl}{
#'    1 \tab impact < P10         \cr
#'    2 \tab P10 <= impact < P20  \cr
#'  ... \tab ...                  \cr
#'    9 \tab P80 <= impact < P90  \cr
#'   10 \tab impact >= P90
#' }
#' The inequality is strict, so a diet exactly on P10 belongs in band 2.
#'
#' @section Reporting a mean band:
#' The paper also reports a mean position. Use `midpoint = TRUE` to get band
#' midpoints (5, 15, ... 95) for that purpose, but note the limitation:
#' averaging an ordinal band index treats the bands as equally spaced on the
#' impact scale, and they are not. In Table 4c the P10-P20 gap is 0.002
#' m2*year while the P80-P90 gap is 1.5 m2*year. A mean band is a summary of
#' *position in the reference distribution*; it is not a summary of impact. For
#' a mean impact, average the impact variable itself.
#'
#' @param value Numeric vector of per capita daily impact.
#' @param group Integer vector of demographic groups 1-8, from
#'   [eiof_age_sex_group()].
#' @param indicator One of the seven indicator names; see
#'   [eiof_list_indicators()].
#' @param midpoint If `TRUE`, return the band midpoint percentile (5, 15, ...
#'   95) instead of the band number. Default `FALSE`.
#'
#' @return An integer vector of bands 1-10, or of midpoints if
#'   `midpoint = TRUE`. `NA` where the impact or the group is missing.
#'
#' @seealso [eiof_absolute_threshold()], [eiof_low_impact()]
#'
#' @examples
#' data(eiof_demo)
#' grp <- eiof_age_sex_group(eiof_demo$age, eiof_demo$sex)
#'
#' band <- eiof_relative_band(eiof_demo$biodiv, grp, "biodiv")
#' table(band, useNA = "ifany")
#'
#' # mean position in the global reference distribution
#' mid <- eiof_relative_band(eiof_demo$biodiv, grp, "biodiv", midpoint = TRUE)
#' mean(mid, na.rm = TRUE)
#'
#' @export
eiof_relative_band <- function(value, group, indicator, midpoint = FALSE) {

  .eiof_check_indicator(indicator)
  .eiof_check_value(value)
  .eiof_check_group(group, n = length(value))
  if (!is.logical(midpoint) || length(midpoint) != 1L || is.na(midpoint)) {
    stop("`midpoint` must be TRUE or FALSE.", call. = FALSE)
  }

  cp <- .eiof_rel_cutpoints(indicator)   # 8 x 9
  out <- rep(NA_integer_, length(value))

  for (g in 1:8) {
    idx <- which(!is.na(group) & group == g & !is.na(value))
    if (!length(idx)) next
    # band = 1 + (number of cut-points the value equals or exceeds)
    out[idx] <- 1L + as.integer(colSums(outer(cp[g, ], value[idx], "<=")))
  }

  if (midpoint) out <- out * 10L - 5L
  out
}
