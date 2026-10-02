#' Healthy diet criterion
#'
#' The diet quality half of both composite metrics: a Global Diet Quality Score
#' of 23 or more. Exported on its own so it can be reported separately from the
#' composites.
#'
#' @param gdqs Numeric vector of Global Diet Quality Score values.
#' @param cutoff The score at or above which a diet counts as healthy. Default
#'   23, as published. Change only with a documented reason.
#'
#' @return An integer vector: `1` healthy, `0` not, `NA` where `gdqs` is
#'   missing.
#'
#' @examples
#' eiof_healthy_diet(c(10, 22.9, 23, 30, NA))
#'
#' @export
eiof_healthy_diet <- function(gdqs, cutoff = 23) {
  if (is.factor(gdqs)) {
    stop("`gdqs` is a factor. Convert with as.numeric(as.character(gdqs)).",
         call. = FALSE)
  }
  if (!is.numeric(gdqs)) stop("`gdqs` must be numeric.", call. = FALSE)
  if (!is.numeric(cutoff) || length(cutoff) != 1L) {
    stop("`cutoff` must be a single number.", call. = FALSE)
  }
  out <- rep(NA_integer_, length(gdqs))
  ok <- !is.na(gdqs)
  out[ok] <- as.integer(gdqs[ok] >= cutoff)
  out
}


#' Environmentally sustainable diet criterion
#'
#' The environmental half of the HESD metric: biodiversity loss below the
#' Table 3 **absolute** threshold. Exported on its own so it can be reported
#' separately from the composite.
#'
#' Biodiversity loss is used as the proxy for total environmental impact,
#' following the methods paper.
#'
#' @param biodiversity Numeric vector of per capita daily biodiversity loss
#'   (PDF*year).
#' @param group Integer vector of demographic groups 1-8.
#'
#' @return An integer vector: `1` sustainable, `0` not, `NA` where missing.
#'
#' @seealso [eiof_low_impact()], the stricter relative criterion used by HDLEI.
#'
#' @examples
#' data(eiof_demo)
#' grp <- eiof_age_sex_group(eiof_demo$age, eiof_demo$sex)
#' table(eiof_sustainable_diet(eiof_demo$biodiv, grp), useNA = "ifany")
#'
#' @export
eiof_sustainable_diet <- function(biodiversity, group) {
  # Delegates, so there is exactly one implementation of the Table 3
  # comparison in the package.
  eiof_absolute_threshold(value = biodiversity, group = group,
                          indicator = "biodiv")
}


#' Low environmental impact criterion
#'
#' The environmental half of the HDLEI metric: biodiversity loss below the
#' 30th percentile of the Table 4g **relative** benchmark distribution.
#' Exported on its own so it can be reported separately from the composite.
#'
#' @section Relationship to the HESD criterion:
#' This criterion is uniformly stricter than [eiof_sustainable_diet()]. For
#' biodiversity the relative P30 cut-off sits below the absolute threshold in
#' every one of the eight demographic groups, by the same constant factor of
#' about 1.52, because both are the same all-population figure scaled by the
#' same energy-requirement weight. So a diet meeting this criterion necessarily
#' meets the HESD one, and HESD prevalence can never fall below HDLEI
#' prevalence. The two are nested, not independent.
#'
#' @param biodiversity Numeric vector of per capita daily biodiversity loss.
#' @param group Integer vector of demographic groups 1-8.
#' @param percentile The relative cut-off. Default 30, as published. Any other
#'   value is a sensitivity analysis, not the HDLEI metric, and is labelled as
#'   such by a message.
#'
#' @return An integer vector: `1` low impact, `0` not, `NA` where missing.
#'
#' @examples
#' data(eiof_demo)
#' grp <- eiof_age_sex_group(eiof_demo$age, eiof_demo$sex)
#' table(eiof_low_impact(eiof_demo$biodiv, grp), useNA = "ifany")
#'
#' @export
eiof_low_impact <- function(biodiversity, group, percentile = 30) {
  if (!is.numeric(percentile) || length(percentile) != 1L ||
      is.na(percentile) || percentile %% 10 != 0 ||
      percentile < 10 || percentile > 90) {
    stop("`percentile` must be a multiple of 10 between 10 and 90. ",
         "The published cut-points are deciles only.", call. = FALSE)
  }
  if (percentile != 30) {
    message("percentile = ", percentile, " is not the published HDLEI ",
            "criterion (30). Label any result from this as a sensitivity ",
            "analysis.")
  }
  band <- eiof_relative_band(value = biodiversity, group = group,
                             indicator = "biodiv")
  out <- rep(NA_integer_, length(band))
  ok <- !is.na(band)
  out[ok] <- as.integer(band[ok] <= percentile / 10)
  out
}


# ---------------------------------------------------------------------------- #
# Shared engine for the two composites
# ---------------------------------------------------------------------------- #

#' @noRd
.eiof_composite <- function(healthy, env, group, form, labels, what) {

  # Composites apply only from age 2. Groups 1 and 5 are the under-twos.
  eligible <- !is.na(group) & !(group %in% c(1L, 5L))
  n_under2 <- sum(!is.na(group) & group %in% c(1L, 5L))

  healthy[!eligible] <- NA_integer_
  env[!eligible]     <- NA_integer_

  cat4 <- rep(NA_integer_, length(healthy))
  cat4[healthy == 1 & env == 1] <- 1L
  cat4[healthy == 0 & env == 1] <- 2L
  cat4[healthy == 1 & env == 0] <- 3L
  cat4[healthy == 0 & env == 0] <- 4L

  # the category must be defined exactly when both criteria are
  stopifnot(identical(is.na(cat4), is.na(healthy) | is.na(env)))

  if (n_under2 > 0L) {
    message(n_under2, " observation(s) aged under 2 years set to NA: the GDQS ",
            "is not validated below age 2, so the ", what,
            " metric does not apply to them.")
  }

  switch(form,
    category = factor(cat4, levels = 1:4, labels = labels),
    binary   = {
      out <- rep(NA_integer_, length(cat4))
      out[!is.na(cat4)] <- as.integer(cat4[!is.na(cat4)] == 1L)
      out
    },
    components = data.frame(
      healthy   = healthy,
      env       = env,
      category  = factor(cat4, levels = 1:4, labels = labels),
      binary    = ifelse(is.na(cat4), NA_integer_, as.integer(cat4 == 1L))
    )
  )
}


#' Healthy and Environmentally Sustainable Diet, HESD (Table 5a)
#'
#' Combines a diet quality criterion with an **absolute** environmental
#' sustainability criterion.
#'
#' \itemize{
#'   \item healthy: GDQS >= 23
#'   \item environmentally sustainable: biodiversity loss below the Table 3
#'     absolute threshold
#' }
#'
#' Both criteria are also available on their own, as
#' [eiof_healthy_diet()] and [eiof_sustainable_diet()]. This function calls
#' those two rather than re-implementing them, so the sub-indicators and the
#' composite cannot drift apart.
#'
#' @section Age restriction:
#' The methods paper restricts the composite metrics to respondents aged 2
#' years and older, because the GDQS has not been validated below that age.
#' Table 5a prints six demographic columns, not eight. Results are therefore
#' `NA` for groups 1 and 5, and a message reports how many observations that
#' affects. If your sample includes under-twos, decide explicitly whether they
#' belong in the analysis denominator at all.
#'
#' @section Relationship to HDLEI:
#' HESD and [eiof_hdlei()] are nested, not independent: every HDLEI diet is
#' also a HESD diet. See [eiof_low_impact()] for why. Do not present them as
#' two dimensions of a 2x2.
#'
#' @param biodiversity Numeric vector of per capita daily biodiversity loss.
#' @param gdqs Numeric vector of Global Diet Quality Score values.
#' @param group Integer vector of demographic groups 1-8.
#' @param form What to return. `"category"` (default) gives the four categories
#'   of Table 5a as a factor; `"binary"` gives 1 for category 1 and 0
#'   otherwise; `"components"` gives a data frame with both sub-indicators and
#'   both combined forms.
#' @param gdqs_cutoff GDQS threshold, default 23.
#'
#' @return A factor, integer vector, or data frame, depending on `form`.
#'
#' @examples
#' data(eiof_demo)
#' grp <- eiof_age_sex_group(eiof_demo$age, eiof_demo$sex)
#'
#' table(eiof_hesd(eiof_demo$biodiv, eiof_demo$gdqs, grp))
#'
#' # everything at once
#' parts <- eiof_hesd(eiof_demo$biodiv, eiof_demo$gdqs, grp, form = "components")
#' head(parts)
#'
#' @export
eiof_hesd <- function(biodiversity, gdqs, group,
                      form = c("category", "binary", "components"),
                      gdqs_cutoff = 23) {

  form <- match.arg(form)
  if (length(biodiversity) != length(gdqs)) {
    stop("`biodiversity` and `gdqs` must be the same length.", call. = FALSE)
  }
  .eiof_check_group(group, n = length(biodiversity))

  .eiof_composite(
    healthy = eiof_healthy_diet(gdqs, cutoff = gdqs_cutoff),
    env     = eiof_sustainable_diet(biodiversity, group),
    group   = group,
    form    = form,
    labels  = c("Healthy and env. sustainable",
                "Not healthy but env. sustainable",
                "Healthy but not env. sustainable",
                "Not healthy and not env. sustainable"),
    what    = "HESD"
  )
}


#' Healthy Diet with Low Environmental Impacts, HDLEI (Table 5b)
#'
#' Combines the same diet quality criterion as [eiof_hesd()] with a
#' **relative** low-impact criterion.
#'
#' \itemize{
#'   \item healthy: GDQS >= 23
#'   \item low environmental impacts: biodiversity loss below the 30th
#'     percentile of the Table 4g relative benchmark distribution
#' }
#'
#' The diet quality criterion is identical to HESD's. The only difference is
#' the environmental one: HESD tests biodiversity against the absolute
#' planetary boundary share, HDLEI against a position in the global reference
#' distribution.
#'
#' @section These two metrics are nested:
#' The HDLEI environmental cut-off is uniformly about 1.52 times stricter than
#' the HESD one, in every demographic group, because both are the same
#' all-population figure scaled by the same weight. So `eiof_hdlei(...) == 1`
#' implies `eiof_hesd(...) == 1`, and HESD prevalence can never fall below
#' HDLEI prevalence. Reporting both is still informative: the gap between them
#' is the share of diets sitting between the two cut-offs. But they are not two
#' independent dimensions.
#'
#' @section Age restriction:
#' As for HESD, results are `NA` for respondents under 2 years.
#'
#' @param biodiversity Numeric vector of per capita daily biodiversity loss.
#' @param gdqs Numeric vector of Global Diet Quality Score values.
#' @param group Integer vector of demographic groups 1-8.
#' @param form `"category"`, `"binary"` or `"components"`; see [eiof_hesd()].
#' @param gdqs_cutoff GDQS threshold, default 23.
#' @param percentile Relative cut-off, default 30 as published.
#'
#' @return A factor, integer vector, or data frame, depending on `form`.
#'
#' @examples
#' data(eiof_demo)
#' grp <- eiof_age_sex_group(eiof_demo$age, eiof_demo$sex)
#'
#' table(eiof_hdlei(eiof_demo$biodiv, eiof_demo$gdqs, grp))
#'
#' # the nesting: every HDLEI diet is also a HESD diet
#' a <- eiof_hesd(eiof_demo$biodiv, eiof_demo$gdqs, grp, form = "binary")
#' b <- eiof_hdlei(eiof_demo$biodiv, eiof_demo$gdqs, grp, form = "binary")
#' table(HESD = a, HDLEI = b, useNA = "ifany")
#'
#' @export
eiof_hdlei <- function(biodiversity, gdqs, group,
                       form = c("category", "binary", "components"),
                       gdqs_cutoff = 23, percentile = 30) {

  form <- match.arg(form)
  if (length(biodiversity) != length(gdqs)) {
    stop("`biodiversity` and `gdqs` must be the same length.", call. = FALSE)
  }
  .eiof_check_group(group, n = length(biodiversity))

  .eiof_composite(
    healthy = eiof_healthy_diet(gdqs, cutoff = gdqs_cutoff),
    env     = eiof_low_impact(biodiversity, group, percentile = percentile),
    group   = group,
    form    = form,
    labels  = c("Healthy with low env. impacts",
                "Not healthy but low env. impacts",
                "Healthy but not low env. impacts",
                "Not healthy and not low env. impacts"),
    what    = "HDLEI"
  )
}
