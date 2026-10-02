# ---------------------------------------------------------------------------- #
# Checks on environmental impact variables.
#
# Unlike the eiof_* metric functions, these never stop() on bad input. Their
# whole job is to report problems, so they must survive the problems they
# report.
# ---------------------------------------------------------------------------- #

#' Build one finding
#' @noRd
.eiof_finding <- function(check, variable, level, n, message) {
  data.frame(check = check, variable = variable, level = level,
             n = as.integer(n), message = message,
             stringsAsFactors = FALSE)
}

#' Build a report object from a list of findings
#' @noRd
.eiof_report <- function(findings, n_obs = NA_integer_, source = NULL) {
  findings <- findings[!vapply(findings, is.null, logical(1))]
  out <- if (length(findings)) do.call(rbind, findings) else
    .eiof_finding(character(0), character(0), character(0),
                  integer(0), character(0))
  rownames(out) <- NULL
  out$level <- factor(out$level, levels = c("error", "warning", "note"))
  out <- out[order(out$level), , drop = FALSE]
  rownames(out) <- NULL
  structure(out, class = c("eiof_check", "data.frame"),
            n_obs = n_obs, source = source)
}


#' Check an environmental impact variable
#'
#' Structural and plausibility checks on one impact vector: storage type, sign,
#' zeros, missingness and non-finite values.
#'
#' @section Why the type checks are errors:
#' Three import failures destroy impact data silently, and all three are common
#' with the very small values biodiversity loss takes:
#'
#' \itemize{
#'   \item A **factor** column. `as.numeric()` on a factor returns the level
#'     codes, so `3.7e-13` becomes `1`. A plausible-looking number with no
#'     warning is far worse than `NA`, because nothing downstream looks wrong.
#'   \item A **character** column, usually caused by one stray cell such as
#'     `"<0.001"` or a decimal comma. Coercion then yields `NA` with a warning
#'     that is easy to miss.
#'   \item An **integer** column. It cannot represent `1e-13` at all; the value
#'     is already `0` by the time you see it.
#' }
#'
#' @param value The impact vector to check. May be any type; that is the point.
#' @param name A label for the variable, used in the report.
#' @param indicator Optionally the indicator name, used to report the expected
#'   unit.
#'
#' @return An object of class `eiof_check`.
#'
#' @examples
#' data(eiof_demo)
#' eiof_check_impact(eiof_demo$biodiv, name = "biodiv")
#'
#' # a factor column, the worst of the three import failures
#' eiof_check_impact(factor(c("3.7e-13", "5.2e-13")), name = "biodiv")
#'
#' @export
eiof_check_impact <- function(value, name = "impact", indicator = NULL) {

  f <- list()
  n <- length(value)

  # ---- type ---------------------------------------------------------------- #
  if (is.factor(value)) {
    f[[length(f) + 1]] <- .eiof_finding(
      "type", name, "error", n,
      paste0("column is a factor. as.numeric() on a factor returns level ",
             "codes, not values, so this fails silently. Fix with ",
             "as.numeric(as.character(x))."))
    return(.eiof_report(f, n_obs = n))
  }
  if (is.character(value)) {
    suppressWarnings(num <- as.numeric(value))
    bad <- which(is.na(num) & !is.na(value) & trimws(value) != "")
    msg <- paste0("column is character, not numeric.")
    if (length(bad)) {
      ex <- utils::head(unique(value[bad]), 3)
      msg <- paste0(msg, " ", length(bad), " value(s) will not convert, e.g. ",
                    paste0("\"", ex, "\"", collapse = ", "),
                    ". Fix these before converting.")
    } else {
      msg <- paste0(msg, " All values convert cleanly, so as.numeric() is ",
                    "safe here, but check why the column imported as text.")
    }
    f[[length(f) + 1]] <- .eiof_finding("type", name, "error", n, msg)
    return(.eiof_report(f, n_obs = n))
  }
  if (is.integer(value)) {
    f[[length(f) + 1]] <- .eiof_finding(
      "type", name, "error", n,
      paste0("column is integer. It cannot represent values below 1, so any ",
             "small impact is already lost. Re-import as double."))
    return(.eiof_report(f, n_obs = n))
  }
  if (!is.numeric(value)) {
    f[[length(f) + 1]] <- .eiof_finding(
      "type", name, "error", n,
      paste0("column is ", class(value)[1], ", not numeric."))
    return(.eiof_report(f, n_obs = n))
  }

  # ---- validity ------------------------------------------------------------ #
  n_na <- sum(is.na(value))
  if (n_na == n) {
    f[[length(f) + 1]] <- .eiof_finding("missing", name, "error", n_na,
      "every value is missing.")
  } else if (n_na > 0) {
    f[[length(f) + 1]] <- .eiof_finding("missing", name, "note", n_na,
      sprintf("%d missing (%.1f%%). These return NA, not 0.",
              n_na, 100 * n_na / n))
  }

  n_inf <- sum(!is.na(value) & !is.finite(value))
  if (n_inf > 0) {
    f[[length(f) + 1]] <- .eiof_finding("finite", name, "error", n_inf,
      sprintf("%d non-finite value(s) (Inf or NaN).", n_inf))
  }

  fin <- value[is.finite(value)]

  n_neg <- sum(fin < 0)
  if (n_neg > 0) {
    f[[length(f) + 1]] <- .eiof_finding("negative", name, "error", n_neg,
      sprintf("%d negative value(s). No environmental impact can be negative.",
              n_neg))
  }

  n_zero <- sum(fin == 0)
  if (n_zero > 0) {
    f[[length(f) + 1]] <- .eiof_finding("zero", name, "warning", n_zero,
      sprintf(paste0("%d value(s) are exactly zero. A diet that was consumed ",
                     "cannot have zero impact; this usually means an unmatched ",
                     "food item summed to 0 instead of NA."), n_zero))
  }

  if (!is.null(indicator) && indicator %in% .eiof_valid_indicators()) {
    f[[length(f) + 1]] <- .eiof_finding("unit", name, "note", NA_integer_,
      paste0("expected unit for \"", indicator, "\": ", .eiof_unit(indicator),
             " per capita per day."))
  }

  .eiof_report(f, n_obs = n)
}


#' Check the numerical precision of an impact variable
#'
#' Two related checks: how many significant digits the stored values actually
#' retain, and how many observations sit close enough to their own cut-off that
#' the classification could flip under rounding.
#'
#' @section The small exponent is not the problem:
#' R's `numeric` is an IEEE 754 double: around 15 to 17 significant decimal
#' digits, with an exponent range to about 1e±308. A biodiversity value of
#' `3.6896e-13` is comfortably representable, and comparison with `<` is exact.
#' No epsilon is needed, provided both sides carry full precision.
#'
#' The real risk is **ingestion fidelity** - digits lost before R ever sees the
#' number. Excel displays `3.69E-13` and can write back only what it displays;
#' CSV exports written with a `%.6g`-style format truncate silently; Stata's
#' `float` storage holds about 7 significant digits; database `REAL` columns are
#' single precision for the same reason.
#'
#' @section What the checks measure:
#' The first finds, for each distinct value, the smallest number of significant
#' digits that reproduces it exactly. Across many distinct values, a maximum of
#' 7 or fewer is consistent with single-precision storage or a truncated export.
#' Measured on simulated data, full-precision values return 17, a
#' single-precision round trip returns exactly 7, and an Excel three-digit
#' re-save returns 3, so the cases separate cleanly.
#'
#' The second is the one that answers whether precision matters *here*. It
#' computes, for each observation, the relative distance to the cut-off that
#' applies to it, and counts those within `tolerance`. Usually the answer is
#' zero and the worry can be set aside; when it is not, you get the row numbers.
#'
#' Nothing is repaired. Digits that were never read cannot be recovered.
#'
#' @param value Numeric vector of impact values.
#' @param group Optional demographic groups 1-8. Required for the cut-off
#'   proximity check.
#' @param indicator Optional indicator name. Required for the proximity check.
#' @param name A label for the variable, used in the report.
#' @param tolerance Relative distance from a cut-off below which a
#'   classification is treated as not robust. Default `1e-6`.
#' @param metric Which cut-off to measure distance from: `"abs"` (default) or
#'   `"rel"`.
#' @param percentile Required when `metric = "rel"`.
#'
#' @return An object of class `eiof_check`.
#'
#' @examples
#' data(eiof_demo)
#' grp <- eiof_age_sex_group(eiof_demo$age, eiof_demo$sex)
#' eiof_check_precision(eiof_demo$biodiv, grp, "biodiv", name = "biodiv")
#'
#' # the same values after a single-precision round trip
#' eiof_check_precision(signif(eiof_demo$biodiv, 7), grp, "biodiv",
#'                      name = "biodiv_float")
#'
#' @export
eiof_check_precision <- function(value, group = NULL, indicator = NULL,
                                 name = "impact", tolerance = 1e-6,
                                 metric = c("abs", "rel"),
                                 percentile = NULL) {

  metric <- match.arg(metric)
  f <- list()
  n <- length(value)

  if (!is.numeric(value) || is.integer(value)) {
    return(.eiof_report(list(.eiof_finding(
      "precision", name, "note", NA_integer_,
      "skipped: variable is not a double. Fix the type first.")), n_obs = n))
  }

  fin <- value[is.finite(value) & value != 0]
  n_distinct <- length(unique(fin))

  # ---- significant digits retained ----------------------------------------- #
  if (n_distinct >= 20) {
    nsig <- .eiof_nsig(fin)
    mx <- max(nsig, na.rm = TRUE)
    if (mx <= 7) {
      f[[length(f) + 1]] <- .eiof_finding(
        "precision", name, "warning", n_distinct,
        sprintf(paste0("at most %d significant digits across %d distinct ",
                       "values. Consistent with single-precision storage or a ",
                       "truncated export. Check how the column was written."),
                mx, n_distinct))
    } else {
      f[[length(f) + 1]] <- .eiof_finding(
        "precision", name, "note", n_distinct,
        sprintf("up to %d significant digits retained across %d distinct values.",
                mx, n_distinct))
    }
  } else if (n_distinct > 0) {
    f[[length(f) + 1]] <- .eiof_finding(
      "precision", name, "note", n_distinct,
      sprintf(paste0("only %d distinct non-zero values; too few to judge ",
                     "stored precision."), n_distinct))
  }

  # ---- underflow ----------------------------------------------------------- #
  if (length(fin)) {
    n_sub <- sum(abs(fin) < .Machine$double.xmin)
    if (n_sub > 0) {
      f[[length(f) + 1]] <- .eiof_finding(
        "underflow", name, "error", n_sub,
        sprintf("%d value(s) are subnormal (below %.3g) and have lost precision.",
                n_sub, .Machine$double.xmin))
    }
  }

  # ---- cut-off proximity ---------------------------------------------------- #
  if (is.null(group) || is.null(indicator)) {
    f[[length(f) + 1]] <- .eiof_finding(
      "proximity", name, "note", NA_integer_,
      paste0("cut-off proximity not checked: supply `group` and `indicator` ",
             "to find out whether precision could change any classification."))
  } else {
    cuts <- rep(NA_real_, n)
    if (metric == "abs") {
      thr <- .eiof_abs_thresholds(indicator)
      ok <- !is.na(group)
      cuts[ok] <- thr[group[ok]]
      what <- "the absolute threshold"
    } else {
      if (is.null(percentile)) {
        stop("`percentile` is required when metric = \"rel\".", call. = FALSE)
      }
      cp <- .eiof_rel_cutpoints(indicator)
      ok <- !is.na(group)
      cuts[ok] <- cp[cbind(group[ok], percentile / 10)]
      what <- paste0("the P", percentile, " benchmark")
    }

    rel_dist <- abs(value - cuts) / cuts
    near <- which(!is.na(rel_dist) & rel_dist < tolerance)

    if (length(near)) {
      ex <- paste(utils::head(near, 6), collapse = ", ")
      if (length(near) > 6) ex <- paste0(ex, ", ...")
      f[[length(f) + 1]] <- .eiof_finding(
        "proximity", name, "warning", length(near),
        sprintf(paste0("%d observation(s) sit within %.0e (relative) of %s. ",
                       "Their classification is not robust to the precision in ",
                       "your input. Rows: %s"),
                length(near), tolerance, what, ex))
    } else {
      f[[length(f) + 1]] <- .eiof_finding(
        "proximity", name, "note", 0L,
        sprintf(paste0("no observation sits within %.0e of %s, so stored ",
                       "precision does not change any classification here."),
                tolerance, what))
    }
  }

  .eiof_report(f, n_obs = n)
}


#' Check the magnitude of an impact variable against the global reference
#'
#' Compares the median of your data against the published global 50th
#' percentile for that indicator, weighted to your sample's demographic mix,
#' and names the likely conversion when the ratio lands near a common unit
#' factor.
#'
#' This is the check that catches a unit error. A greenhouse gas column
#' supplied in grams rather than kilograms will sit about 1000 times above the
#' reference and be reported as such, rather than quietly producing a result in
#' which every diet exceeds every threshold.
#'
#' @param value Numeric vector of impact values.
#' @param indicator The indicator name.
#' @param group Demographic groups 1-8, used to weight the expected median.
#' @param name A label for the variable, used in the report.
#'
#' @return An object of class `eiof_check`.
#'
#' @examples
#' data(eiof_demo)
#' grp <- eiof_age_sex_group(eiof_demo$age, eiof_demo$sex)
#'
#' eiof_check_magnitude(eiof_demo$ghg, "ghg", grp, name = "ghg")
#'
#' # the same data in grams
#' eiof_check_magnitude(eiof_demo$ghg * 1000, "ghg", grp, name = "ghg_grams")
#'
#' @export
eiof_check_magnitude <- function(value, indicator, group = NULL, name = "impact") {

  .eiof_check_indicator(indicator)
  n <- length(value)

  if (!is.numeric(value) || all(is.na(value))) {
    return(.eiof_report(list(.eiof_finding(
      "magnitude", name, "note", NA_integer_,
      "skipped: variable is not usable numeric data.")), n_obs = n))
  }

  cp <- .eiof_rel_cutpoints(indicator)
  p50 <- cp[, 5L]

  # A unit error and a broken sex variable are independent faults. If the
  # demographic group could not be built, fall back to an unweighted mean of
  # the eight group medians rather than skipping: the ratios that matter here
  # are factors of 1000, which dwarf any demographic weighting.
  tab <- if (is.null(group)) NULL else
    table(factor(group[!is.na(group)], levels = 1:8))

  if (is.null(tab) || sum(tab) == 0) {
    expected <- mean(p50)
    weighted <- FALSE
  } else {
    expected <- sum(p50 * as.numeric(tab)) / sum(tab)
    weighted <- TRUE
  }

  observed <- stats::median(value, na.rm = TRUE)
  ratio <- observed / expected

  # candidate unit confusions, by indicator
  cand <- switch(indicator,
    landtotal   = , landarable = , landpasture =
      c("m2 <-> ha" = 1e4, "ha <-> m2" = 1e-4),
    ghg         = c("g <-> kg" = 1e3, "kg <-> g" = 1e-3,
                    "mg <-> kg" = 1e6, "t <-> kg" = 1e-3),
    eutroph     = c("g <-> kg" = 1e3, "kg <-> g" = 1e-3,
                    "mg <-> kg" = 1e6),
    water       = c("L <-> m3" = 1e3, "m3 <-> L" = 1e-3,
                    "mL <-> L" = 1e3),
    biodiv      = c("factor of 1000" = 1e3, "factor of 1000" = 1e-3),
    numeric(0)
  )

  hit <- NULL
  for (i in seq_along(cand)) {
    if (ratio / cand[i] > 0.5 && ratio / cand[i] < 2) {
      hit <- names(cand)[i]
      break
    }
  }

  lvl <- if (!is.null(hit)) "warning"
         else if (ratio > 10 || ratio < 0.1) "warning"
         else "note"

  msg <- sprintf("median %.4g vs expected %.4g %s (ratio %.3g).",
                 observed, expected,
                 if (weighted) "for this demographic mix"
                 else "(unweighted: no usable demographic group)",
                 ratio)
  if (!is.null(hit)) {
    msg <- paste0(msg, " Consistent with a ", hit, " unit error. ",
                  "Expected unit: ", .eiof_unit(indicator), ".")
  } else if (lvl == "warning") {
    msg <- paste0(msg, " Unusual but not a recognised unit factor. ",
                  "Check the unit (expected ", .eiof_unit(indicator),
                  ") and the population.")
  }

  .eiof_report(list(.eiof_finding("magnitude", name, lvl, NA_integer_, msg)),
               n_obs = n)
}
