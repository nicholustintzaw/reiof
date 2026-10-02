#' List the seven environmental impact indicators
#'
#' Prints the indicator names the `eiof_*` functions accept, with their labels
#' and the units your impact values must be in.
#'
#' @param quiet If `TRUE`, return the table without printing it.
#'
#' @return A data frame of indicator, label and unit, invisibly when printing.
#'
#' @examples
#' eiof_list_indicators()
#'
#' @export
eiof_list_indicators <- function(quiet = FALSE) {
  ind <- .eiof_data("eiof_indicators")
  if (quiet) return(ind)

  cat("Seven environmental impact indicators, per capita per day:\n\n")
  w <- max(nchar(ind$indicator))
  for (i in seq_len(nrow(ind))) {
    cat(sprintf("  %-*s  %-26s %s\n", w, ind$indicator[i],
                ind$indicator_label[i], ind$unit[i]))
  }
  cat("\nValues must already be summed across everything the respondent\n")
  cat("consumed that day. reiof does not compute impacts from food items.\n")
  invisible(ind)
}


#' Look up a published cut-off value
#'
#' Returns the cut-off itself, rather than applying it. Useful for reporting
#' which number a respondent was compared against, and for checking the
#' package against the printed tables.
#'
#' @param indicator One of the seven indicator names.
#' @param group Demographic group 1-8, or `NULL` for all eight.
#' @param metric `"abs"` for the Table 3 absolute threshold, or `"rel"` for a
#'   Table 4 relative benchmark cut-point.
#' @param percentile Required when `metric = "rel"`: a multiple of 10 from 10
#'   to 90.
#' @param published If `TRUE`, return the value exactly as printed in the
#'   paper rather than the harmonised value the metric functions use. The two
#'   differ only for relative eutrophication; see
#'   `vignette("reiof")`.
#'
#' @return A named numeric vector, named by demographic group label.
#'
#' @examples
#' # the HESD biodiversity criterion, all groups
#' eiof_lookup_cutoff("biodiv", metric = "abs")
#'
#' # the HDLEI biodiversity criterion
#' eiof_lookup_cutoff("biodiv", metric = "rel", percentile = 30)
#'
#' # one group
#' eiof_lookup_cutoff("ghg", group = 4, metric = "abs")
#'
#' @export
eiof_lookup_cutoff <- function(indicator, group = NULL,
                               metric = c("abs", "rel"),
                               percentile = NULL, published = FALSE) {

  metric <- match.arg(metric)
  .eiof_check_indicator(indicator)

  cut <- .eiof_data("eiof_cutoffs")
  s <- cut[cut$metric == metric & cut$indicator == indicator, ]

  if (metric == "rel") {
    if (is.null(percentile)) {
      stop("`percentile` is required when metric = \"rel\". ",
           "Use a multiple of 10 from 10 to 90.", call. = FALSE)
    }
    if (!is.numeric(percentile) || length(percentile) != 1L ||
        percentile %% 10 != 0 || percentile < 10 || percentile > 90) {
      stop("`percentile` must be a multiple of 10 between 10 and 90.",
           call. = FALSE)
    }
    s <- s[!is.na(s$percentile) & s$percentile == percentile, ]
  } else {
    if (!is.null(percentile)) {
      stop("`percentile` does not apply when metric = \"abs\".", call. = FALSE)
    }
  }

  if (!is.null(group)) {
    if (!is.numeric(group) || !all(group %in% 1:8)) {
      stop("`group` must be one or more integers from 1 to 8.", call. = FALSE)
    }
    s <- s[s$demogroup %in% group, ]
  }

  s <- s[order(s$demogroup), ]
  out <- if (published) s$value_published else s$value
  stats::setNames(out, s$demogroup_label)
}
