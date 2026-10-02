#' Check a dataset before computing any metric
#'
#' Runs every `eiof_check_*` function over a data frame and returns a single
#' report. This is the function to call first, before
#' [eiof_absolute_threshold()] or anything else.
#'
#' @section What it checks:
#' \itemize{
#'   \item **Structure** - that each named column exists, and that impact
#'     columns are doubles rather than factors, characters or integers.
#'   \item **Validity** - no negative or non-finite impacts, exact zeros
#'     flagged, missingness reported.
#'   \item **Precision** - significant digits actually retained, and how many
#'     observations sit close enough to a cut-off that rounding could change
#'     their classification. See [eiof_check_precision()].
#'   \item **Unit magnitude** - each impact compared against the published
#'     global reference, with likely unit errors named. See
#'     [eiof_check_magnitude()].
#'   \item **Demographics** - age range and units, sex coding, coverage of the
#'     eight groups, and the count of under-twos.
#'   \item **GDQS** - type and range.
#' }
#'
#' @section Severity:
#' `error` means a metric computed on this data would be wrong, not merely
#' uncertain; fix it first. `warning` means proceed, but you need to have
#' looked. `note` is information worth having in the record.
#'
#' @param data A data frame.
#' @param impacts A named character vector mapping indicator names to column
#'   names, for example `c(biodiv = "biodiv_pdf", ghg = "ghg_kg")`. Names must
#'   be among the seven in [eiof_list_indicators()].
#' @param age,sex,gdqs Column names, or `NULL` to skip that check.
#' @param age_units `"years"` (default) or `"months"`.
#' @param male,female Optional sex codes; see [eiof_age_sex_group()].
#' @param tolerance Relative distance from a cut-off below which a
#'   classification is treated as not robust. Default `1e-6`.
#'
#' @return An object of class `eiof_check`. Use `summary()` for a single
#'   pass/fail, or treat it as a data frame to filter the findings.
#'
#' @seealso [eiof_check_impact()], [eiof_check_precision()],
#'   [eiof_check_magnitude()], [eiof_check_demographics()],
#'   [eiof_check_gdqs()]
#'
#' @examples
#' data(eiof_demo)
#'
#' report <- eiof_check_data(
#'   eiof_demo,
#'   impacts = c(biodiv = "biodiv", ghg = "ghg"),
#'   age = "age", sex = "sex", gdqs = "gdqs"
#' )
#' report
#' summary(report)
#'
#' @export
eiof_check_data <- function(data, impacts = NULL, age = NULL, sex = NULL,
                            gdqs = NULL, age_units = c("years", "months"),
                            male = NULL, female = NULL, tolerance = 1e-6) {

  age_units <- match.arg(age_units)
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  src <- deparse(substitute(data))
  n <- nrow(data)
  parts <- list()

  # ---- column existence ---------------------------------------------------- #
  wanted <- c(impacts, age = age, sex = sex, gdqs = gdqs)
  wanted <- wanted[!vapply(wanted, is.null, logical(1))]
  missing_cols <- setdiff(unname(unlist(wanted)), names(data))
  if (length(missing_cols)) {
    parts[[length(parts) + 1]] <- .eiof_report(list(.eiof_finding(
      "structure", paste(missing_cols, collapse = ", "), "error", NA_integer_,
      paste0("column(s) not found in ", src, "."))), n_obs = n)
  }

  # ---- demographics, first: the group is needed by later checks ------------ #
  grp <- NULL
  if (!is.null(age) && !is.null(sex) &&
      all(c(age, sex) %in% names(data))) {

    parts[[length(parts) + 1]] <- eiof_check_demographics(
      data[[age]], data[[sex]], age_units = age_units,
      male = male, female = female)

    grp <- tryCatch(
      eiof_age_sex_group(data[[age]], data[[sex]], age_units = age_units,
                         male = male, female = female),
      error = function(e) NULL)
  }

  # ---- impacts ------------------------------------------------------------- #
  if (!is.null(impacts)) {
    if (is.null(names(impacts)) || any(names(impacts) == "")) {
      stop("`impacts` must be a NAMED character vector, for example ",
           "c(biodiv = \"biodiv_pdf\").", call. = FALSE)
    }
    bad_ind <- setdiff(names(impacts), .eiof_valid_indicators())
    if (length(bad_ind)) {
      stop("unknown indicator name(s) in `impacts`: ",
           paste(bad_ind, collapse = ", "),
           ".\n  Valid names: ",
           paste(.eiof_valid_indicators(), collapse = ", "), call. = FALSE)
    }

    for (ind in names(impacts)) {
      col <- impacts[[ind]]
      if (!col %in% names(data)) next
      v <- data[[col]]

      imp <- eiof_check_impact(v, name = col, indicator = ind)
      parts[[length(parts) + 1]] <- imp

      # precision and magnitude only make sense on usable numeric data
      blocked <- any(imp$level == "error" & imp$check == "type")
      if (!blocked) {
        parts[[length(parts) + 1]] <- eiof_check_precision(
          v, group = grp, indicator = ind, name = col, tolerance = tolerance)
        parts[[length(parts) + 1]] <- eiof_check_magnitude(
          v, indicator = ind, group = grp, name = col)
      }
    }
  }

  # ---- gdqs ---------------------------------------------------------------- #
  if (!is.null(gdqs) && gdqs %in% names(data)) {
    parts[[length(parts) + 1]] <- eiof_check_gdqs(data[[gdqs]])
  }

  findings <- do.call(rbind, lapply(parts, function(p) {
    attributes(p) <- list(names = names(p), row.names = seq_len(nrow(p)),
                          class = "data.frame")
    p
  }))
  if (is.null(findings)) {
    findings <- .eiof_finding(character(0), character(0), character(0),
                              integer(0), character(0))
  }
  findings$level <- factor(as.character(findings$level),
                           levels = c("error", "warning", "note"))
  findings <- findings[order(findings$level), , drop = FALSE]
  rownames(findings) <- NULL

  structure(findings, class = c("eiof_check", "data.frame"),
            n_obs = n, source = src)
}


#' @param x An `eiof_check` object.
#' @param ... Ignored.
#' @rdname eiof_check_data
#' @export
print.eiof_check <- function(x, ...) {

  n_obs <- attr(x, "n_obs")
  src   <- attr(x, "source")

  cat("reiof data quality report\n")
  if (!is.null(src) && !is.na(n_obs)) {
    cat("  data: ", src, "   n = ", format(n_obs, big.mark = ","), "\n", sep = "")
  }
  cat("\n")

  if (nrow(x) == 0L) {
    cat("  no findings.\n")
    return(invisible(x))
  }

  counts <- table(factor(as.character(x$level),
                         levels = c("error", "warning", "note")))
  cat(sprintf("  ERROR    %d\n  WARNING  %d\n  NOTE     %d\n\n",
              counts[["error"]], counts[["warning"]], counts[["note"]]))

  for (lv in c("error", "warning", "note")) {
    rows <- which(as.character(x$level) == lv)
    for (i in rows) {
      head_txt <- sprintf("  %-8s %-14s %-12s ",
                          toupper(lv), x$variable[i], x$check[i])
      msg <- strwrap(x$message[i], width = 76 - nchar(head_txt) + 36,
                     prefix = "")
      cat(head_txt, msg[1], "\n", sep = "")
      if (length(msg) > 1) {
        for (m in msg[-1]) cat(strrep(" ", nchar(head_txt)), m, "\n", sep = "")
      }
    }
  }

  cat("\n")
  n_err <- counts[["error"]]
  if (n_err > 0) {
    cat("  ", n_err, " blocking problem", if (n_err > 1) "s" else "",
        ". Fix before computing metrics.\n", sep = "")
  } else {
    cat("  No blocking problems.\n")
  }
  invisible(x)
}


#' @param object An `eiof_check` object.
#' @rdname eiof_check_data
#' @export
summary.eiof_check <- function(object, ...) {
  counts <- table(factor(as.character(object$level),
                         levels = c("error", "warning", "note")))
  structure(
    list(n_error = as.integer(counts[["error"]]),
         n_warning = as.integer(counts[["warning"]]),
         n_note = as.integer(counts[["note"]]),
         pass = counts[["error"]] == 0L),
    class = "summary.eiof_check")
}

#' @export
print.summary.eiof_check <- function(x, ...) {
  cat(if (x$pass) "PASS" else "FAIL",
      sprintf(" - %d error, %d warning, %d note\n",
              x$n_error, x$n_warning, x$n_note), sep = "")
  invisible(x)
}
