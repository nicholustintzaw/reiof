# ---------------------------------------------------------------------------- #
# Internal helpers. Not exported.
# ---------------------------------------------------------------------------- #

#' Access a dataset shipped with the package
#'
#' Avoids the "no visible binding for global variable" note that referring to
#' lazy-loaded package data directly would produce under `R CMD check`.
#'
#' @param name Dataset name.
#' @return The dataset.
#' @noRd
.eiof_data <- function(name) {
  get(name, envir = asNamespace("reiof"))
}

#' The seven valid indicator names
#' @noRd
.eiof_valid_indicators <- function() {
  c("landtotal", "landarable", "landpasture", "ghg",
    "eutroph", "water", "biodiv")
}

#' Validate an indicator name
#' @noRd
.eiof_check_indicator <- function(indicator) {
  if (!is.character(indicator) || length(indicator) != 1L || is.na(indicator)) {
    stop("`indicator` must be a single character string.", call. = FALSE)
  }
  ok <- .eiof_valid_indicators()
  if (!indicator %in% ok) {
    stop("`indicator` must be one of: ", paste(ok, collapse = ", "),
         ".\n  Got: \"", indicator, "\".",
         "\n  See eiof_list_indicators() for labels and units.",
         call. = FALSE)
  }
  invisible(indicator)
}

#' Absolute thresholds for one indicator
#'
#' @return A numeric vector of length 8, indexed by demographic group.
#' @noRd
.eiof_abs_thresholds <- function(indicator) {
  cut <- .eiof_data("eiof_cutoffs")
  s <- cut[cut$metric == "abs" & cut$indicator == indicator, ]
  if (nrow(s) != 8L) {
    stop("expected 8 absolute thresholds for \"", indicator,
         "\", found ", nrow(s), ". The cut-off dataset is damaged.",
         call. = FALSE)
  }
  s$value[order(s$demogroup)]
}

#' Relative cut-points for one indicator
#'
#' @return An 8 x 9 numeric matrix: rows are demographic groups, columns are
#'   the P10..P90 cut-points in ascending order.
#' @noRd
.eiof_rel_cutpoints <- function(indicator) {
  cut <- .eiof_data("eiof_cutoffs")
  s <- cut[cut$metric == "rel" & cut$indicator == indicator &
             !is.na(cut$percentile), ]
  if (nrow(s) != 72L) {
    stop("expected 72 relative cut-points for \"", indicator,
         "\" (8 groups x P10-P90), found ", nrow(s),
         ". The cut-off dataset is damaged.", call. = FALSE)
  }
  m <- matrix(NA_real_, nrow = 8L, ncol = 9L)
  m[cbind(s$demogroup, s$percentile / 10L)] <- s$value
  if (anyNA(m)) stop("incomplete cut-point grid for \"", indicator, "\".",
                     call. = FALSE)
  m
}

#' The unit string for one indicator
#' @noRd
.eiof_unit <- function(indicator) {
  ind <- .eiof_data("eiof_indicators")
  ind$unit[match(indicator, ind$indicator)]
}

#' Validate a demographic group vector
#' @noRd
.eiof_check_group <- function(group, n = NULL) {
  if (!is.numeric(group)) {
    stop("`group` must be numeric, with values 1-8. ",
         "Build it with eiof_age_sex_group().", call. = FALSE)
  }
  bad <- !is.na(group) & !(group %in% 1:8)
  if (any(bad)) {
    stop(sum(bad), " value(s) in `group` are outside 1-8. ",
         "Build it with eiof_age_sex_group().", call. = FALSE)
  }
  if (!is.null(n) && length(group) != n) {
    stop("`group` has length ", length(group),
         " but the impact vector has length ", n, ".", call. = FALSE)
  }
  invisible(TRUE)
}

#' Validate a numeric impact vector
#' @noRd
.eiof_check_value <- function(value, arg = "value") {
  if (is.factor(value)) {
    stop("`", arg, "` is a factor. as.numeric() on a factor returns the level ",
         "codes, not the values, so this would silently produce wrong results. ",
         "Convert with as.numeric(as.character(x)), then re-check. ",
         "See eiof_check_impact().", call. = FALSE)
  }
  if (is.character(value)) {
    stop("`", arg, "` is character, not numeric. This usually means the ",
         "column failed to import: a stray symbol such as \"<0.001\", a decimal ",
         "comma, or a thousands separator. See eiof_check_impact().",
         call. = FALSE)
  }
  if (!is.numeric(value)) {
    stop("`", arg, "` must be numeric.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Number of significant digits needed to reproduce each value exactly
#'
#' Used by [eiof_check_precision()]. A column whose maximum is 7 or fewer,
#' across many distinct values, is consistent with single-precision storage or
#' a truncated export.
#'
#' @param x A numeric vector.
#' @return An integer vector; `NA` where `x` is zero or not finite.
#' @noRd
.eiof_nsig <- function(x) {
  res <- rep(NA_integer_, length(x))
  ok <- is.finite(x) & x != 0
  if (!any(ok)) return(res)
  xv <- x[ok]
  nd <- rep(17L, length(xv))
  for (d in 16:1) {
    nd[signif(xv, d) == xv] <- d
  }
  res[ok] <- nd
  res
}

#' Resolve a sex vector to internal codes
#'
#' Returns 1 for male, 2 for female, `NA` for missing. Accepts a factor,
#' character or numeric vector. See [eiof_age_sex_group()] for the full
#' discussion of which input type to prefer.
#'
#' The numeric branch deliberately refuses to guess. Survey data uses 1/2 and
#' 0/1 about equally often, and guessing wrong inverts every downstream result
#' silently, so `male` and `female` must be given explicitly.
#'
#' @noRd
.eiof_resolve_sex <- function(sex, male = NULL, female = NULL) {

  if (is.logical(sex) && all(is.na(sex))) {
    return(rep(NA_integer_, length(sex)))
  }
  if (is.logical(sex)) {
    stop("`sex` is logical (TRUE/FALSE), which does not say which value means ",
         "male. Convert it first, for example with eiof_as_sex_factor().",
         call. = FALSE)
  }

  one_given <- xor(is.null(male), is.null(female))
  if (one_given) {
    stop("supply both `male` and `female`, or neither.", call. = FALSE)
  }

  # ---- factor or character: match on labels -------------------------------- #
  if (is.factor(sex) || is.character(sex)) {
    key <- tolower(trimws(as.character(sex)))
    key[key == ""] <- NA_character_

    if (!is.null(male)) {
      mk <- tolower(trimws(as.character(male)))
      fk <- tolower(trimws(as.character(female)))
    } else {
      mk <- c("m", "male", "males", "man", "men", "boy", "boys")
      fk <- c("f", "female", "females", "woman", "women", "girl", "girls")
    }

    out <- rep(NA_integer_, length(key))
    out[key %in% mk] <- 1L
    out[key %in% fk] <- 2L

    unmatched <- unique(key[!is.na(key) & is.na(out)])
    if (length(unmatched)) {
      stop("these values of `sex` were not recognised as male or female:\n  ",
           paste0("\"", unmatched, "\"", collapse = ", "),
           "\n  Set `male` and `female` explicitly, for example ",
           "male = \"M\", female = \"F\".",
           "\n  They are not dropped silently, because that would quietly ",
           "shrink your analysis sample.", call. = FALSE)
    }
    return(out)
  }

  # ---- numeric: require explicit codes ------------------------------------- #
  if (is.numeric(sex)) {
    obs <- sort(unique(sex[!is.na(sex)]))
    if (is.null(male)) {
      stop("`sex` is numeric, so reiof cannot tell which code means male.\n",
           "  Observed codes: ", paste(obs, collapse = ", "), "\n",
           "  Survey data uses 1/2 and 0/1 about equally often, and guessing ",
           "wrong would invert every result without any error.\n",
           "  Either state the codes:\n",
           "    eiof_age_sex_group(age, sex, male = 1, female = 2)\n",
           "  or convert once, which is the recommended route:\n",
           "    dat$sex <- eiof_as_sex_factor(dat$sex, male = 1, female = 2)",
           call. = FALSE)
    }
    if (length(male) != 1L || length(female) != 1L ||
        !is.numeric(male) || !is.numeric(female)) {
      stop("`male` and `female` must each be a single numeric code when `sex` ",
           "is numeric.", call. = FALSE)
    }
    if (male == female) {
      stop("`male` and `female` must differ.", call. = FALSE)
    }

    out <- rep(NA_integer_, length(sex))
    out[!is.na(sex) & sex == male]   <- 1L
    out[!is.na(sex) & sex == female] <- 2L

    unmatched <- setdiff(obs, c(male, female))
    if (length(unmatched)) {
      stop("these values of `sex` match neither male = ", male,
           " nor female = ", female, ":\n  ",
           paste(unmatched, collapse = ", "),
           "\n  Recode them or set `male` and `female` to match your data. ",
           "They are not dropped silently.", call. = FALSE)
    }
    return(out)
  }

  stop("`sex` must be a factor, character or numeric vector. Got ",
       class(sex)[1], ".", call. = FALSE)
}
