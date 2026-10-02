#' Check the age and sex variables
#'
#' Verifies that age and sex can be turned into the eight demographic groups,
#' and reports group coverage, including how many respondents fall below the
#' age-2 floor that the composite metrics apply.
#'
#' Like the other check functions, this reports rather than stops. Where
#' [eiof_age_sex_group()] would raise an error, this records it as a finding so
#' you can see every problem in one pass instead of one per run.
#'
#' @param age Numeric vector of ages.
#' @param sex Factor, character or numeric vector of sex.
#' @param age_units `"years"` (default) or `"months"`.
#' @param male,female Optional codes; see [eiof_age_sex_group()].
#'
#' @return An object of class `eiof_check`.
#'
#' @examples
#' data(eiof_demo)
#' eiof_check_demographics(eiof_demo$age, eiof_demo$sex)
#'
#' # numeric sex with no codes given: reported, not guessed
#' eiof_check_demographics(eiof_demo$age,
#'                         ifelse(eiof_demo$sex == "male", 1L, 0L))
#'
#' @export
eiof_check_demographics <- function(age, sex, age_units = c("years", "months"),
                                    male = NULL, female = NULL) {

  age_units <- match.arg(age_units)
  f <- list()
  n <- length(age)

  # ---- age ----------------------------------------------------------------- #
  if (is.factor(age) || !is.numeric(age)) {
    f[[length(f) + 1]] <- .eiof_finding("type", "age", "error", n,
      paste0("age is ", class(age)[1], ", not numeric."))
    age_yr <- rep(NA_real_, n)
  } else {
    age_yr <- if (age_units == "months") age / 12 else age

    n_na <- sum(is.na(age_yr))
    if (n_na) {
      f[[length(f) + 1]] <- .eiof_finding("missing", "age", "note", n_na,
        sprintf("%d missing (%.1f%%); these get no demographic group and so no metrics.",
                n_na, 100 * n_na / n))
    }
    n_neg <- sum(!is.na(age_yr) & age_yr < 0)
    if (n_neg) {
      f[[length(f) + 1]] <- .eiof_finding("negative", "age", "error", n_neg,
        sprintf("%d negative age(s).", n_neg))
    }
    n_old <- sum(!is.na(age_yr) & age_yr > 120)
    if (n_old) {
      f[[length(f) + 1]] <- .eiof_finding("range", "age", "warning", n_old,
        sprintf(paste0("%d age(s) above 120 years. If age is recorded in ",
                       "months, set age_units = \"months\"."), n_old))
    }
    if (age_units == "years" && sum(!is.na(age_yr)) > 0 &&
        stats::median(age_yr, na.rm = TRUE) > 110) {
      f[[length(f) + 1]] <- .eiof_finding("units", "age", "warning", NA_integer_,
        "median age is implausibly high for years; is this months?")
    }
  }

  # ---- sex ----------------------------------------------------------------- #
  sex_code <- tryCatch(
    .eiof_resolve_sex(sex, male = male, female = female),
    error = function(e) {
      f[[length(f) + 1]] <<- .eiof_finding("sex", "sex", "error", n,
        conditionMessage(e))
      rep(NA_integer_, length(sex))
    })

  if (is.numeric(sex) && !is.null(male)) {
    f[[length(f) + 1]] <- .eiof_finding("sex", "sex", "note", NA_integer_,
      paste0("sex is numeric with male = ", male, ", female = ", female,
             ". Converting once with eiof_as_sex_factor() makes every later ",
             "call unambiguous."))
  } else if (is.character(sex)) {
    f[[length(f) + 1]] <- .eiof_finding("sex", "sex", "note", NA_integer_,
      "sex is character; converting to a factor makes the data self-documenting.")
  } else if (is.factor(sex)) {
    f[[length(f) + 1]] <- .eiof_finding("sex", "sex", "note", NA_integer_,
      paste0("sex is a factor with levels ",
             paste0("\"", levels(sex), "\"", collapse = ", "),
             ". This is the recommended input type."))
  }

  n_na_sex <- sum(is.na(sex_code))
  if (n_na_sex && n_na_sex < n) {
    f[[length(f) + 1]] <- .eiof_finding("missing", "sex", "note", n_na_sex,
      sprintf("%d missing (%.1f%%).", n_na_sex, 100 * n_na_sex / n))
  }

  # ---- group coverage ------------------------------------------------------ #
  if (!all(is.na(age_yr)) && !all(is.na(sex_code))) {
    band <- rep(NA_integer_, n)
    ok <- !is.na(age_yr) & age_yr >= 0
    band[ok & age_yr <  2]                <- 1L
    band[ok & age_yr >= 2 & age_yr <  5]  <- 2L
    band[ok & age_yr >= 5 & age_yr < 10]  <- 3L
    band[ok & age_yr >= 10]               <- 4L
    grp <- ifelse(is.na(band) | is.na(sex_code), NA_integer_,
                  band + (sex_code - 1L) * 4L)

    tab <- table(factor(grp, levels = 1:8))
    empty <- which(tab == 0)
    if (length(empty)) {
      labs <- c("Male 0-1.9", "Male 2-4.9", "Male 5-9.9", "Male 10+",
                "Female 0-1.9", "Female 2-4.9", "Female 5-9.9", "Female 10+")
      f[[length(f) + 1]] <- .eiof_finding("coverage", "group", "note",
        length(empty),
        paste0("no observations in: ", paste(labs[empty], collapse = ", "),
               ". Group-specific estimates are unavailable for these."))
    }

    n_u2 <- sum(grp %in% c(1L, 5L), na.rm = TRUE)
    if (n_u2) {
      f[[length(f) + 1]] <- .eiof_finding("coverage", "group", "note", n_u2,
        sprintf(paste0("%d respondent(s) under 2 years. Tables 3 and 4 apply ",
                       "to them; HESD and HDLEI do not, and will be NA. Decide ",
                       "explicitly whether they belong in your denominator."),
                n_u2))
    }
  }

  .eiof_report(f, n_obs = n)
}


#' Check the Global Diet Quality Score variable
#'
#' @param gdqs Numeric vector of GDQS values.
#' @param cutoff The healthy-diet threshold, default 23.
#' @param max_score The maximum attainable score, used for the range check.
#'   Default 49.
#'
#' @return An object of class `eiof_check`.
#'
#' @examples
#' data(eiof_demo)
#' eiof_check_gdqs(eiof_demo$gdqs)
#'
#' @export
eiof_check_gdqs <- function(gdqs, cutoff = 23, max_score = 49) {

  f <- list()
  n <- length(gdqs)

  if (is.factor(gdqs) || !is.numeric(gdqs)) {
    f[[length(f) + 1]] <- .eiof_finding("type", "gdqs", "error", n,
      paste0("gdqs is ", class(gdqs)[1], ", not numeric."))
    return(.eiof_report(f, n_obs = n))
  }

  n_na <- sum(is.na(gdqs))
  if (n_na == n) {
    f[[length(f) + 1]] <- .eiof_finding("missing", "gdqs", "error", n_na,
      "every GDQS value is missing; the composite metrics cannot be computed.")
    return(.eiof_report(f, n_obs = n))
  }
  if (n_na) {
    f[[length(f) + 1]] <- .eiof_finding("missing", "gdqs", "note", n_na,
      sprintf("%d missing (%.1f%%); HESD and HDLEI will be NA for these.",
              n_na, 100 * n_na / n))
  }

  fin <- gdqs[is.finite(gdqs)]
  n_out <- sum(fin < 0 | fin > max_score)
  if (n_out) {
    f[[length(f) + 1]] <- .eiof_finding("range", "gdqs", "error", n_out,
      sprintf(paste0("%d value(s) outside 0-%g. Check this is the total GDQS ",
                     "score and not a sub-score or a percentage."),
              n_out, max_score))
  }

  if (max(fin) <= 1) {
    f[[length(f) + 1]] <- .eiof_finding("range", "gdqs", "warning", NA_integer_,
      paste0("all values are 1 or below. This looks like a proportion rather ",
             "than a GDQS score; the cutoff of ", cutoff, " would never be met."))
  }

  f[[length(f) + 1]] <- .eiof_finding("range", "gdqs", "note", NA_integer_,
    sprintf("range %.3g to %.3g; %.1f%% at or above the cutoff of %g.",
            min(fin), max(fin), 100 * mean(fin >= cutoff), cutoff))

  .eiof_report(f, n_obs = n)
}
