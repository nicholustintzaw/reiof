#' Build the eight-category age and sex demographic group
#'
#' Every cut-off in the Intake framework is scaled to one of eight age/sex
#' groups, because average dietary energy requirement differs across them. This
#' function builds that grouping variable, which all other `eiof_*` metric
#' functions require.
#'
#' @section The eight groups:
#' \tabular{rl}{
#'   1 \tab Male 0-1.9 yr   \cr
#'   2 \tab Male 2-4.9 yr   \cr
#'   3 \tab Male 5-9.9 yr   \cr
#'   4 \tab Male 10+ yr     \cr
#'   5 \tab Female 0-1.9 yr \cr
#'   6 \tab Female 2-4.9 yr \cr
#'   7 \tab Female 5-9.9 yr \cr
#'   8 \tab Female 10+ yr
#' }
#'
#' @section Age boundaries are half-open:
#' The paper prints the groups as "0-1.9", "2-4.9", "5-9.9" and "10+". These are
#' read as half-open intervals: `age < 2`, `2 <= age < 5`, `5 <= age < 10`,
#' `age >= 10`. A child aged 1.95 years therefore belongs in the first group.
#' Testing `age <= 1.9` instead would leave everyone between 1.9 and 2.0
#' unassigned, and they would then drop out of every downstream metric without
#' any error being raised.
#'
#' @section Which type should `sex` be?:
#' All three common types are accepted, but they are not equally safe.
#'
#' \strong{A factor is the recommended input.} Its levels carry the meaning, so
#' the data is self-documenting and no separate codebook is needed; `table()`
#' and model output print "male" and "female" rather than 1 and 2; and the
#' function can resolve it without being told anything further.
#'
#' \strong{Character} works identically and is what `read.csv()` produces by
#' default in R 4.0 and later. Converting it to a factor costs nothing and is
#' worth doing.
#'
#' \strong{Numeric requires `male` and `female` to be given explicitly.} This is
#' a deliberate refusal rather than a missing feature. Survey data uses 1/2 and
#' 0/1 about equally often, and there is no way to tell them apart from the
#' values alone: both are just two distinct numbers. If the function guessed and
#' guessed wrong, every result would be silently inverted, with no error and
#' nothing visibly odd in the output. Rather than carry that risk, numeric input
#' errors until the codes are stated.
#'
#' The recommended route for numeric data is to convert once, near the top of
#' the analysis, with [eiof_as_sex_factor()]. After that every downstream call
#' is unambiguous.
#'
#' For factor and character input, these labels are recognised automatically,
#' case-insensitively and ignoring surrounding spaces: `m`, `male`, `males`,
#' `man`, `men`, `boy`, `boys`, and `f`, `female`, `females`, `woman`, `women`,
#' `girl`, `girls`. Anything else raises an error listing the unrecognised
#' values; they are never dropped silently, because that would quietly shrink
#' the analysis sample.
#'
#' @param age Numeric vector of respondent ages.
#' @param sex Factor, character or numeric vector of respondent sex. See
#'   "Which type should `sex` be?" above.
#' @param age_units Either `"years"` (the default) or `"months"`.
#' @param male,female Optional. The value of `sex` that means male and female
#'   respectively. Required when `sex` is numeric; optional for factor and
#'   character input, where they override the automatic label matching. Supply
#'   both or neither.
#'
#' @return An integer vector of group codes 1 to 8, with `NA` where age or sex
#'   is missing. The result carries a `"labels"` attribute giving the group
#'   names, and can be turned into a labelled factor with
#'   `factor(x, 1:8, attr(x, "labels"))`.
#'
#' @seealso [eiof_as_sex_factor()] to convert a numeric sex code once.
#'
#' @examples
#' # the recommended input: sex as a factor
#' eiof_age_sex_group(age = c(1.5, 3, 7, 40), sex = factor(c("male", "male",
#'                    "female", "female")))
#'
#' # character works the same way
#' eiof_age_sex_group(age = c(1.5, 40), sex = c("M", "F"))
#'
#' # numeric requires the codes to be stated
#' eiof_age_sex_group(age = c(1.5, 40), sex = c(1, 2), male = 1, female = 2)
#'
#' # age in months
#' eiof_age_sex_group(age = c(23, 24), sex = c("male", "male"),
#'                    age_units = "months")
#'
#' @export
eiof_age_sex_group <- function(age, sex,
                               age_units = c("years", "months"),
                               male = NULL, female = NULL) {

  age_units <- match.arg(age_units)

  # ---- validate age ------------------------------------------------------- #
  if (is.factor(age)) {
    stop("`age` is a factor. as.numeric() on a factor returns level codes, ",
         "not ages. Convert with as.numeric(as.character(age)).", call. = FALSE)
  }
  if (!is.numeric(age)) {
    stop("`age` must be numeric. Got ", class(age)[1], ".", call. = FALSE)
  }
  if (length(age) != length(sex)) {
    stop("`age` and `sex` must be the same length: ", length(age), " vs ",
         length(sex), ".", call. = FALSE)
  }

  age_yr <- if (age_units == "months") age / 12 else age

  neg <- !is.na(age_yr) & age_yr < 0
  if (any(neg)) {
    stop(sum(neg), " negative value(s) in `age`.", call. = FALSE)
  }
  implausible <- !is.na(age_yr) & age_yr > 120
  if (any(implausible)) {
    warning(sum(implausible), " value(s) in `age` exceed 120 years. ",
            "If age is recorded in months, set age_units = \"months\".",
            call. = FALSE)
  }

  # ---- resolve sex -------------------------------------------------------- #
  sex_code <- .eiof_resolve_sex(sex, male = male, female = female)

  # ---- assign ------------------------------------------------------------- #
  # Half-open intervals; see the "Age boundaries" section above.
  band <- rep(NA_integer_, length(age_yr))
  ok <- !is.na(age_yr)
  band[ok & age_yr <   2]                  <- 1L
  band[ok & age_yr >=  2 & age_yr <  5]    <- 2L
  band[ok & age_yr >=  5 & age_yr < 10]    <- 3L
  band[ok & age_yr >= 10]                  <- 4L

  out <- rep(NA_integer_, length(age_yr))
  keep <- !is.na(band) & !is.na(sex_code)
  out[keep] <- band[keep] + (sex_code[keep] - 1L) * 4L

  # anyone with both age and sex observed must have landed in a group
  lost <- !is.na(age_yr) & !is.na(sex_code) & is.na(out)
  if (any(lost)) {
    stop(sum(lost), " observation(s) with non-missing age and sex were not ",
         "assigned a group. This should be impossible; please report it.",
         call. = FALSE)
  }

  attr(out, "labels") <- c("Male 0-1.9 yr", "Male 2-4.9 yr", "Male 5-9.9 yr",
                           "Male 10+ yr", "Female 0-1.9 yr", "Female 2-4.9 yr",
                           "Female 5-9.9 yr", "Female 10+ yr")
  out
}


#' Convert a sex variable to the recommended factor form
#'
#' Turns a numeric or character sex variable into a two-level factor with the
#' levels `"male"` and `"female"`. Doing this once near the top of an analysis
#' removes the 0/1 versus 1/2 ambiguity from every later call.
#'
#' @param x A numeric or character vector of sex codes.
#' @param male,female The value of `x` that means male and female respectively.
#'   Both are required: see the discussion in [eiof_age_sex_group()] for why
#'   this is not inferred.
#' @param labels The two level labels to use, male first.
#'
#' @return A factor with two levels, `NA` preserved.
#'
#' @examples
#' eiof_as_sex_factor(c(1, 2, 2, 1), male = 1, female = 2)
#' eiof_as_sex_factor(c(0, 1, 1), male = 1, female = 0)
#' eiof_as_sex_factor(c("M", "F"), male = "M", female = "F")
#'
#' @export
eiof_as_sex_factor <- function(x, male, female,
                               labels = c("male", "female")) {

  if (missing(male) || missing(female)) {
    stop("`male` and `female` are both required. reiof does not guess which ",
         "code means male: 1/2 and 0/1 are both common, and guessing wrong ",
         "would invert every downstream result silently.", call. = FALSE)
  }
  if (length(labels) != 2L) {
    stop("`labels` must have exactly two elements, male first.", call. = FALSE)
  }
  if (identical(as.character(male), as.character(female))) {
    stop("`male` and `female` must differ.", call. = FALSE)
  }

  out <- rep(NA_character_, length(x))
  out[!is.na(x) & x == male]   <- labels[1]
  out[!is.na(x) & x == female] <- labels[2]

  unmatched <- setdiff(unique(x[!is.na(x)]), c(male, female))
  if (length(unmatched)) {
    stop("these values match neither male nor female:\n  ",
         paste(unmatched, collapse = ", "),
         "\n  Recode them first. They are not dropped silently.",
         call. = FALSE)
  }

  factor(out, levels = labels)
}
