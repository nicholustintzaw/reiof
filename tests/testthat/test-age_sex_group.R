test_that("half-open age boundaries assign correctly", {
  # The paper prints "0-1.9", "2-4.9", "5-9.9", "10+". Read as half-open
  # intervals, a child aged 1.95 is in the first group. Testing age <= 1.9
  # would leave everyone between 1.9 and 2.0 unassigned.
  age  <- c(0, 1.9, 1.95, 1.999, 2, 4.9, 4.999, 5, 9.999, 10, 85)
  want <- c(1, 1,   1,    1,     2, 2,   2,     3, 3,     4,  4)
  got  <- eiof_age_sex_group(age, rep("male", length(age)))
  expect_equal(as.integer(got), as.integer(want))
})

test_that("female groups are the male groups plus four", {
  age <- c(1.5, 3, 7, 40)
  m <- eiof_age_sex_group(age, rep("male", 4))
  f <- eiof_age_sex_group(age, rep("female", 4))
  expect_equal(as.integer(f), as.integer(m) + 4L)
  expect_equal(as.integer(m), 1:4)
})

test_that("age in months converts correctly", {
  agem <- c(23, 24, 59, 60, 119, 120)
  want <- c(1,  2,  2,  3,  3,   4)
  got  <- eiof_age_sex_group(agem, rep("male", 6), age_units = "months")
  expect_equal(as.integer(got), as.integer(want))
})

test_that("a factor is accepted and is the recommended input", {
  s <- factor(c("male", "female"), levels = c("male", "female"))
  expect_equal(as.integer(eiof_age_sex_group(c(40, 40), s)), c(4L, 8L))

  # level order must not matter
  s2 <- factor(c("male", "female"), levels = c("female", "male"))
  expect_equal(as.integer(eiof_age_sex_group(c(40, 40), s2)), c(4L, 8L))
})

test_that("character sex is matched case- and space-insensitively", {
  expect_equal(as.integer(eiof_age_sex_group(c(40, 40), c("M", "F"))),
               c(4L, 8L))
  expect_equal(as.integer(eiof_age_sex_group(c(40, 40), c("MALE", "female"))),
               c(4L, 8L))
  expect_equal(as.integer(eiof_age_sex_group(c(40, 40), c(" male ", "Girl"))),
               c(4L, 8L))
})

test_that("numeric sex is refused unless the codes are stated", {
  # This is the central safety behaviour: 1/2 and 0/1 are both common and
  # cannot be told apart from the values, so guessing would silently invert
  # every result.
  expect_error(eiof_age_sex_group(c(40, 40), c(1, 0)),
               "cannot tell which code means male")
  expect_error(eiof_age_sex_group(c(40, 40), c(1, 2)),
               "cannot tell which code means male")

  # stated explicitly, it works - and the two codings give opposite answers,
  # which is exactly why guessing is not acceptable
  expect_equal(as.integer(eiof_age_sex_group(c(40, 40), c(1, 0),
                                             male = 1, female = 0)),
               c(4L, 8L))
  expect_equal(as.integer(eiof_age_sex_group(c(40, 40), c(1, 0),
                                             male = 0, female = 1)),
               c(8L, 4L))
})

test_that("unrecognised sex values error rather than being dropped", {
  expect_error(eiof_age_sex_group(c(40, 40, 40), c("male", "female", "other")),
               "not recognised")
  expect_error(eiof_age_sex_group(c(40, 40), c(1, 9), male = 1, female = 2),
               "match neither")
  # explicit labels let an unusual coding through
  expect_equal(
    as.integer(eiof_age_sex_group(c(40, 40), c("H", "M"),
                                  male = "H", female = "M")),
    c(4L, 8L))
})

test_that("missing age or sex gives a missing group", {
  g <- eiof_age_sex_group(c(40, NA, 40), c("male", "male", NA))
  expect_equal(as.integer(g), c(4L, NA, NA))
})

test_that("bad input is rejected", {
  expect_error(eiof_age_sex_group(c(-1, 40), c("male", "male")), "negative")
  expect_error(eiof_age_sex_group(c(40), c("male", "female")), "same length")
  expect_error(eiof_age_sex_group(factor("40"), "male"), "factor")
  expect_warning(eiof_age_sex_group(c(400), "male"), "exceed 120")
  expect_error(eiof_age_sex_group(c(40, 40), c("m", "f"), male = "m"),
               "both `male` and `female`")
})

test_that("eiof_as_sex_factor converts and refuses to guess", {
  f <- eiof_as_sex_factor(c(1, 2, 2, NA), male = 1, female = 2)
  expect_s3_class(f, "factor")
  expect_equal(levels(f), c("male", "female"))
  expect_equal(as.character(f), c("male", "female", "female", NA))

  # the opposite coding, same function
  f2 <- eiof_as_sex_factor(c(0, 1), male = 1, female = 0)
  expect_equal(as.character(f2), c("female", "male"))

  expect_error(eiof_as_sex_factor(c(1, 2)), "both required")
  expect_error(eiof_as_sex_factor(c(1, 2, 7), male = 1, female = 2),
               "match neither")

  # round trip: converting then grouping equals grouping with explicit codes
  s <- c(1, 2, 1, 2)
  a <- c(40, 40, 3, 3)
  expect_equal(
    as.integer(eiof_age_sex_group(a, eiof_as_sex_factor(s, 1, 2))),
    as.integer(eiof_age_sex_group(a, s, male = 1, female = 2)))
})
