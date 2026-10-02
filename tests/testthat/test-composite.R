abs4 <- unname(eiof_lookup_cutoff("biodiv", 4, "abs"))
p30  <- unname(eiof_lookup_cutoff("biodiv", 4, "rel", percentile = 30))

test_that("HESD produces the four categories of Table 5a", {
  bio  <- c(abs4 / 2, abs4 / 2, abs4 * 2, abs4 * 2)
  gdqs <- c(30,       10,       30,       10)
  got <- eiof_hesd(bio, gdqs, rep(4L, 4))
  expect_equal(as.integer(got), 1:4)
  expect_equal(levels(got)[1], "Healthy and env. sustainable")
})

test_that("HDLEI produces the four categories of Table 5b", {
  bio  <- c(p30 / 2, p30 / 2, p30 * 50, p30 * 50)
  gdqs <- c(30,      10,      30,       10)
  got <- eiof_hdlei(bio, gdqs, rep(4L, 4))
  expect_equal(as.integer(got), 1:4)
})

test_that("a GDQS of exactly 23 counts as healthy", {
  expect_equal(eiof_healthy_diet(c(22.999, 23, 23.001)), c(0L, 1L, 1L))
})

test_that("the binary form is exactly category 1", {
  grp <- eiof_age_sex_group(eiof_demo$age, eiof_demo$sex)
  suppressMessages({
    cat4 <- eiof_hesd(eiof_demo$biodiv, eiof_demo$gdqs, grp, form = "category")
    bin  <- eiof_hesd(eiof_demo$biodiv, eiof_demo$gdqs, grp, form = "binary")
  })
  expect_equal(bin, ifelse(is.na(cat4), NA_integer_,
                           as.integer(as.integer(cat4) == 1L)))
})

test_that("sub-indicators equal the standalone functions", {
  grp <- eiof_age_sex_group(eiof_demo$age, eiof_demo$sex)
  eligible <- !(grp %in% c(1L, 5L))

  suppressMessages({
    h <- eiof_hesd(eiof_demo$biodiv, eiof_demo$gdqs, grp, form = "components")
    d <- eiof_hdlei(eiof_demo$biodiv, eiof_demo$gdqs, grp, form = "components")
  })

  standalone_healthy <- eiof_healthy_diet(eiof_demo$gdqs)
  standalone_sust    <- eiof_sustainable_diet(eiof_demo$biodiv, grp)
  standalone_low     <- eiof_low_impact(eiof_demo$biodiv, grp)

  expect_equal(h$healthy[eligible], standalone_healthy[eligible])
  expect_equal(h$env[eligible],     standalone_sust[eligible])
  expect_equal(d$env[eligible],     standalone_low[eligible])
  expect_equal(h$healthy, d$healthy)
})

test_that("composites are NA below age 2", {
  grp  <- c(1L, 5L, 2L, 6L)
  bio  <- rep(1e-14, 4)        # comfortably below every cut-off
  gdqs <- rep(30, 4)
  suppressMessages({
    h <- eiof_hesd(bio, gdqs, grp, form = "binary")
    d <- eiof_hdlei(bio, gdqs, grp, form = "binary")
  })
  expect_equal(h, c(NA, NA, 1L, 1L))
  expect_equal(d, c(NA, NA, 1L, 1L))
})

test_that("missing inputs propagate to a missing category", {
  grp <- rep(4L, 3)
  suppressMessages(
    got <- eiof_hesd(c(abs4 / 2, NA, abs4 / 2), c(30, 30, NA), grp,
                     form = "binary"))
  expect_equal(got, c(1L, NA, NA))
})

test_that("HDLEI is nested inside HESD", {
  # The relative P30 cut-off is stricter than the absolute threshold in every
  # demographic group, by the same constant factor, so hdlei == 1 must imply
  # hesd == 1. This is structural, not a property of any particular sample.
  for (g in 1:8) {
    a <- unname(eiof_lookup_cutoff("biodiv", g, "abs"))
    r <- unname(eiof_lookup_cutoff("biodiv", g, "rel", percentile = 30))
    expect_lt(r, a)
  }

  # the ratio is the same in all eight groups
  ratios <- vapply(1:8, function(g) {
    unname(eiof_lookup_cutoff("biodiv", g, "abs")) /
      unname(eiof_lookup_cutoff("biodiv", g, "rel", percentile = 30))
  }, numeric(1))
  expect_equal(diff(range(ratios)), 0, tolerance = 1e-9)

  # and it shows up in the data
  set.seed(7)
  n <- 2000
  grp  <- sample(c(2L, 3L, 4L, 6L, 7L, 8L), n, replace = TRUE)
  bio  <- stats::rlnorm(n, log(6e-13), 1.2)
  gdqs <- stats::runif(n, 12, 38)
  suppressMessages({
    h <- eiof_hesd(bio, gdqs, grp, form = "binary")
    d <- eiof_hdlei(bio, gdqs, grp, form = "binary")
  })
  expect_true(all(h[which(d == 1L)] == 1L))
  expect_gte(sum(h, na.rm = TRUE), sum(d, na.rm = TRUE))
})

test_that("a non-default percentile is flagged as a sensitivity analysis", {
  expect_message(eiof_low_impact(rep(1e-13, 2), rep(4L, 2), percentile = 50),
                 "not the published HDLEI criterion")
  expect_silent(eiof_low_impact(rep(1e-13, 2), rep(4L, 2)))
  expect_error(eiof_low_impact(1e-13, 4L, percentile = 35), "multiple of 10")
})

test_that("components form returns all four pieces", {
  grp <- rep(4L, 4)
  suppressMessages(
    x <- eiof_hesd(c(abs4 / 2, abs4 / 2, abs4 * 2, abs4 * 2),
                   c(30, 10, 30, 10), grp, form = "components"))
  expect_s3_class(x, "data.frame")
  expect_named(x, c("healthy", "env", "category", "binary"))
  expect_equal(nrow(x), 4L)
})
