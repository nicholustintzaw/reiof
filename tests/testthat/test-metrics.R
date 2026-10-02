# Absolute threshold (Table 3) and relative benchmark (Tables 4a-4g)

abs4 <- unname(eiof_lookup_cutoff("biodiv", 4, "abs"))

test_that("the absolute comparison is strict", {
  # published cut-offs carry a "<" sign: exactly on the threshold is not below
  v <- c(abs4 * (1 - 1e-9), abs4, abs4 * (1 + 1e-9))
  got <- eiof_absolute_threshold(v, rep(4L, 3), "biodiv")
  expect_equal(got, c(1L, 0L, 0L))
})

test_that("missing impacts and groups stay missing", {
  # In R, NA < x is NA, but a careless implementation can turn missing data
  # into an apparent threshold breach. These must be NA, never 0 or band 10.
  v <- c(abs4 / 2, NA, abs4 / 2)
  g <- c(4L, 4L, NA)
  expect_equal(eiof_absolute_threshold(v, g, "biodiv"), c(1L, NA, NA))
  expect_equal(eiof_relative_band(v, g, "biodiv"), c(1L, NA, NA))
})

test_that("every band is reachable for every demographic group", {
  cuts <- lapply(1:8, function(g)
    unname(eiof_lookup_cutoff("biodiv", g, "rel", percentile = 10)))

  for (g in 1:8) {
    p <- vapply(seq(10, 90, 10), function(q)
      unname(eiof_lookup_cutoff("biodiv", g, "rel", percentile = q)), numeric(1))

    # a value inside each of the ten bands
    v <- c(p[1] * 0.5,                       # band 1
           (p[-9] + p[-1]) / 2,              # bands 2-9
           p[9] * 1.5)                       # band 10
    got <- eiof_relative_band(v, rep(g, 10), "biodiv")
    expect_equal(got, 1:10, info = paste("group", g))
  }
})

test_that("a value exactly on a cut-point falls in the higher band", {
  p <- vapply(seq(10, 90, 10), function(q)
    unname(eiof_lookup_cutoff("biodiv", 4, "rel", percentile = q)), numeric(1))
  expect_equal(eiof_relative_band(p[1] * (1 - 1e-9), 4L, "biodiv"), 1L)
  expect_equal(eiof_relative_band(p[1], 4L, "biodiv"), 2L)   # on P10 -> band 2
  expect_equal(eiof_relative_band(p[9], 4L, "biodiv"), 10L)  # on P90 -> band 10
})

test_that("bands are monotone in the impact", {
  g <- rep(4L, 200)
  v <- sort(stats::rlnorm(200, log(8e-13), 1.2))
  b <- eiof_relative_band(v, g, "biodiv")
  expect_true(all(diff(b) >= 0))
})

test_that("midpoint returns the band midpoint percentile", {
  b <- eiof_relative_band(eiof_demo$biodiv,
                          eiof_age_sex_group(eiof_demo$age, eiof_demo$sex),
                          "biodiv")
  m <- eiof_relative_band(eiof_demo$biodiv,
                          eiof_age_sex_group(eiof_demo$age, eiof_demo$sex),
                          "biodiv", midpoint = TRUE)
  expect_equal(m, b * 10L - 5L)
})

test_that("all seven indicators run and return sensible shapes", {
  grp <- eiof_age_sex_group(eiof_demo$age, eiof_demo$sex)
  for (ind in eiof_indicators$indicator) {
    a <- eiof_absolute_threshold(eiof_demo[[ind]], grp, ind)
    b <- eiof_relative_band(eiof_demo[[ind]], grp, ind)
    expect_length(a, nrow(eiof_demo))
    expect_length(b, nrow(eiof_demo))
    expect_true(all(a %in% c(0L, 1L, NA)))
    expect_true(all(b %in% c(1:10, NA)))
  }
})

test_that("bad arguments are rejected clearly", {
  expect_error(eiof_absolute_threshold(1, 4L, "nope"), "must be one of")
  expect_error(eiof_absolute_threshold(factor("1"), 4L, "ghg"), "factor")
  expect_error(eiof_absolute_threshold("1", 4L, "ghg"), "character")
  expect_error(eiof_absolute_threshold(1, 99L, "ghg"), "outside 1-8")
  expect_error(eiof_absolute_threshold(c(1, 2), 4L, "ghg"), "length")
})

test_that("the absolute threshold sits where expected in the relative scale", {
  # Documented in the vignette and in the Stata companion: for GHG the
  # planetary-boundary threshold lies below the whole reference distribution,
  # and for water above it.
  for (g in 1:8) {
    thr_ghg <- unname(eiof_lookup_cutoff("ghg", g, "abs"))
    p10_ghg <- unname(eiof_lookup_cutoff("ghg", g, "rel", percentile = 10))
    expect_lt(thr_ghg, p10_ghg)

    thr_wat <- unname(eiof_lookup_cutoff("water", g, "abs"))
    p90_wat <- unname(eiof_lookup_cutoff("water", g, "rel", percentile = 90))
    expect_gt(thr_wat, p90_wat)
  }
})
