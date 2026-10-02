# Values hand-typed from the printed tables, as an independent check on the
# programmatic transcription in data-raw/. Compared with identical(), not
# all.equal(): the cut-offs are published to 11 significant digits and a
# double holds them exactly, so anything less than exact equality means
# precision was lost somewhere in the build.

test_that("the cut-off dataset has the expected shape", {
  expect_equal(nrow(eiof_cutoffs), 616L)
  expect_equal(sum(eiof_cutoffs$metric == "abs"), 56L)
  expect_equal(sum(eiof_cutoffs$metric == "rel"), 560L)
  expect_equal(length(unique(eiof_cutoffs$indicator)), 7L)
  expect_true(all(eiof_cutoffs$demogroup %in% 1:8))
  expect_false(anyNA(eiof_cutoffs$value))
  expect_true(all(eiof_cutoffs$value > 0))
})

abs_val <- function(ind, g) {
  eiof_cutoffs$value_published[eiof_cutoffs$metric == "abs" &
    eiof_cutoffs$indicator == ind & eiof_cutoffs$demogroup == g]
}
rel_val <- function(ind, g, p) {
  eiof_cutoffs$value_published[eiof_cutoffs$metric == "rel" &
    eiof_cutoffs$indicator == ind & eiof_cutoffs$demogroup == g &
    !is.na(eiof_cutoffs$percentile) & eiof_cutoffs$percentile == p]
}

test_that("Table 3 values match the printed table exactly", {
  expect_identical(abs_val("landtotal",   1), 4.0898629907)
  expect_identical(abs_val("landarable",  4), 5.5735931918)
  expect_identical(abs_val("landpasture", 8), 6.9692717624)
  expect_identical(abs_val("ghg",         6), 0.8845537944)
  expect_identical(abs_val("eutroph",     4), 0.0267453228)
  expect_identical(abs_val("water",       8), 1726.3821057877)
  expect_identical(abs_val("biodiv",      4), 1.2844358718e-12)
  expect_identical(abs_val("biodiv",      1), 3.6896027444e-13)
})

test_that("Tables 4a-4g values match the printed tables exactly", {
  expect_identical(rel_val("landtotal",   1, 10),  1.1645159358)
  expect_identical(rel_val("landtotal",   4, 50), 15.4981270626)
  expect_identical(rel_val("landtotal",   8, 90), 30.1963086516)
  expect_identical(rel_val("landarable",  1, 10),  1.1010661656)
  expect_identical(rel_val("landarable",  6, 50),  2.2621082746)
  expect_identical(rel_val("landpasture", 1, 10),  0.0634497703)
  expect_identical(rel_val("landpasture", 8, 90), 23.2607981018)
  expect_identical(rel_val("ghg",         1, 10),  0.8921758398)
  expect_identical(rel_val("ghg",         4, 10),  3.1058700138)
  expect_identical(rel_val("water",       7, 80), 819.3029666619)
  expect_identical(rel_val("water",       1, 10), 213.1623613755)
  expect_identical(rel_val("biodiv",      1, 10),  2.0227051058e-13)
  expect_identical(rel_val("biodiv",      2, 30),  3.7599901323e-13)
  expect_identical(rel_val("biodiv",      8, 30),  6.7861513180e-13)
})

test_that("relative eutrophication carries the documented rescaling", {
  # published value as printed, and the harmonised value the functions use
  expect_identical(rel_val("eutroph", 2, 20), 8.3451357297)
  v <- eiof_cutoffs$value[eiof_cutoffs$metric == "rel" &
    eiof_cutoffs$indicator == "eutroph" & eiof_cutoffs$demogroup == 2 &
    !is.na(eiof_cutoffs$percentile) & eiof_cutoffs$percentile == 20]
  expect_equal(v, 0.0083451357297, tolerance = 1e-15)

  # and nothing else is rescaled
  other <- eiof_cutoffs$scale_to_db_unit[
    !(eiof_cutoffs$metric == "rel" & eiof_cutoffs$indicator == "eutroph")]
  expect_true(all(other == 1))
  expect_equal(sum(eiof_cutoffs$scale_to_db_unit != 1), 80L)
})

test_that("relative cut-points are strictly increasing, row 10 repeats P90", {
  rel <- eiof_cutoffs[eiof_cutoffs$metric == "rel", ]
  series <- split(rel, list(rel$indicator, rel$demogroup), drop = TRUE)
  expect_equal(length(series), 56L)
  for (s in series) {
    s <- s[order(s$row_order), ]
    expect_equal(nrow(s), 10L)
    expect_true(all(diff(s$value[1:9]) > 0))
    expect_equal(s$value[10], s$value[9])
  }
})

test_that("Table 5 cut-offs trace to Tables 3 and 4g", {
  # Table 5a prints the ABSOLUTE biodiversity threshold
  expect_identical(abs_val("biodiv", 2), 5.7244838747e-13)
  expect_identical(abs_val("biodiv", 3), 7.8836516181e-13)
  expect_identical(abs_val("biodiv", 6), 5.3018709712e-13)
  expect_identical(abs_val("biodiv", 8), 1.0331732910e-12)

  # Table 5b prints the 30th-percentile RELATIVE benchmark
  expect_identical(rel_val("biodiv", 3, 30), 5.1781877527e-13)
  expect_identical(rel_val("biodiv", 4, 30), 8.4365094030e-13)
  expect_identical(rel_val("biodiv", 6, 30), 3.4824069682e-13)
  expect_identical(rel_val("biodiv", 7, 30), 4.7542425566e-13)
})

test_that("eiof_lookup_cutoff returns the published values", {
  expect_equal(unname(eiof_lookup_cutoff("biodiv", 4, "abs", published = TRUE)),
               1.2844358718e-12)
  expect_equal(unname(eiof_lookup_cutoff("biodiv", 4, "rel", 30,
                                         published = TRUE)),
               8.4365094030e-13)
  expect_length(eiof_lookup_cutoff("ghg", metric = "abs"), 8L)
  expect_error(eiof_lookup_cutoff("ghg", metric = "rel"), "percentile.*required")
  expect_error(eiof_lookup_cutoff("nonsense", metric = "abs"), "must be one of")
})
