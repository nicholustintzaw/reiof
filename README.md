---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->



# reiof

<!-- badges: start -->
[![Project Status: WIP](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/nicholustintzaw/reiof/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/nicholustintzaw/reiof/actions/workflows/R-CMD-check.yaml)
[![pkgdown](https://github.com/nicholustintzaw/reiof/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/nicholustintzaw/reiof/actions/workflows/pkgdown.yaml)
[![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

Dietary environmental impact metrics from Intake's Environmental Impacts of
Foods framework, for R.

**`reiof` does not contain the EIOF database.** It implements the metric
*cut-offs* defined in the Intake methods paper and applies them to impact
values you have already derived. Matching food items to impact factors,
applying edible-portion, processing and cooking-yield adjustments, and summing
to the respondent-day is upstream work that this package starts after. It also
does not calculate the Global Diet Quality Score; the composite metrics take a
GDQS value as input.

## What it does

| Family | Question | Source | Function |
|---|---|---|---|
| Absolute threshold | Is this diet below the per capita daily share of a planetary boundary? | Table 3 | `eiof_absolute_threshold()` |
| Relative benchmark | Where does this diet sit in a fixed global reference distribution? | Tables 4a–4g | `eiof_relative_band()` |
| Composite — HESD | Healthy *and* environmentally sustainable? | Table 5a | `eiof_hesd()` |
| Composite — HDLEI | Healthy *and* low environmental impact? | Table 5b | `eiof_hdlei()` |

Seven indicators: total, arable and pasture land use; greenhouse gases;
eutrophication potential; water use; biodiversity loss. All cut-offs are
age- and sex-specific.

## Installation

```r
# install.packages("pak")
pak::pak("nicholustintzaw/reiof")
```

**No dependencies.** `reiof` imports nothing, so it installs with no dependency
tree. The consequence is a plain vector API: pass `df$biodiv`, not `biodiv`.

## Usage


```r
library(reiof)

# 1. check the data before computing anything
report <- eiof_check_data(
  eiof_demo,
  impacts = c(biodiv = "biodiv", ghg = "ghg"),
  age = "age", sex = "sex", gdqs = "gdqs"
)
summary(report)
#> PASS - 0 error, 0 warning, 13 note
```


```r
# 2. the demographic group, required by everything else
grp <- eiof_age_sex_group(age = eiof_demo$age, sex = eiof_demo$sex)

# 3. the metrics
ghg_abs    <- eiof_absolute_threshold(eiof_demo$ghg, grp, "ghg")
biodiv_bnd <- eiof_relative_band(eiof_demo$biodiv, grp, "biodiv")
hesd       <- eiof_hesd(eiof_demo$biodiv, eiof_demo$gdqs, grp, form = "binary")
#> 15 observation(s) aged under 2 years set to NA: the GDQS is not validated below age 2, so the HESD metric does not apply to them.

round(100 * c(ghg_below_threshold = mean(ghg_abs, na.rm = TRUE),
              hesd                = mean(hesd,    na.rm = TRUE)), 1)
#> ghg_below_threshold                hesd 
#>                22.2                36.5
```

Each composite also returns its two criteria separately, which is usually what
you want for reporting:


```r
parts <- eiof_hesd(eiof_demo$biodiv, eiof_demo$gdqs, grp, form = "components")
#> 15 observation(s) aged under 2 years set to NA: the GDQS is not validated below age 2, so the HESD metric does not apply to them.
head(parts, 3)
#>   healthy env                         category binary
#> 1       1   0 Healthy but not env. sustainable      0
#> 2       0   1 Not healthy but env. sustainable      0
#> 3       1   1     Healthy and env. sustainable      1
```

## A note on the `sex` variable

A **factor is the recommended input**; character works the same way. Numeric is
deliberately refused unless you say which code means male:


```r
eiof_age_sex_group(age = c(1.5, 40), sex = c(1, 0))
#> Error: `sex` is numeric, so reiof cannot tell which code means male.
#>   Observed codes: 0, 1
#>   Survey data uses 1/2 and 0/1 about equally often, and guessing wrong would invert every result without any error.
#>   Either state the codes:
#>     eiof_age_sex_group(age, sex, male = 1, female = 2)
#>   or convert once, which is the recommended route:
#>     dat$sex <- eiof_as_sex_factor(dat$sex, male = 1, female = 2)
```

Survey data uses `1/2` and `0/1` about equally often, and they cannot be told
apart from the values alone. Guessing wrong would invert every result silently.
Convert once instead:


```r
eiof_as_sex_factor(c(1, 0), male = 1, female = 0)
#> [1] male   female
#> Levels: male female
```

## Three things to know before reporting results

1. **Eutrophication units are unresolved.** The relative benchmarks carry an
   inferred ÷1000 rescaling that Intake has not confirmed. Isolated in
   `eiof_cutoffs$scale_to_db_unit` so it can be switched off in one place.
2. **A low relative band is not evidence of sustainability.** For greenhouse
   gases even band 1 exceeds the planetary boundary; for water even band 10 is
   below it.
3. **HDLEI is nested inside HESD.** The HDLEI cut-off is uniformly 1.52×
   stricter, so every HDLEI diet is also a HESD diet. They are not two
   independent dimensions.

All three are explained in `vignette("reiof")`.

## Documentation

- `vignette("reiof")` — EIOF, reiof, and step-by-step usage
- `vignette("relative-benchmarks")` — what the reference distribution is, and is not
- `vignette("troubleshooting")` — a broken dataset, the errors, and the fixes

## Data quality checks

`eiof_check_data()` runs six component checks covering variable type,
plausibility, numerical precision and unit magnitude. Two are worth singling
out:

**Precision.** R's `numeric` is a double, so a biodiversity value of
`3.6896e-13` is comfortably representable and `<` comparison is exact — the
small exponent is not the problem. The risk is ingestion fidelity: Excel
re-saving displayed digits, CSV exports written at six significant digits,
Stata `float` storage, database `REAL` columns. The check measures significant
digits actually retained, and counts how many observations sit close enough to
their own cut-off that rounding could flip the classification.

**Unit magnitude.** Compares your median against the published global
reference and names the likely conversion when the ratio lands near a known
factor:


```r
eiof_check_magnitude(eiof_demo$ghg * 1000, "ghg", grp, name = "ghg_in_grams")
#> reiof data quality report
#> 
#>   ERROR    0
#>   WARNING  1
#>   NOTE     0
#> 
#>   WARNING  ghg_in_grams   magnitude    median 3061 vs expected 5.547 for this demographic mix (ratio 552).
#>                                        Consistent with a g <-> kg unit error. Expected unit: kg CO2eq.
#> 
#>   No blocking problems.
```

## Source

Intake (2026). *Methods for Assessing Healthy, Environmentally Sustainable
Diets Globally.* Tables 3, 4a–4g, 5a and 5b.

The cut-offs ship as `eiof_cutoffs`, a 616-row long-format dataset built from
the same source file as a companion Stata implementation, so the two cannot
drift apart at the data layer.

## Related

[`{riycf}`](https://github.com/nutriverse/riycf) — infant and young child
feeding indicators, same dependency-free design.
