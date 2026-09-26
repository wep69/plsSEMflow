# plsSEMflow 0.1.0

## First CRAN Release

### Defect fixes from the behavioural audit

* `pls_compare_engines()` now returns the `comparison` table and a `reference`
  engine name, which the function name always promised.
* `pls_validate_cross_engine()` no longer fails with
  `replacement has 1 row, data has 0` when a non-native engine is requested.
  The cSEM adapter now reads the square `Estimates$Path_estimates` matrix that
  cSEM 0.6 returns, the `length(NA)` guard that let an `NA` column index slip
  through was corrected, and an engine that yields no comparable paths is now
  named in the error message instead of surfacing a cryptic `rbind` failure.
* `pls_mgd()` takes `(model, data, group)`, mirroring `pls_mga()`, and routes
  through `cSEM::csem()` with one data frame per group, which is what produces
  the `cSEMResults_multi` object that `cSEM::testMGD()` requires. The previous
  single-fit signature could not reach a working call.
* `pls_cvpat()` accepts a second fit, because `cSEM::testCVPAT()` compares two
  models. A single fit is now refused with an actionable message.
* `pls_endogeneity()` explains that cSEM records instruments at estimation time,
  so no test-time argument can supply them, instead of surfacing cSEM's internal
  "Instruments required".
* `pls_rebus()` gained `n_classes`. `plspm::rebus.pls()` reads the number of
  classes from the console with `scan(file = "")`, which blocks in a
  non-interactive session; outside an interactive one the function now refuses
  with instructions rather than hanging.
* `pls_bootstrap(unit = "cluster")` validates that `cluster` is a single column
  name and lists the available columns when it is not, instead of failing with
  an internal coercion message. The result now records the grouping actually
  used in `cluster`, the name in `cluster_name`, and the count in
  `n_clusters`.
* Functions that accept `seed` no longer rewind the caller's random stream:
  `pls_simulate()`, `pls_bootstrap()`, `pls_predict()`, `pls_mga()`,
  `pls_moderation_test()` and `pls_nonlinear_test()` restore `.Random.seed` on
  exit.
* `pls_moderation_test()` names the construct that is missing or non-numeric,
  which previously surfaced as a row-count mismatch from the interaction term.
* `pls_ordinal()` gives `ordered` a default of `character(0)` instead of
  requiring it.
* `pls_simulate()` documents that `path_values` are structural coefficients
  rather than the correlations the engine recovers, and now returns the implied
  correlation matrix in the `implied_cor` attribute.
* `pls_syntax()` documents that `dialect` only changes the higher-order line.

### Features

* Integrated workflow for partial least squares structural equation modeling (PLS-SEM).
* Native continuous linear PLS-PM engine for R.
* Semantic model specification for reflective, composite, formative, mediation, moderation, nonlinear and higher-order relations.
* User-controlled bootstrap with path, loading and weight confidence intervals.
* Measurement and structural assessment helpers.
* Prediction, multigroup, advanced-backend adapters and publication outputs.
* Agronomic teaching datasets and extended foundations-to-advanced tutorial.
* REBUS-PLS routing and explicit external FIMIX-PLS/PLS-POS workflow descriptors.
* Frozen simulation and cross-engine validation scaffolds.
* Advanced publication coefficient/MGA plots and tables.
* Direct, indirect and total-effect bootstrap intervals for native mediation workflows.
* Public API example catalog with at least three vignette examples per exported function.
* Optional adapters to cSEM, seminr, plssem, plspm, lavaan, and blavaan.

## Validation

* R CMD check --as-cran: 0 ERRORs, 0 WARNINGs, 0 NOTEs.
* testthat: 25 tests, 0 failures, 0 errors, 0 skips.
* 21 vignettes built successfully.
* win-builder R-devel and R-release: passed.
* CI GitHub Actions configured (Windows, macOS, Ubuntu).
