# plsSEMflow 0.1.0

## First CRAN Release

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
