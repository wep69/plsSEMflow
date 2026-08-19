# plsSEMflow implementation matrix

This matrix distinguishes implemented package infrastructure from optional external engines and from workflows that require local runtime validation.

| Phase | Scope | Implementation status | Primary functions / artifacts |
|---|---|---|---|
| A | Architecture, classes, DSL, capability registry | Implemented | `pls_model()`, `pls_measurement()`, `pls_structural()`, `pls_capabilities()`, audit trail |
| B | PLS core, assessment, bootstrap | Implemented | native PLS-PM, `pls_assess()`, `pls_bootstrap()` |
| C | Mediation, moderation, HOC, prediction | Implemented | `pls_effects()`, `pls_moderation_test()`, HOC DSL/backends, `pls_predict()` |
| D | MICOM, MGA, heterogeneity, endogeneity | Implemented through native and optional specialist backends | `pls_mga()`, `pls_micom()`, `pls_mgd()`, `pls_rebus()`, `pls_endogeneity()` |
| E | Ordinal, categorical, nonlinear, multilevel | Implemented as optional-backend workflows plus native sensitivity tools | `pls_ordinal()`, `pls_nonlinear_test()`, `pls_nonlinear_effects()`, `pls_multilevel()` |
| F | Bayesian companion and experimental frontier | Implemented conservatively | `pls_bayes_compare()` is Bayesian CB-SEM companion; no false claim of native Bayesian PLS-SEM |
| G | Publication plots, tables, reports, teaching | Implemented | `pls_plot()`, `pls_table()`, advanced outputs, `pls_report()`, `pls_tour()` |
| H | Simulation, frozen tests, cross-engine validation | Infrastructure implemented | `pls_simulate()`, `pls_validation_case()`, `pls_validate_cross_engine()`, testthat, validation docs |
| I | Vignettes, pkgdown, CI, release scaffolding | Implemented as source infrastructure | 21 Rmd vignettes, `_pkgdown.yml`, GitHub Actions, roxygen source |

## Bootstrap as a transversal layer

For coefficient-bearing tables and figures, users can request bootstrap directly and choose the number of resamples. The relevant functions record `R`, confidence level, and interval method in returned data or figure metadata. Existing `plssem_bootstrap` objects can be reused to avoid unnecessary recomputation.

## Runtime caveat for this development artifact

The package source is designed for `roxygen2`-generated documentation and NAMESPACE. A machine with R and the selected optional backends is required to run `roxygen2::roxygenise()`, `devtools::document()`, unit tests, vignettes, and `R CMD check`. The development environment used to assemble this artifact did not provide an R executable, so the source has been statically audited but not represented as a completed local R CMD check.
