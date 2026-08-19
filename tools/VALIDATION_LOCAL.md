# Local validation of `plsSEMflow`

This package was designed so that the core does not require Python, Julia, Java, SmartPLS, WarpPLS, or ADANCO.

## 1. Restore a clean R environment

Use R >= 4.1.0. Install at minimum `devtools`, `roxygen2`, and `testthat`. Optional backends should be installed individually according to the modules that will be tested.

## 2. Regenerate documentation and NAMESPACE

```r
roxygen2::roxygenise()
```

`NAMESPACE` is managed from roxygen2 tags and must not be edited by hand.

## 3. Install the package

```r
devtools::install(dependencies = FALSE)
```

For full optional validation:

```r
install.packages(c("cSEM", "seminr", "plssem", "plspm", "lavaan",
                   "ggplot2", "knitr", "rmarkdown", "testthat"))
```

Install `blavaan` only for the Bayesian companion module.

## 4. Core tests

```r
devtools::test()
devtools::check(document = TRUE, manual = TRUE)
```

## 5. Native numerical checks

Run the frozen agronomic examples and verify:

- convergence is achieved;
- dominant loadings are oriented positively;
- path coefficients have the expected direction in frozen simulated data;
- bootstrap intervals reproduce with a fixed seed;
- changing `R` changes Monte Carlo stability but not the model specification;
- plots and tables report the selected CI method and bootstrap replication count;
- reliability, AVE, HTMT, R2, f2 and VIF are numerically finite when defined.

## 6. Cross-engine golden tests

For models with equivalent parameterization, compare native results against `cSEM` and `seminr`. Do not demand exact equality when construct conceptualization, disattenuation, sign handling, weighting scheme, or standardization differs. Store tolerances and reasons in a validation manifest.

Recommended frozen scenarios:

1. two-construct reflective model;
2. reflective mediation model;
3. mixed reflective/composite model;
4. formative collinearity scenario;
5. moderation model;
6. ordinal model via `plssem`;
7. multilevel model via `plssem`;
8. multigroup model with MICOM and MGD via `cSEM`;
9. nonlinear model via `cSEM` and `plssem`;
10. predictive model with repeated cross-validation.

## 7. Bootstrap validation

For each scenario, evaluate at least `R = 199`, `999`, and a larger final value selected for the study. Check CI coverage in simulation, Monte Carlo variability, seed reproducibility, inadmissible resamples, and computational time. Final manuscript values should be selected by the analyst rather than imposed by the package.

## 8. Documentation tests

```r
devtools::run_examples()
devtools::build_vignettes()
pkgdown::build_site()
```

Long vignettes should use small demonstration bootstrap values in evaluated chunks and clearly show larger analysis-scale values in non-evaluated chunks.

## 9. Publication-output checks

Export path diagrams and coefficient plots to PDF/SVG, and raster figures to TIFF/PNG at 600 dpi. Confirm that labels are legible, confidence intervals are visible, no values are clipped, and tables retain unrounded numeric values before rendering.

## 10. Optional backend checks

```r
pls_capabilities()
```

The package must fail informatively when an optional engine is unavailable. Absence of Python or Julia must never prevent installation or use of the native R core.

## 11. Reproduce the source-only quality gates

Before running R, the bundled static checker can be executed with Python when available:

```bash
python tools/static_validate.py
```

It verifies source delimiters, duplicate function names, NAMESPACE/export consistency, bundled agronomic datasets, vignette count, at-least-three vignette calls for every exported API function, user-controlled bootstrap hooks in publication output, and the absence of mandatory Python/Julia bridges. This is a **static** quality gate and does not replace R parsing or execution.

## 12. Simulation recovery and interval coverage

```r
m <- pls_model(
  pls_measurement(
    pls_reflective("SOIL", c("s1","s2","s3")),
    pls_reflective("NUTR", c("n1","n2","n3")),
    pls_reflective("YIELD", c("y1","y2","y3"))
  ),
  pls_structural(
    pls_path("SOIL","NUTR"),
    pls_path("NUTR","YIELD")
  )
)

truth <- c("SOIL -> NUTR"=.50, "NUTR -> YIELD"=.45)
val <- pls_validation_case(m, n=600, path_values=truth,
                           R=1999, seed=260819)
print(val)
```

For a software-paper simulation battery, repeat frozen scenario IDs rather than relying on one simulated sample. At minimum vary sample size, loading strength, path strength, multicollinearity, misspecification, missingness, ordinal treatment, nonlinearity, group heterogeneity, and clustered sampling. Record bias, RMSE, interval coverage, convergence, runtime, and memory where applicable.

## 13. External and cross-engine validation

When `cSEM` is installed and the model is parameterized comparably:

```r
cmp <- pls_validate_cross_engine(
  m,
  pls_simulate(m, n=1000, path_values=truth, seed=260819),
  engines=c("native","cSEM"),
  tolerance=.05
)
print(cmp)
```

Do not define a universal tolerance without first checking scaling, sign orientation, weighting approach, factor/composite conceptualization, disattenuation, and the actual target estimand.

## 14. Final release gate

Do not describe a source archive as release-ready until all applicable items pass:

- [ ] `roxygen2::roxygenise()` regenerates documentation and NAMESPACE without unexpected changes;
- [ ] package installs from a clean library;
- [ ] core `testthat` suite has 0 failures and 0 errors;
- [ ] all 21 vignettes build;
- [ ] primary examples run;
- [ ] native numerical validation is frozen and reproducible;
- [ ] advertised Tier-1 optional engines have compatible golden tests;
- [ ] Tier-2/Tier-3 backends are either tested or explicitly labeled unvalidated/experimental;
- [ ] bootstrap coefficient CIs reproduce under fixed seeds;
- [ ] cluster bootstrap is checked on multilevel teaching scenarios;
- [ ] publication PDF/SVG/TIFF/PNG outputs open correctly;
- [ ] table exports preserve numeric values before formatting;
- [ ] `R CMD build .` passes;
- [ ] `R CMD check --as-cran <tarball>` has no ERRORs or WARNINGs and every NOTE is reviewed;
- [ ] pkgdown site builds;
- [ ] exact source archive and SHA-256 checksum are frozen;
- [ ] `sessionInfo()` and dependency versions are archived.
