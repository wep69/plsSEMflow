# plsSEMflow

`plsSEMflow` is an integrated R workflow for partial least squares structural equation modeling (PLS-SEM), designed for research, teaching, model auditing, prediction, and publication-ready communication.

The package is intentionally **R-first**. Python, Julia, SmartPLS, WarpPLS, ADANCO, and spreadsheet-based tools are optional comparison or interoperability targets, not mandatory runtime requirements.

## Design principles

1. Specify the scientific model before choosing an engine.
2. Distinguish common factors, composites, and formative specifications.
3. Diagnose the measurement model before interpreting structural paths.
4. Use bootstrap uncertainty as a first-class inferential layer.
5. Assess prediction, not only in-sample explanation.
6. Treat multigroup, nonlinear, ordinal, multilevel, and Bayesian workflows as explicit extensions with capability checks.
7. Keep a complete audit trail of engine selection and analytical decisions.
8. Prefer agronomic teaching examples, with frozen simulated datasets clearly labeled as teaching data.

## Installation

### Quick install (without vignettes)

```r
pak::pak("wep69/plsSEMflow")
```

### Full install (with vignettes)

```r
remotes::install_github("wep69/plsSEMflow", build_vignettes = TRUE)
```

## Minimal example

```r
library(plsSEMflow)

d <- pls_data("soil_crop")

m <- pls_model(
  measurement = pls_measurement(
    pls_reflective("SOIL", c("soil_om", "soil_cec", "soil_whc")),
    pls_reflective("NUTR", c("leaf_n", "leaf_p", "leaf_k")),
    pls_reflective("VIGOR", c("ndvi", "lai", "chlorophyll")),
    pls_reflective("YIELD", c("grain_yield", "biomass", "harvest_index"))
  ),
  structural = pls_structural(
    pls_path("SOIL", "NUTR"),
    pls_path("SOIL", "VIGOR"),
    pls_path("NUTR", "VIGOR"),
    pls_path("VIGOR", "YIELD"),
    pls_path("NUTR", "YIELD")
  )
)

fit <- pls_fit(m, d, engine = "native")
pls_assess(fit)

boot <- pls_bootstrap(fit, R = 1999, seed = 260819)
pls_table(fit, component = "paths", boot = boot)
pls_plot(fit, type = "paths", boot = boot)
```

For a final manuscript, increase the number of bootstrap replications to a value justified for the study. The package never hard-codes a mandatory final value.
