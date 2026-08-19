# PLS-SEM ecosystem snapshot used for `plsSEMflow`

**Verification date:** 2026-08-19

This file records the software landscape used to design the capability registry and validation tiers. It is documentation, not a guarantee that every external package or commercial product will retain the same interface in future releases.

## R

### seminr

- CRAN version checked: **2.4.2**, published 2026-02-18.
- Scope: model specification, PLS-SEM/PLSc, CB-SEM/CFA, interactions, bootstrap, prediction and multigroup workflows.
- Source checked: https://CRAN.R-project.org/package=seminr

### cSEM

- CRAN version checked: **0.6.1**.
- Scope: PLS-PM, PLSc, ordPLSc, robustPLSc, GSCA/GSCAm/GCCA and post-estimation including assessment, inference, prediction, CVPAT, MICOM, MGD, Hausman endogeneity testing, IPMA and nonlinear-effects analysis.
- Sources checked: https://CRAN.R-project.org/package=cSEM and https://floschuberth.github.io/cSEM/

### plssem

- CRAN version checked: **0.1.3**, published 2026-07-03.
- Development pkgdown documentation observed as **0.1.4** during the verification date.
- Scope: complex PLS/PLSc with categorical/ordinal data, nonlinear relations, higher-order models, parallel bootstrap and multilevel random-intercept/random-slope structures.
- Sources checked: https://CRAN.R-project.org/package=plssem and https://kss2k.github.io/plssem/

### plspm

- CRAN version checked: **0.6.0**, published 2025-09-26.
- Scope: classical PLS-PM, metric/nonmetric data and REBUS analysis.
- Source checked: https://CRAN.R-project.org/package=plspm

## Python

### seminr-py

- Repository: https://github.com/sem-in-r/seminr-py
- The project describes itself as a Python port of `seminr`, with PLS-SEM/PLSc, bootstrap, assessment, PLSpredict, MGA and CB-SEM/CFA functionality.
- Role in `plsSEMflow`: optional parity/benchmark bridge through `reticulate`; never a mandatory dependency.

### plspm-python

- Repository: https://github.com/GoogleCloudPlatform/plspm-python
- Scope documented by the project: Mode A/Mode B PLS-PM, higher-order constructs, metric/nonmetric data, missing data and multicore bootstrap.
- Role in `plsSEMflow`: optional external benchmark. The project itself states that it is not an officially supported Google product.

## Julia

### StructuralEquationModels.jl

- Documentation: https://structuralequationmodels.github.io/StructuralEquationModels.jl/stable/
- Scope: extensible linear SEM, ML, GLS, FIML, regularization, multigroup SEM and custom loss functions.
- Role in `plsSEMflow`: optional SEM research and benchmarking bridge. It is **not** treated as a native PLS-SEM backend.

## Specialized/commercial software

### SmartPLS 4

- Documentation: https://www.smartpls.com/documentation/
- Relevant documented procedures include PLS-SEM, PLSc, bootstrapping, PLSpredict, CVPAT, mediation, moderation, higher-order models, IPMA, NCA, endogeneity/Gaussian copula approaches, FIMIX-PLS, PLS-POS, MGA and MICOM.
- Current bootstrap documentation describes user-selectable resamples and approximately 10,000 subsamples for final results, with percentile, studentized and BCa interval options.
- Role: external benchmark and workflow reference, not a runtime dependency.

### WarpPLS

- Official site: https://warppls.com/
- Stable version reported by the official site on the verification date: **8.0**.
- Role: external nonlinear PLS-based SEM benchmark.

### XLSTAT

- PLS-PM documentation: https://www.xlstat.com/solutions/features/pls-path-modelling
- Role: spreadsheet-based external benchmark where appropriate.

### ADANCO

- Role: optional external commercial validation when a licensed local installation and an export format are available.
- No version is hard-coded in `plsSEMflow`; users should record the version used in each validation study.

## Design consequence

The package does not force equivalence among ecosystems. A capability is routed only to an engine that actually supports the intended estimator. Python and Julia remain optional. Commercial software is never required to install or run the R core.
