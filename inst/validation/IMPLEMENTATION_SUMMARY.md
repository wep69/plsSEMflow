# plsSEMflow 0.1.0.9000 implementation summary

## Scope

`plsSEMflow` is an R-first integrator for PLS-SEM workflows. The native core implements continuous linear PLS-PM and user-controlled bootstrap inference. Optional backends are routed only when they provide a scientifically relevant capability not claimed by the native engine.

## Implemented layers

### Scientific model grammar

- reflective/common-factor specifications;
- composites and formative blocks;
- direct paths;
- mediation;
- moderation metadata;
- quadratic and polynomial nonlinear metadata;
- higher-order constructs;
- clustering/multilevel metadata;
- syntax conversion for cSEM/plssem/lavaan-compatible workflows.

### Native R estimation and assessment

- iterative PLS-PM Mode A/Mode B weighting;
- path/centroid inner weighting schemes;
- construct scores;
- path coefficients and R2;
- loadings and weights;
- alpha, composite reliability, AVE, HTMT;
- formative VIF;
- structural VIF and f2;
- audit trail and explicit capability routing.

### Bootstrap and uncertainty

- case bootstrap;
- cluster bootstrap;
- percentile, basic and BCa intervals for native path/loading/weight coefficients;
- user-selected `R`, confidence level, seed and interval method;
- direct use of bootstrap CIs in publication path/loading/weight tables and plots;
- derived percentile intervals for native indirect and total effects;
- native two-group path-difference bootstrap;
- cSEM resampling adapter.

### Advanced workflows

- cSEM: PLSc/composite workflows, assessment, CVPAT, MICOM, MGD, Hausman endogeneity, IPMA and nonlinear-effects analysis;
- seminr adapter;
- plssem ordinal and multilevel wrappers;
- plspm classical PLS-PM and REBUS workflow;
- score-based moderation and polynomial nonlinear sensitivity analyses;
- Bayesian CB-SEM companion through blavaan with explicit non-equivalence labeling;
- optional Python SEMinR interoperability;
- Julia SEM comparison bundle without claiming Julia-native PLS-SEM;
- external result exchange for SmartPLS/WarpPLS/other specialist software.

### Prediction, heterogeneity and validation

- repeated k-fold construct-score prediction with a linear benchmark in the native workflow;
- cSEM CVPAT adapter;
- native MGA and cSEM MICOM/MGD;
- REBUS-PLS routing;
- external workflow descriptors for FIMIX-PLS and PLS-POS;
- recursive PLS-SEM simulation helper;
- parameter-recovery/coverage validation cases;
- cross-engine numerical comparison scaffold.

### Agronomic teaching data

Six frozen simulated datasets are bundled:

1. soil quality -> nutrition -> crop vigor -> production;
2. precision-agriculture adoption;
3. irrigation management and drought stress;
4. bioinput management/adoption;
5. farms nested in municipalities;
6. ordinal soil-health/resilience assessment.

All are explicitly teaching data and are not field evidence.

### Documentation and publication

- 21 English R Markdown vignettes;
- extensive foundations-to-advanced integrated tutorial modeled on the supplied pedagogical reference;
- API example catalog with at least three vignette calls for every exported function;
- publication path, loading, weight, bootstrap, HTMT, prediction and R2 plots;
- advanced coefficient and MGA plots;
- data.frame, Markdown, gt, flextable and XLSX table workflows;
- PDF/SVG vector and 600-dpi TIFF/PNG export through ggplot2;
- Markdown report scaffold;
- pkgdown and GitHub Actions scaffolding;
- roxygen2-authoritative documentation source and NAMESPACE regeneration script.

## Source quality gates in this build

- 62 exported functions;
- 10 registered S3 print methods;
- 21 vignettes;
- 6 agronomic teaching datasets;
- 72 conservative Rd source snapshots;
- 37/37 source-only static checks passed;
- every exported function appears at least three times across the vignette collection.

## Runtime validation status

**NOT RUN in the construction environment.** No R/Rscript executable is available here. The Scientific Package Builder rule is therefore followed: static validation is reported as static validation only. Official roxygen2 regeneration, R parsing/execution, unit tests, vignette builds, optional-backend golden tests, `R CMD build`, and `R CMD check --as-cran` remain explicit local release gates.

Use `tools/VALIDATION_LOCAL.md` for the complete sequence.
