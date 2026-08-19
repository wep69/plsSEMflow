# plsSEMflow architecture

```text
Scientific question / agronomic mechanism
             |
             v
Construct conceptualization
(reflective | composite | formative | higher-order)
             |
             v
Structural relation grammar
(path | mediation | moderation | nonlinear | cluster)
             |
             v
Pre-fit audit + capability registry
             |
             v
Engine router
  | native R PLS-PM
  | cSEM
  | seminr
  | plssem
  | plspm
  | Bayesian CB-SEM companion
  | optional Python / Julia / external benchmark bridges
             |
             v
Unified result/audit layer
             |
             +--> measurement assessment
             +--> structural assessment
             +--> user-controlled bootstrap and CIs
             +--> effects / groups / heterogeneity
             +--> prediction / CVPAT
             +--> advanced specialist workflows
             |
             v
Publication tables + figures + report
             |
             v
Simulation / frozen validation / cross-engine checks
```

## Core design rule

Backends are capabilities, not brands. `engine="auto"` must not choose a method merely because a package is installed. The requested estimator, data type, construct specification, nonlinearity, grouping, and multilevel requirements determine admissible engines; the decision is stored in the fit audit.

## Dependency rule

R is sufficient for the native core. Python, Julia, Bayesian engines, and commercial software are optional. A missing optional dependency should produce an informative error only when that capability is requested.

## Validation tiers

- **Tier 1:** core/established engine eligible for routine routing after local golden validation.
- **Tier 2:** supported specialist backend requiring explicit capability checks and local validation for the advertised module.
- **Tier 3:** experimental/external bridge; never silently selected for a primary analysis.
