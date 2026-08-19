# Agronomic teaching datasets

All datasets bundled with `plsSEMflow` are **simulated teaching datasets**. They are frozen examples for documentation, testing, and reproducible instruction. They are not field evidence and must not be presented as empirical agronomic results.

## `soil_crop`

Continuous field-scale example linking soil condition, crop nutrition, canopy vigor, and crop production.

- Context variables: `system` (rainfed/irrigated).
- Soil construct candidates: `soil_om`, `cec`, `whc`.
- Nutritional construct candidates: `leaf_n`, `leaf_p`, `leaf_k`.
- Crop-vigor construct candidates: `ndvi`, `lai`, `chlorophyll`.
- Production construct candidates: `grain_yield`, `biomass`, `harvest_index`.
- Main teaching targets: reflective measurement, mediation, structural assessment, bootstrap, prediction, multigroup analysis.

## `precision_adoption`

Farm-level technology adoption example with ordinal 1--7 survey indicators.

- Context: `region`, `farm_size_ha`, `irrigation`.
- Digital readiness: `dr1`--`dr3`.
- Perceived usefulness: `pu1`--`pu3`.
- Ease of use: `ease1`--`ease3`.
- Adoption intention: `intent1`--`intent3`.
- Actual use: `use1`--`use3`.
- Main teaching targets: ordinal PLSc, mediation, regional comparison, prediction.

## `irrigation_stress`

Irrigation-management example combining management quality, water-use efficiency, drought stress, and yield stability.

- Environmental context: `drought_index`.
- Main teaching targets: moderation, nonlinear response, stress-conditioned effects.

## `bioinput_adoption`

Bioinput-management example with a formative management-capacity block and ordinal reflective outcomes.

- Formative candidates: `training_hours`, `technical_assistance_visits`, `credit_access`, `recordkeeping_score`.
- Main teaching targets: composite/formative specification, outer-weight inference, VIF, adoption pathways.

## `multilevel_farms`

Hierarchical sample of farms nested within municipalities.

- Clustering: `municipality`.
- Context: rainfall zone and farm-level indicators.
- Main teaching targets: multilevel PLS/PLSc, cluster-aware bootstrap, between- and within-context interpretation.

## `ordinal_soil_health`

Ordinal 1--5 assessment of soil health, rooting condition, resilience, and yield stability.

- Main teaching targets: ordinal indicators, measurement assessment, sensitivity to treating ordinal scales as continuous.

## Teaching principle

The examples are deliberately agronomic rather than generic marketing examples. The model should still be justified from agronomic theory and the data-generating process; variable names alone never validate a causal interpretation.
