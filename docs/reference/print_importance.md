# Prints variable importance

Prints variable importance scores from
[rf](https://blasbenito.github.io/spatialRF/reference/rf.md),
[rf_repeat](https://blasbenito.github.io/spatialRF/reference/rf_repeat.md),
and
[rf_spatial](https://blasbenito.github.io/spatialRF/reference/rf_spatial.md)
models.

## Usage

``` r
print_importance(
  model,
  verbose = TRUE
)
```

## Arguments

- model:

  A model fitted with
  [rf](https://blasbenito.github.io/spatialRF/reference/rf.md),
  [rf_repeat](https://blasbenito.github.io/spatialRF/reference/rf_repeat.md),
  or
  [rf_spatial](https://blasbenito.github.io/spatialRF/reference/rf_spatial.md).

- verbose:

  Logical, if `TRUE`, variable importance is returned. Default: `TRUE`

## Value

A table printed to the standard output.

## See also

[`plot_importance()`](https://blasbenito.github.io/spatialRF/reference/plot_importance.md),
[`get_importance()`](https://blasbenito.github.io/spatialRF/reference/get_importance.md)

Other model_info:
[`get_evaluation()`](https://blasbenito.github.io/spatialRF/reference/get_evaluation.md),
[`get_importance()`](https://blasbenito.github.io/spatialRF/reference/get_importance.md),
[`get_importance_local()`](https://blasbenito.github.io/spatialRF/reference/get_importance_local.md),
[`get_moran()`](https://blasbenito.github.io/spatialRF/reference/get_moran.md),
[`get_performance()`](https://blasbenito.github.io/spatialRF/reference/get_performance.md),
[`get_predictions()`](https://blasbenito.github.io/spatialRF/reference/get_predictions.md),
[`get_residuals()`](https://blasbenito.github.io/spatialRF/reference/get_residuals.md),
[`get_response_curves()`](https://blasbenito.github.io/spatialRF/reference/get_response_curves.md),
[`get_spatial_predictors()`](https://blasbenito.github.io/spatialRF/reference/get_spatial_predictors.md),
[`print.rf()`](https://blasbenito.github.io/spatialRF/reference/print.md),
[`print_evaluation()`](https://blasbenito.github.io/spatialRF/reference/print_evaluation.md),
[`print_moran()`](https://blasbenito.github.io/spatialRF/reference/print_moran.md),
[`print_performance()`](https://blasbenito.github.io/spatialRF/reference/print_performance.md)

## Examples

``` r
data(plants_rf)

print_importance(plants_rf)
#>                 ┌─────────────────────────────────┬────────────┐
#>                 │ Variable                        │ Importance │
#>                 ├─────────────────────────────────┼────────────┤
#>                 │ climate_bio1_average            │   1949.457 │
#>                 ├─────────────────────────────────┼────────────┤
#>                 │ human_population                │   1595.806 │
#>                 ├─────────────────────────────────┼────────────┤
#>                 │ climate_hypervolume             │   1188.702 │
#>                 ├─────────────────────────────────┼────────────┤
#>                 │ bias_area_km2                   │   1177.989 │
#>                 ├─────────────────────────────────┼────────────┤
#>                 │ human_population_density        │   1026.905 │
#>                 ├─────────────────────────────────┼────────────┤
#>                 │ human_footprint_average         │   1021.417 │
#>                 ├─────────────────────────────────┼────────────┤
#>                 │ climate_aridity_index_average   │    844.765 │
#>                 ├─────────────────────────────────┼────────────┤
#>                 │ neighbors_count                 │    707.572 │
#>                 ├─────────────────────────────────┼────────────┤
#>                 │ neighbors_area                  │    649.625 │
#>                 ├─────────────────────────────────┼────────────┤
#>                 │ bias_species_per_record         │    607.005 │
#>                 ├─────────────────────────────────┼────────────┤
#>                 │ topography_elevation_average    │    523.113 │
#>                 ├─────────────────────────────────┼────────────┤
#>                 │ fragmentation_cohesion          │    484.973 │
#>                 ├─────────────────────────────────┼────────────┤
#>                 │ climate_velocity_lgm_average    │    218.293 │
#>                 ├─────────────────────────────────┼────────────┤
#>                 │ climate_bio15_minimum           │    215.974 │
#>                 ├─────────────────────────────────┼────────────┤
#>                 │ fragmentation_division          │    197.148 │
#>                 ├─────────────────────────────────┼────────────┤
#>                 │ neighbors_percent_shared_edge   │     92.874 │
#>                 ├─────────────────────────────────┼────────────┤
#>                 │ landcover_herbs_percent_average │    -81.849 │
#>                 └─────────────────────────────────┴────────────┘
```
