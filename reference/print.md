# Custom print method for random forest models

Custom print method for models fitted with
[`rf()`](https://blasbenito.github.io/spatialRF/reference/rf.md),
[`rf_repeat()`](https://blasbenito.github.io/spatialRF/reference/rf_repeat.md),
and
[`rf_spatial()`](https://blasbenito.github.io/spatialRF/reference/rf_spatial.md).

## Usage

``` r
# S3 method for class 'rf'
print(x, ...)
```

## Arguments

- x:

  A model fitted with
  [`rf()`](https://blasbenito.github.io/spatialRF/reference/rf.md),
  [`rf_repeat()`](https://blasbenito.github.io/spatialRF/reference/rf_repeat.md),
  or
  [`rf_spatial()`](https://blasbenito.github.io/spatialRF/reference/rf_spatial.md).

- ...:

  Additional arguments for print methods.

## Value

Prints model details to the console.

## See also

[`print_evaluation()`](https://blasbenito.github.io/spatialRF/reference/print_evaluation.md),
[`print_importance()`](https://blasbenito.github.io/spatialRF/reference/print_importance.md),
[`print_moran()`](https://blasbenito.github.io/spatialRF/reference/print_moran.md),
[`print_performance()`](https://blasbenito.github.io/spatialRF/reference/print_performance.md)

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
[`print_evaluation()`](https://blasbenito.github.io/spatialRF/reference/print_evaluation.md),
[`print_importance()`](https://blasbenito.github.io/spatialRF/reference/print_importance.md),
[`print_moran()`](https://blasbenito.github.io/spatialRF/reference/print_moran.md),
[`print_performance()`](https://blasbenito.github.io/spatialRF/reference/print_performance.md)

## Examples

``` r
data(plants_rf)

print(plants_rf)
#> Model type
#>   - Fitted with:                     ranger()
#>   - Response variable:               richness_species_vascular
#> 
#> Random forest parameters
#>   - Type:                            Regression
#>   - Number of trees:                 50
#>   - Sample size:                     227
#>   - Number of predictors:            17
#>   - Mtry:                            4
#>   - Minimum node size:               30
#> 
#> 
#> Model performance 
#>   - R squared (oob):                  0.5015626
#>   - R squared (cor(obs, pred)^2):     0.8378728
#>   - Pseudo R squared (cor(obs, pred)):0.9153539
#>   - RMSE (oob):                       2379.254
#>   - RMSE:                             1609.614
#>   - Normalized RMSE:                  0.4646691
#> 
#> Model residuals 
#>   - Stats: 
#>          ┌──────────┬─────────┬─────────┬────────┬────────┬──────────┐
#>          │ Min.     │ 1st Q.  │ Median  │ Mean   │ 3rd Q. │ Max.     │
#>          ├──────────┼─────────┼─────────┼────────┼────────┼──────────┤
#>          │ -3408.93 │ -847.75 │ -302.35 │ -61.71 │ 325.94 │ 11040.86 │
#>          └──────────┴─────────┴─────────┴────────┴────────┴──────────┘
#>   - Normality: 
#>       - Shapiro-Wilks W: 0.798 
#>       - p-value        : 0 
#>       - Interpretation : Residuals are not normal 
#> 
#>   - Spatial autocorrelation: 
#>              ┌──────────┬───────────┬─────────┬──────────────────┐
#>              │ Distance │ Moran's I │ P value │ Interpretation   │
#>              ├──────────┼───────────┼─────────┼──────────────────┤
#>              │    100.0 │     0.164 │   0.000 │ Positive spatial │
#>              │          │           │         │ correlation      │
#>              ├──────────┼───────────┼─────────┼──────────────────┤
#>              │   1000.0 │     0.073 │   0.000 │ Positive spatial │
#>              │          │           │         │ correlation      │
#>              ├──────────┼───────────┼─────────┼──────────────────┤
#>              │   2000.0 │     0.026 │   0.002 │ Positive spatial │
#>              │          │           │         │ correlation      │
#>              ├──────────┼───────────┼─────────┼──────────────────┤
#>              │   4000.0 │     0.008 │   0.032 │ Positive spatial │
#>              │          │           │         │ correlation      │
#>              └──────────┴───────────┴─────────┴──────────────────┘
#> 
#> Variable importance: 
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

#or
plants_rf
#> Model type
#>   - Fitted with:                     ranger()
#>   - Response variable:               richness_species_vascular
#> 
#> Random forest parameters
#>   - Type:                            Regression
#>   - Number of trees:                 50
#>   - Sample size:                     227
#>   - Number of predictors:            17
#>   - Mtry:                            4
#>   - Minimum node size:               30
#> 
#> 
#> Model performance 
#>   - R squared (oob):                  0.5015626
#>   - R squared (cor(obs, pred)^2):     0.8378728
#>   - Pseudo R squared (cor(obs, pred)):0.9153539
#>   - RMSE (oob):                       2379.254
#>   - RMSE:                             1609.614
#>   - Normalized RMSE:                  0.4646691
#> 
#> Model residuals 
#>   - Stats: 
#>          ┌──────────┬─────────┬─────────┬────────┬────────┬──────────┐
#>          │ Min.     │ 1st Q.  │ Median  │ Mean   │ 3rd Q. │ Max.     │
#>          ├──────────┼─────────┼─────────┼────────┼────────┼──────────┤
#>          │ -3408.93 │ -847.75 │ -302.35 │ -61.71 │ 325.94 │ 11040.86 │
#>          └──────────┴─────────┴─────────┴────────┴────────┴──────────┘
#>   - Normality: 
#>       - Shapiro-Wilks W: 0.798 
#>       - p-value        : 0 
#>       - Interpretation : Residuals are not normal 
#> 
#>   - Spatial autocorrelation: 
#>              ┌──────────┬───────────┬─────────┬──────────────────┐
#>              │ Distance │ Moran's I │ P value │ Interpretation   │
#>              ├──────────┼───────────┼─────────┼──────────────────┤
#>              │    100.0 │     0.164 │   0.000 │ Positive spatial │
#>              │          │           │         │ correlation      │
#>              ├──────────┼───────────┼─────────┼──────────────────┤
#>              │   1000.0 │     0.073 │   0.000 │ Positive spatial │
#>              │          │           │         │ correlation      │
#>              ├──────────┼───────────┼─────────┼──────────────────┤
#>              │   2000.0 │     0.026 │   0.002 │ Positive spatial │
#>              │          │           │         │ correlation      │
#>              ├──────────┼───────────┼─────────┼──────────────────┤
#>              │   4000.0 │     0.008 │   0.032 │ Positive spatial │
#>              │          │           │         │ correlation      │
#>              └──────────┴───────────┴─────────┴──────────────────┘
#> 
#> Variable importance: 
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
