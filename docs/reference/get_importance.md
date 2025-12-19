# Extract variable importance from model

Extracts variable importance scores from models fitted with
[`rf()`](https://blasbenito.github.io/spatialRF/reference/rf.md),
[`rf_repeat()`](https://blasbenito.github.io/spatialRF/reference/rf_repeat.md),
or
[`rf_spatial()`](https://blasbenito.github.io/spatialRF/reference/rf_spatial.md).

## Usage

``` r
get_importance(model)
```

## Arguments

- model:

  Model object from
  [`rf()`](https://blasbenito.github.io/spatialRF/reference/rf.md),
  [`rf_repeat()`](https://blasbenito.github.io/spatialRF/reference/rf_repeat.md),
  or
  [`rf_spatial()`](https://blasbenito.github.io/spatialRF/reference/rf_spatial.md).

## Value

Data frame with columns `variable` (character) and `importance`
(numeric), sorted by decreasing importance.

## Details

For spatial models
([`rf_spatial()`](https://blasbenito.github.io/spatialRF/reference/rf_spatial.md))
with many spatial predictors, this function returns aggregated
importance statistics for spatial predictors to improve readability.
Non-spatial models return per-variable importance scores directly.

## See also

[`rf()`](https://blasbenito.github.io/spatialRF/reference/rf.md),
[`rf_repeat()`](https://blasbenito.github.io/spatialRF/reference/rf_repeat.md),
[`rf_spatial()`](https://blasbenito.github.io/spatialRF/reference/rf_spatial.md),
[`plot_importance()`](https://blasbenito.github.io/spatialRF/reference/plot_importance.md),
[`print_importance()`](https://blasbenito.github.io/spatialRF/reference/print_importance.md)

Other model_info:
[`get_evaluation()`](https://blasbenito.github.io/spatialRF/reference/get_evaluation.md),
[`get_importance_local()`](https://blasbenito.github.io/spatialRF/reference/get_importance_local.md),
[`get_moran()`](https://blasbenito.github.io/spatialRF/reference/get_moran.md),
[`get_performance()`](https://blasbenito.github.io/spatialRF/reference/get_performance.md),
[`get_predictions()`](https://blasbenito.github.io/spatialRF/reference/get_predictions.md),
[`get_residuals()`](https://blasbenito.github.io/spatialRF/reference/get_residuals.md),
[`get_response_curves()`](https://blasbenito.github.io/spatialRF/reference/get_response_curves.md),
[`get_spatial_predictors()`](https://blasbenito.github.io/spatialRF/reference/get_spatial_predictors.md),
[`print.rf()`](https://blasbenito.github.io/spatialRF/reference/print.md),
[`print_evaluation()`](https://blasbenito.github.io/spatialRF/reference/print_evaluation.md),
[`print_importance()`](https://blasbenito.github.io/spatialRF/reference/print_importance.md),
[`print_moran()`](https://blasbenito.github.io/spatialRF/reference/print_moran.md),
[`print_performance()`](https://blasbenito.github.io/spatialRF/reference/print_performance.md)

## Examples

``` r
data(plants_rf)

# Extract variable importance
importance <- get_importance(plants_rf)
head(importance)
#>                   variable importance
#> 1     climate_bio1_average   1949.457
#> 2         human_population   1595.806
#> 3      climate_hypervolume   1188.702
#> 4            bias_area_km2   1177.989
#> 5 human_population_density   1026.905
#> 6  human_footprint_average   1021.417

# View top 5 most important variables
importance[1:5, ]
#>                   variable importance
#> 1     climate_bio1_average   1949.457
#> 2         human_population   1595.806
#> 3      climate_hypervolume   1188.702
#> 4            bias_area_km2   1177.989
#> 5 human_population_density   1026.905
```
