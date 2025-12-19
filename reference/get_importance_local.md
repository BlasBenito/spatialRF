# Extract local variable importance from model

Extracts local (case-specific) variable importance scores from models
fitted with
[`rf()`](https://blasbenito.github.io/spatialRF/reference/rf.md),
[`rf_repeat()`](https://blasbenito.github.io/spatialRF/reference/rf_repeat.md),
or
[`rf_spatial()`](https://blasbenito.github.io/spatialRF/reference/rf_spatial.md).

## Usage

``` r
get_importance_local(model)
```

## Arguments

- model:

  Model object from
  [`rf()`](https://blasbenito.github.io/spatialRF/reference/rf.md),
  [`rf_repeat()`](https://blasbenito.github.io/spatialRF/reference/rf_repeat.md),
  or
  [`rf_spatial()`](https://blasbenito.github.io/spatialRF/reference/rf_spatial.md).

## Value

Data frame with one row per observation and one column per predictor
variable. Each cell contains the local importance score for that
variable at that observation.

## Details

Local importance measures how much each predictor contributes to
predictions for individual observations, unlike global importance which
summarizes contributions across all observations. This can reveal
spatial or contextual patterns in variable influence.

## See also

[`rf()`](https://blasbenito.github.io/spatialRF/reference/rf.md),
[`rf_repeat()`](https://blasbenito.github.io/spatialRF/reference/rf_repeat.md),
[`rf_spatial()`](https://blasbenito.github.io/spatialRF/reference/rf_spatial.md),
[`get_importance()`](https://blasbenito.github.io/spatialRF/reference/get_importance.md),
[`plot_importance()`](https://blasbenito.github.io/spatialRF/reference/plot_importance.md),
[`print_importance()`](https://blasbenito.github.io/spatialRF/reference/print_importance.md)

Other model_info:
[`get_evaluation()`](https://blasbenito.github.io/spatialRF/reference/get_evaluation.md),
[`get_importance()`](https://blasbenito.github.io/spatialRF/reference/get_importance.md),
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

# Extract local importance scores
local_imp <- get_importance_local(plants_rf)

# View structure: rows = observations, columns = variables
dim(local_imp)
#> [1] 227  17
head(local_imp)
#>   bias_area_km2 bias_species_per_record climate_aridity_index_average
#> 1      828.0285               -395.2323                     499.11729
#> 2     -467.9925              -1007.3478                    -167.80674
#> 3        0.0000                  0.0000                      26.38278
#> 4     -212.0167                721.2856                     467.19906
#> 5     1183.8162                950.5329                     880.59377
#> 6     -205.3745                248.4184                       0.00000
#>   climate_hypervolume climate_velocity_lgm_average neighbors_count
#> 1            885.6491                       0.0000          0.0000
#> 2           -574.3834                    -862.5579      -1154.3253
#> 3              0.0000                       0.0000       -165.7917
#> 4           1020.1034                     877.2258      -1349.4845
#> 5           1269.9320                    -760.4056          0.0000
#> 6            380.8480                     105.5448       -397.5548
#>   neighbors_percent_shared_edge human_population_density
#> 1                     -257.9104                  0.00000
#> 2                     1128.5381                 44.69683
#> 3                     -313.4053                337.68437
#> 4                        0.0000                688.07958
#> 5                    -1462.2742               1404.97754
#> 6                        0.0000                340.49454
#>   topography_elevation_average landcover_herbs_percent_average
#> 1                       0.0000                        254.0174
#> 2                     664.0936                       -323.0326
#> 3                      89.4040                          0.0000
#> 4                       0.0000                          0.0000
#> 5                   -1117.9085                        751.4930
#> 6                     183.2137                        297.9530
#>   fragmentation_cohesion fragmentation_division neighbors_area human_population
#> 1                 0.0000                 0.0000         0.0000            0.000
#> 2               684.4019                 0.0000      -271.0058         -821.800
#> 3                 0.0000                 0.0000         0.0000         1818.641
#> 4              -345.8733               416.6738       676.9000         1478.199
#> 5               512.6154                 0.0000      1400.9972        -1419.067
#> 6                 0.0000                 0.0000         0.0000            0.000
#>   human_footprint_average climate_bio1_average climate_bio15_minimum
#> 1                940.8037             149.0290                0.0000
#> 2                266.3238            -872.0896                0.0000
#> 3                315.7241            2495.2004                0.0000
#> 4                501.3918             917.7755                0.0000
#> 5               -527.6551             493.6461                0.0000
#> 6                824.8517            -269.4552              773.9732

# Find which variable is most important for first observation
colnames(local_imp)[which.max(local_imp[1, ])]
#> [1] "human_footprint_average"
```
