# Variance Inflation Factor of a data frame

Computes the variance inflation factor (VIF) of the colums in a data
frame. **Warning**: variables in `preference.order` not in
`colnames(x)`, and non-numeric columns are removed silently from `x` and
`preference.order`. The same happens with rows having NA values
([`na.omit()`](https://rdrr.io/r/stats/na.fail.html) is applied). The
function issues a warning if zero-variance columns are found.

## Usage

``` r
vif(x)
```

## Arguments

- x:

  Data frame with numeric columns, typically containing a set of model
  predictors.

## Value

A data frame with two columns having the name of the variables in 'x'
and their respective VIF values.

## See also

[`auto_vif()`](https://blasbenito.github.io/spatialRF/reference/auto_vif.md),
[`auto_cor()`](https://blasbenito.github.io/spatialRF/reference/auto_cor.md)

Other utilities:
[`auc`](https://blasbenito.github.io/spatialRF/reference/auc.md)`()`,
[`beowulf_cluster`](https://blasbenito.github.io/spatialRF/reference/beowulf_cluster.md)`()`,
[`objects_size`](https://blasbenito.github.io/spatialRF/reference/objects_size.md)`()`,
[`optimization_function`](https://blasbenito.github.io/spatialRF/reference/optimization_function.md)`()`,
[`prepare_importance_spatial`](https://blasbenito.github.io/spatialRF/reference/prepare_importance_spatial.md)`()`,
[`rescale_vector`](https://blasbenito.github.io/spatialRF/reference/rescale_vector.md)`()`,
[`root_mean_squared_error`](https://blasbenito.github.io/spatialRF/reference/root_mean_squared_error.md)`()`,
[`setup_parallel_execution`](https://blasbenito.github.io/spatialRF/reference/setup_parallel_execution.md)`()`,
[`standard_error`](https://blasbenito.github.io/spatialRF/reference/standard_error.md)`()`,
[`statistical_mode`](https://blasbenito.github.io/spatialRF/reference/statistical_mode.md)`()`,
[`thinning`](https://blasbenito.github.io/spatialRF/reference/thinning.md)`()`,
[`thinning_til_n`](https://blasbenito.github.io/spatialRF/reference/thinning_til_n.md)`()`

## Examples

``` r
 data(plants_df)

 vif(plants_df[, 5:21])
#>                           variable   vif
#> 1          human_footprint_average 4.701
#> 2             climate_bio1_average 2.997
#> 3         human_population_density 2.879
#> 4                 human_population 2.709
#> 5     topography_elevation_average 1.975
#> 6    climate_aridity_index_average 1.938
#> 7                    bias_area_km2 1.847
#> 8  landcover_herbs_percent_average 1.742
#> 9     climate_velocity_lgm_average 1.742
#> 10   neighbors_percent_shared_edge 1.680
#> 11          fragmentation_cohesion 1.538
#> 12         bias_species_per_record 1.508
#> 13           climate_bio15_minimum 1.483
#> 14                 neighbors_count 1.409
#> 15                  neighbors_area 1.392
#> 16             climate_hypervolume 1.390
#> 17          fragmentation_division 1.334
```
