# Convert VIF values to data frame

Computes variance inflation factors for all variables in a data frame
and returns them in a tidy format, sorted by VIF in descending order.

## Usage

``` r
.vif_to_df(x)
```

## Arguments

- x:

  Data frame with numeric predictors for which to compute VIF values.

## Value

Data frame with two columns: `variable` (character, variable names) and
`vif` (numeric, VIF scores), sorted by VIF in descending order.

## See also

Other utilities:
[`auc()`](https://blasbenito.github.io/spatialRF/reference/auc.md),
[`beowulf_cluster()`](https://blasbenito.github.io/spatialRF/reference/beowulf_cluster.md),
[`objects_size()`](https://blasbenito.github.io/spatialRF/reference/objects_size.md),
[`optimization_function()`](https://blasbenito.github.io/spatialRF/reference/optimization_function.md),
[`prepare_importance_spatial()`](https://blasbenito.github.io/spatialRF/reference/prepare_importance_spatial.md),
[`rescale_vector()`](https://blasbenito.github.io/spatialRF/reference/rescale_vector.md),
[`root_mean_squared_error()`](https://blasbenito.github.io/spatialRF/reference/root_mean_squared_error.md),
[`setup_parallel_execution()`](https://blasbenito.github.io/spatialRF/reference/setup_parallel_execution.md),
[`standard_error()`](https://blasbenito.github.io/spatialRF/reference/standard_error.md),
[`statistical_mode()`](https://blasbenito.github.io/spatialRF/reference/statistical_mode.md),
[`thinning()`](https://blasbenito.github.io/spatialRF/reference/thinning.md),
[`thinning_til_n()`](https://blasbenito.github.io/spatialRF/reference/thinning_til_n.md)
