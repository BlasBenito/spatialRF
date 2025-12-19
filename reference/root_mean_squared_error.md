# RMSE and normalized RMSE

Computes the rmse or normalized rmse (nrmse) between two numeric vectors
of the same length representing observations and model predictions.

## Usage

``` r
root_mean_squared_error(
  o,
  p,
  normalization = c("rmse", "all", "mean", "sd", "maxmin", "iq")
)
```

## Arguments

- o:

  Numeric vector with observations, must have the same length as `p`.

- p:

  Numeric vector with predictions, must have the same length as `o`.

- normalization:

  character, normalization method, Default: "rmse" (see Details).

## Value

Named numeric vector with either one or 5 values, as selected by the
user.

## Details

The normalization methods go as follows:

- `"rmse"`: RMSE with no normalization.

- `"mean"`: RMSE dividied by the mean of the observations
  (rmse/mean(o)).

- `"sd"`: RMSE dividied by the standard deviation of the observations
  (rmse/sd(o)).

- `"maxmin"`: RMSE divided by the range of the observations
  (rmse/(max(o) - min(o))).

- "`iq"`: RMSE divided by the interquartile range of the observations
  (rmse/(quantile(o, 0.75) - quantile(o, 0.25)))

## See also

Other utilities:
[`.vif_to_df()`](https://blasbenito.github.io/spatialRF/reference/dot-vif_to_df.md),
[`auc()`](https://blasbenito.github.io/spatialRF/reference/auc.md),
[`beowulf_cluster()`](https://blasbenito.github.io/spatialRF/reference/beowulf_cluster.md),
[`objects_size()`](https://blasbenito.github.io/spatialRF/reference/objects_size.md),
[`optimization_function()`](https://blasbenito.github.io/spatialRF/reference/optimization_function.md),
[`prepare_importance_spatial()`](https://blasbenito.github.io/spatialRF/reference/prepare_importance_spatial.md),
[`rescale_vector()`](https://blasbenito.github.io/spatialRF/reference/rescale_vector.md),
[`setup_parallel_execution()`](https://blasbenito.github.io/spatialRF/reference/setup_parallel_execution.md),
[`standard_error()`](https://blasbenito.github.io/spatialRF/reference/standard_error.md),
[`statistical_mode()`](https://blasbenito.github.io/spatialRF/reference/statistical_mode.md),
[`thinning()`](https://blasbenito.github.io/spatialRF/reference/thinning.md),
[`thinning_til_n()`](https://blasbenito.github.io/spatialRF/reference/thinning_til_n.md)

## Examples

``` r
root_mean_squared_error(
  o = runif(10),
  p = runif(10)
)
#>   rmse 
#> 0.4481 
```
