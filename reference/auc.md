# Area under the ROC curve

Computes the area under the ROC curve (AUC) for binary classification.

## Usage

``` r
auc(o, p)
```

## Arguments

- o:

  Numeric vector of actual binary labels (0 or 1). Must have the same
  length as `p`.

- p:

  Numeric vector of predicted probabilities (typically 0 to 1). Must
  have the same length as `o`.

## Value

Numeric value between 0 and 1 representing the AUC. Higher values
indicate better classification performance, with 0.5 indicating random
performance and 1.0 indicating perfect classification.

## See also

Other utilities:
[`.vif_to_df()`](https://blasbenito.github.io/spatialRF/reference/dot-vif_to_df.md),
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

## Examples

``` r
auc(
  o = c(0, 0, 1, 1),
  p = c(0.1, 0.6, 0.4, 0.8)
  )
#> [1] 0.75
```
