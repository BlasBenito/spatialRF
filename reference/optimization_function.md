# Compute optimization scores for spatial predictor selection

Computes optimization scores for candidate spatial predictor sets using
either the "moran.i" or "p.value" method. Higher scores indicate better
trade-offs between spatial autocorrelation reduction, model performance,
and parsimony.

## Usage

``` r
optimization_function(
  x = NULL,
  weight.r.squared = NULL,
  weight.penalization.n.predictors = NULL,
  optimization.method = "moran.i"
)
```

## Arguments

- x:

  Data frame containing optimization metrics for candidate spatial
  predictor sets. Generated internally by
  [`select_spatial_predictors_sequential()`](https://blasbenito.github.io/spatialRF/reference/select_spatial_predictors_sequential.md)
  or
  [`select_spatial_predictors_recursive()`](https://blasbenito.github.io/spatialRF/reference/select_spatial_predictors_recursive.md).
  Must include columns: `moran.i`, `r.squared`,
  `penalization.per.variable`, and `p.value.binary` (for "p.value"
  method).

- weight.r.squared:

  Numeric value between 0 and 1 specifying the weight for R-squared in
  the optimization score. Higher values prioritize model performance.

- weight.penalization.n.predictors:

  Numeric value between 0 and 1 specifying the weight for penalizing the
  number of spatial predictors. Higher values favor more parsimonious
  models.

- optimization.method:

  Character string specifying the optimization method: `"moran.i"`
  (default) or `"p.value"`. Default: `"moran.i"`.

## Value

Numeric vector of optimization scores, one per row in `x`. Higher scores
indicate better solutions. All values are rescaled between 0 and 1 for
comparability.

## Details

This function balances three objectives when selecting spatial
predictors:

1.  **Reduce spatial autocorrelation**: Maximize `1 - Moran's I` to
    minimize residual spatial autocorrelation

2.  **Maintain model performance**: Account for model R-squared

3.  **Favor parsimony**: Penalize models with many spatial predictors

**Optimization methods:**

The **"moran.i"** method computes:

`score = (1 - Moran's I) + w1 × R² - w2 × penalization`

where all components are rescaled to the range 0 to 1, `w1` =
`weight.r.squared`, and `w2` = `weight.penalization.n.predictors`.

The **"p.value"** method computes:

`score = max(1 - Moran's I, binary p-value) + w1 × R² - w2 × penalization`

where the binary p-value is 1 if p equal or lower than 0.05 (no
significant spatial autocorrelation), and 0 otherwise.

**Practical differences:**

- The "moran.i" method uses continuous Moran's I values and typically
  selects more spatial predictors to achieve lower spatial
  autocorrelation

- The "p.value" method uses binary significance thresholds and typically
  selects fewer predictors, stopping once spatial autocorrelation
  becomes non-significant

The optimal model is the one with the highest optimization score.

## See also

[`select_spatial_predictors_recursive()`](https://blasbenito.github.io/spatialRF/reference/select_spatial_predictors_recursive.md),
[`select_spatial_predictors_sequential()`](https://blasbenito.github.io/spatialRF/reference/select_spatial_predictors_sequential.md),
[`moran()`](https://blasbenito.github.io/spatialRF/reference/moran.md)

Other utilities:
[`.vif_to_df()`](https://blasbenito.github.io/spatialRF/reference/dot-vif_to_df.md),
[`auc()`](https://blasbenito.github.io/spatialRF/reference/auc.md),
[`beowulf_cluster()`](https://blasbenito.github.io/spatialRF/reference/beowulf_cluster.md),
[`objects_size()`](https://blasbenito.github.io/spatialRF/reference/objects_size.md),
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
if (FALSE) { # \dontrun{
# This function is typically called internally during spatial predictor selection
# Example showing the structure of input data:

# Simulated optimization data frame
opt_data <- data.frame(
  moran.i = c(0.5, 0.3, 0.2, 0.15),
  r.squared = c(0.6, 0.65, 0.68, 0.69),
  penalization.per.variable = c(0.1, 0.2, 0.3, 0.4),
  p.value.binary = c(0, 0, 1, 1)
)

# Compute optimization scores
scores_moran <- optimization_function(
  x = opt_data,
  weight.r.squared = 0.5,
  weight.penalization.n.predictors = 0.5,
  optimization.method = "moran.i"
)

# Compare methods
scores_pvalue <- optimization_function(
  x = opt_data,
  weight.r.squared = 0.5,
  weight.penalization.n.predictors = 0.5,
  optimization.method = "p.value"
)

# Higher score indicates better solution
which.max(scores_moran)
which.max(scores_pvalue)
} # }
```
