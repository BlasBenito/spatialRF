# Generate case weights for imbalanced binary data

Generates case weights to balance binary response variables for use with
`ranger` models. Used internally by
[`rf()`](https://blasbenito.github.io/spatialRF/reference/rf.md).

## Usage

``` r
case_weights(data = NULL, dependent.variable.name = NULL)
```

## Arguments

- data:

  Data frame containing the response variable. Default: `NULL`.

- dependent.variable.name:

  Character string specifying the response variable name. Must be a
  column in `data`. Default: `NULL`.

## Value

Numeric vector of length `nrow(data)` with case weights. Each weight is
the inverse of the class frequency: `1/n_zeros` for 0s and `1/n_ones`
for 1s.

## Details

The weighting scheme assigns higher weights to the minority class to
balance training:

- Cases with value 0: weight = `1 / n_zeros`

- Cases with value 1: weight = `1 / n_ones`

This ensures both classes contribute equally to model training
regardless of class imbalance.

## See also

Other preprocessing:
[`auto_cor()`](https://blasbenito.github.io/spatialRF/reference/auto_cor.md),
[`auto_vif()`](https://blasbenito.github.io/spatialRF/reference/auto_vif.md),
[`default_distance_thresholds()`](https://blasbenito.github.io/spatialRF/reference/default_distance_thresholds.md),
[`double_center_distance_matrix()`](https://blasbenito.github.io/spatialRF/reference/double_center_distance_matrix.md),
[`is_binary()`](https://blasbenito.github.io/spatialRF/reference/is_binary.md),
[`make_spatial_fold()`](https://blasbenito.github.io/spatialRF/reference/make_spatial_fold.md),
[`make_spatial_folds()`](https://blasbenito.github.io/spatialRF/reference/make_spatial_folds.md),
[`the_feature_engineer()`](https://blasbenito.github.io/spatialRF/reference/the_feature_engineer.md),
[`weights_from_distance_matrix()`](https://blasbenito.github.io/spatialRF/reference/weights_from_distance_matrix.md)

## Examples

``` r
# Imbalanced dataset: 3 zeros, 2 ones
weights <- case_weights(
  data = data.frame(
    response = c(0, 0, 0, 1, 1)
  ),
  dependent.variable.name = "response"
)

weights
#> [1] 0.3333333 0.3333333 0.3333333 0.5000000 0.5000000
# Returns: 0.333, 0.333, 0.333, 0.5, 0.5
# Zeros get weight 1/3, ones get weight 1/2
```
