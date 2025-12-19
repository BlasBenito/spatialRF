# Check if variable is binary with values 0 and 1

Tests whether a variable contains only the values 0 and 1.

## Usage

``` r
is_binary(data = NULL, dependent.variable.name = NULL)
```

## Arguments

- data:

  Data frame containing the variable to check.

- dependent.variable.name:

  Character string with the name of the variable to test. Must be a
  column name in `data`.

## Value

Logical. `TRUE` if the variable contains exactly two unique values (0
and 1), `FALSE` otherwise.

## Details

This function is used internally by spatialRF to determine whether to
apply classification-specific methods (e.g., case weighting with
[`case_weights()`](https://blasbenito.github.io/spatialRF/reference/case_weights.md)).
The function returns `FALSE` if:

- The variable has more than two unique values

- The variable has only one unique value (constant)

- The unique values are not exactly 0 and 1 (e.g., 1 and 2, or TRUE and
  FALSE)

Missing values (NA) are ignored when determining unique values.

## See also

[`case_weights()`](https://blasbenito.github.io/spatialRF/reference/case_weights.md)

Other preprocessing:
[`auto_cor()`](https://blasbenito.github.io/spatialRF/reference/auto_cor.md),
[`auto_vif()`](https://blasbenito.github.io/spatialRF/reference/auto_vif.md),
[`case_weights()`](https://blasbenito.github.io/spatialRF/reference/case_weights.md),
[`default_distance_thresholds()`](https://blasbenito.github.io/spatialRF/reference/default_distance_thresholds.md),
[`double_center_distance_matrix()`](https://blasbenito.github.io/spatialRF/reference/double_center_distance_matrix.md),
[`make_spatial_fold()`](https://blasbenito.github.io/spatialRF/reference/make_spatial_fold.md),
[`make_spatial_folds()`](https://blasbenito.github.io/spatialRF/reference/make_spatial_folds.md),
[`the_feature_engineer()`](https://blasbenito.github.io/spatialRF/reference/the_feature_engineer.md),
[`weights_from_distance_matrix()`](https://blasbenito.github.io/spatialRF/reference/weights_from_distance_matrix.md)

## Examples

``` r
# Binary variable (returns TRUE)
is_binary(
  data = data.frame(response = c(0, 0, 0, 1, 1)),
  dependent.variable.name = "response"
)
#> [1] TRUE

# Non-binary variable (returns FALSE)
is_binary(
  data = data.frame(response = c(1, 2, 3, 4, 5)),
  dependent.variable.name = "response"
)
#> [1] FALSE

# Binary but wrong values (returns FALSE)
is_binary(
  data = data.frame(response = c(1, 1, 2, 2)),
  dependent.variable.name = "response"
)
#> [1] FALSE
```
