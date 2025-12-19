# Default distance thresholds for spatial predictors

Generates four evenly-spaced distance thresholds for spatial predictor
generation, ranging from 0 to half the maximum distance in the matrix.

## Usage

``` r
default_distance_thresholds(distance.matrix = NULL)
```

## Arguments

- distance.matrix:

  Numeric distance matrix (typically square and symmetric). Default:
  `NULL`.

## Value

Numeric vector of length 4 with distance thresholds (floored to
integers).

## Details

The maximum threshold is set to half the maximum distance to avoid
spatial predictors based on distances that are too large to capture
meaningful spatial autocorrelation. The four thresholds are evenly
spaced using [`seq()`](https://rdrr.io/r/base/seq.html) with
`length.out = 4`.

## See also

Other preprocessing:
[`auto_cor()`](https://blasbenito.github.io/spatialRF/reference/auto_cor.md),
[`auto_vif()`](https://blasbenito.github.io/spatialRF/reference/auto_vif.md),
[`case_weights()`](https://blasbenito.github.io/spatialRF/reference/case_weights.md),
[`double_center_distance_matrix()`](https://blasbenito.github.io/spatialRF/reference/double_center_distance_matrix.md),
[`is_binary()`](https://blasbenito.github.io/spatialRF/reference/is_binary.md),
[`make_spatial_fold()`](https://blasbenito.github.io/spatialRF/reference/make_spatial_fold.md),
[`make_spatial_folds()`](https://blasbenito.github.io/spatialRF/reference/make_spatial_folds.md),
[`the_feature_engineer()`](https://blasbenito.github.io/spatialRF/reference/the_feature_engineer.md),
[`weights_from_distance_matrix()`](https://blasbenito.github.io/spatialRF/reference/weights_from_distance_matrix.md)

## Examples

``` r
data(plants_distance)

thresholds <- default_distance_thresholds(
  distance.matrix = plants_distance
)

thresholds
#> [1]    0 2399 4798 7198
# Example output: c(0, 3333, 6666, 10000)
# Four evenly-spaced thresholds from 0 to max(plants_distance)/2
```
