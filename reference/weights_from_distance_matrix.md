# Transforms a distance matrix into a matrix of weights

Transforms a distance matrix into weights (1/distance.matrix) normalized
by the row sums. Used to compute Moran's I values and Moran's
Eigenvector Maps. Allows to apply a threshold to the distance matrix
before computing the weights.

## Usage

``` r
weights_from_distance_matrix(
  distance.matrix = NULL,
  distance.threshold = 0
)
```

## Arguments

- distance.matrix:

  Distance matrix. Default: `NULL`.

- distance.threshold:

  Numeric, positive, in the range of values of `distance.matrix`.
  Distances below this value in the distance matrix are set to 0.,
  Default: `0`.

## Value

A weighted distance matrix.

## See also

Other preprocessing:
[`auto_cor()`](https://blasbenito.github.io/spatialRF/reference/auto_cor.md),
[`auto_vif()`](https://blasbenito.github.io/spatialRF/reference/auto_vif.md),
[`case_weights()`](https://blasbenito.github.io/spatialRF/reference/case_weights.md),
[`default_distance_thresholds()`](https://blasbenito.github.io/spatialRF/reference/default_distance_thresholds.md),
[`double_center_distance_matrix()`](https://blasbenito.github.io/spatialRF/reference/double_center_distance_matrix.md),
[`is_binary()`](https://blasbenito.github.io/spatialRF/reference/is_binary.md),
[`make_spatial_fold()`](https://blasbenito.github.io/spatialRF/reference/make_spatial_fold.md),
[`make_spatial_folds()`](https://blasbenito.github.io/spatialRF/reference/make_spatial_folds.md),
[`the_feature_engineer()`](https://blasbenito.github.io/spatialRF/reference/the_feature_engineer.md)

## Examples

``` r
data(plants_distance)

y <- weights_from_distance_matrix(
  distance.matrix = plants_distance
)

y[1:5, 1:5]
#>     columns
#> rows            1            2            3            4            5
#>    1 0.000000e+00 8.547100e-05 1.342814e-05 1.625653e-04 5.150753e-03
#>    2 6.139458e-05 0.000000e+00 1.826840e-05 2.632839e-04 2.903626e-05
#>    3 1.663113e-05 3.149888e-05 0.000000e+00 1.875990e-05 1.435260e-05
#>    4 3.743251e-04 8.439839e-04 3.487753e-05 0.000000e+00 1.287678e-04
#>    5 5.128856e-03 4.025125e-05 1.153917e-05 5.568472e-05 0.000000e+00
```
