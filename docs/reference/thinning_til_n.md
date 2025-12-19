# Applies thinning to pairs of coordinates until reaching a given n

Resamples a set of points with x and y coordinates by increasing the
distance step by step until a given sample size is obtained.

## Usage

``` r
thinning_til_n(
  xy,
  n = 30,
  distance.step = NULL
)
```

## Arguments

- xy:

  A data frame with columns named "x" and "y" representing geographic
  coordinates. Default: `NULL`

- n:

  Integer, number of samples to obtain. Must be lower than `nrow(xy)`.
  Default: `30`

- distance.step:

  Numeric, distance step used during the thinning iterations. If `NULL`,
  the one percent of the maximum distance among points in `xy` is used.
  Default: `NULL`

## Value

A data frame with the same columns as xy with a row number close to n.

## See also

[`thinning()`](https://blasbenito.github.io/spatialRF/reference/thinning.md)

Other utilities:
[`.vif_to_df()`](https://blasbenito.github.io/spatialRF/reference/dot-vif_to_df.md),
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
[`thinning()`](https://blasbenito.github.io/spatialRF/reference/thinning.md)

## Examples

``` r
data(plants_xy)

y <- thinning_til_n(
  xy = plants_xy,
  n = 10
)

if (interactive()) {
  plot(
    plants_xy[, c("x", "y")],
    col = "blue",
    pch = 15
  )

  points(
    y[, c("x", "y")],
    col = "red",
    pch = 15,
    cex = 1.5
  )
}
```
