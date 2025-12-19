# Applies thinning to pairs of coordinates

Resamples a set of points with x and y coordinates to impose a minimum
distance among nearby points.

## Usage

``` r
thinning(xy, minimum.distance = NULL)
```

## Arguments

- xy:

  A data frame with columns named "x" and "y" representing geographic
  coordinates.

- minimum.distance:

  Numeric, minimum distance to be set between nearby points, in the same
  units as the coordinates of xy.

## Value

A data frame with the same columns as `xy` with points separated by the
defined minimum distance.

## Details

Generally used to remove redundant points that could produce
pseudo-replication, and to limit sampling bias by disaggregating
clusters of points.

## See also

[`thinning_til_n()`](https://blasbenito.github.io/spatialRF/reference/thinning_til_n.md)

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
[`thinning_til_n()`](https://blasbenito.github.io/spatialRF/reference/thinning_til_n.md)

## Examples

``` r
data(plants_xy)

y <- thinning(
  xy = plants_xy,
  minimum.distance = 10
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
    pch = 15
  )
}
```
