# Create multiple spatially independent training and testing folds

Applies
[`make_spatial_fold()`](https://blasbenito.github.io/spatialRF/reference/make_spatial_fold.md)
to every row in `xy.selected`, generating one spatially independent fold
centered on each focal point. Used for spatial cross-validation in
[`rf_evaluate()`](https://blasbenito.github.io/spatialRF/reference/rf_evaluate.md).

## Usage

``` r
make_spatial_folds(
  data = NULL,
  dependent.variable.name = NULL,
  xy.selected = NULL,
  xy = NULL,
  distance.step.x = NULL,
  distance.step.y = NULL,
  training.fraction = 0.75,
  n.cores = parallel::detectCores() - 1,
  cluster = NULL
)
```

## Arguments

- data:

  Data frame containing response variable and predictors. Required only
  for binary response variables.

- dependent.variable.name:

  Character string with the name of the response variable. Must be a
  column name in `data`. Required only for binary response variables.

- xy.selected:

  Data frame with columns "x" (longitude), "y" (latitude), and "id"
  (record identifier). Defines the focal points for fold creation.
  Typically a spatially thinned subset of `xy` created with
  [`thinning()`](https://blasbenito.github.io/spatialRF/reference/thinning.md)
  or
  [`thinning_til_n()`](https://blasbenito.github.io/spatialRF/reference/thinning_til_n.md).

- xy:

  Data frame with columns "x" (longitude), "y" (latitude), and "id"
  (record identifier). Contains all spatial coordinates for the dataset.

- distance.step.x:

  Numeric value specifying the buffer growth increment along the x-axis.
  Default: `NULL` (automatically set to 1/1000th of the x-coordinate
  range).

- distance.step.y:

  Numeric value specifying the buffer growth increment along the y-axis.
  Default: `NULL` (automatically set to 1/1000th of the y-coordinate
  range).

- training.fraction:

  Numeric value between 0.1 and 0.9 specifying the fraction of records
  to include in the training fold. Default: `0.75`.

- n.cores:

  Integer specifying the number of CPU cores for parallel execution.
  Default: `parallel::detectCores() - 1`.

- cluster:

  Optional cluster object created with
  [`parallel::makeCluster()`](https://rdrr.io/r/parallel/makeCluster.html).
  If provided, overrides `n.cores`. User is responsible for stopping the
  cluster with
  [`parallel::stopCluster()`](https://rdrr.io/r/parallel/makeCluster.html).
  Default: `NULL`.

## Value

List where each element corresponds to a row in `xy.selected` and
contains:

- `training`: Integer vector of record IDs (from `xy$id`) in the
  training fold.

- `testing`: Integer vector of record IDs (from `xy$id`) in the testing
  fold.

## Details

This function creates multiple spatially independent folds for spatial
cross-validation by calling
[`make_spatial_fold()`](https://blasbenito.github.io/spatialRF/reference/make_spatial_fold.md)
once for each row in `xy.selected`. Each fold is created by growing a
rectangular buffer from the corresponding focal point until the desired
`training.fraction` is achieved.

**Parallel execution:**

The function uses parallel processing to speed up fold creation. You can
control parallelization with `n.cores` or provide a pre-configured
cluster object.

**Typical workflow:**

1.  Thin spatial points with
    [`thinning()`](https://blasbenito.github.io/spatialRF/reference/thinning.md)
    or
    [`thinning_til_n()`](https://blasbenito.github.io/spatialRF/reference/thinning_til_n.md)
    to create `xy.selected`

2.  Create spatial folds with this function

3.  Use the folds for spatial cross-validation in
    [`rf_evaluate()`](https://blasbenito.github.io/spatialRF/reference/rf_evaluate.md)

## See also

[`make_spatial_fold()`](https://blasbenito.github.io/spatialRF/reference/make_spatial_fold.md),
[`rf_evaluate()`](https://blasbenito.github.io/spatialRF/reference/rf_evaluate.md),
[`thinning()`](https://blasbenito.github.io/spatialRF/reference/thinning.md),
[`thinning_til_n()`](https://blasbenito.github.io/spatialRF/reference/thinning_til_n.md)

Other preprocessing:
[`auto_cor()`](https://blasbenito.github.io/spatialRF/reference/auto_cor.md),
[`auto_vif()`](https://blasbenito.github.io/spatialRF/reference/auto_vif.md),
[`case_weights()`](https://blasbenito.github.io/spatialRF/reference/case_weights.md),
[`default_distance_thresholds()`](https://blasbenito.github.io/spatialRF/reference/default_distance_thresholds.md),
[`double_center_distance_matrix()`](https://blasbenito.github.io/spatialRF/reference/double_center_distance_matrix.md),
[`is_binary()`](https://blasbenito.github.io/spatialRF/reference/is_binary.md),
[`make_spatial_fold()`](https://blasbenito.github.io/spatialRF/reference/make_spatial_fold.md),
[`the_feature_engineer()`](https://blasbenito.github.io/spatialRF/reference/the_feature_engineer.md),
[`weights_from_distance_matrix()`](https://blasbenito.github.io/spatialRF/reference/weights_from_distance_matrix.md)

## Examples

``` r
data(plants_df, plants_xy)

# Thin to 10 focal points to speed up example
xy.thin <- thinning_til_n(
  xy = plants_xy,
  n = 10
)

# Create spatial folds centered on the 10 thinned points
folds <- make_spatial_folds(
  xy.selected = xy.thin,
  xy = plants_xy,
  distance.step.x = 0.05,
  training.fraction = 0.6,
  n.cores = 1
)

# Each element is a fold with training and testing indices
length(folds)  # 10 folds
#> [1] 10
names(folds[[1]])  # "training" and "testing"
#> [1] "training" "testing" 

# Visualize first fold (training = red, testing = blue, center = black)
if (interactive()) {
  plot(plants_xy[c("x", "y")], type = "n", xlab = "", ylab = "")
  points(plants_xy[folds[[1]]$training, c("x", "y")], col = "red4", pch = 15)
  points(plants_xy[folds[[1]]$testing, c("x", "y")], col = "blue4", pch = 15)
  points(
    plants_xy[folds[[1]]$training[1], c("x", "y")],
    col = "black",
    pch = 15,
    cex = 2
  )
}
```
