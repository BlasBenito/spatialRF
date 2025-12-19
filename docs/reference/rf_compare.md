# Compares models via spatial cross-validation

Uses
[`rf_evaluate()`](https://blasbenito.github.io/spatialRF/reference/rf_evaluate.md)
to compare the performance of several models on independent spatial
folds via spatial cross-validation.

## Usage

``` r
rf_compare(
  models = NULL,
  xy = NULL,
  repetitions = 30,
  training.fraction = 0.75,
  metrics = c("r.squared", "pseudo.r.squared", "rmse", "nrmse", "auc"),
  distance.step = NULL,
  distance.step.x = NULL,
  distance.step.y = NULL,
  fill.color = viridis::viridis(100, option = "F", direction = -1, alpha = 0.8),
  line.color = "gray30",
  seed = 1,
  verbose = TRUE,
  n.cores = parallel::detectCores() - 1,
  cluster = NULL
)
```

## Arguments

- models:

  Named list with models resulting from
  [`rf()`](https://blasbenito.github.io/spatialRF/reference/rf.md),
  [`rf_spatial()`](https://blasbenito.github.io/spatialRF/reference/rf_spatial.md),
  [`rf_tuning()`](https://blasbenito.github.io/spatialRF/reference/rf_tuning.md),
  or
  [`rf_evaluate()`](https://blasbenito.github.io/spatialRF/reference/rf_evaluate.md).
  Example: `models = list(a = model.a, b = model.b)`. Default: `NULL`

- xy:

  Data frame or matrix with two columns containing coordinates and named
  "x" and "y". Default: `NULL`

- repetitions:

  Integer, number of spatial folds to use during cross-validation. Must
  be lower than the total number of rows available in the model's data.
  Default: `30`

- training.fraction:

  Proportion between 0.5 and 0.9 indicating the proportion of records to
  be used as training set during spatial cross-validation. Default:
  `0.75`

- metrics:

  Character vector, names of the performance metrics selected. The
  possible values are: "r.squared" (`cor(obs, pred) ^ 2`),
  "pseudo.r.squared" (`cor(obs, pred)`), "rmse"
  (`sqrt(sum((obs - pred)^2)/length(obs))`), "nrmse"
  (`rmse/(quantile(obs, 0.75) - quantile(obs, 0.25))`). Default:
  `c("r.squared", "pseudo.r.squared", "rmse", "nrmse")`

- distance.step:

  Numeric, argument `distance.step` of
  [`thinning_til_n()`](https://blasbenito.github.io/spatialRF/reference/thinning_til_n.md).
  distance step used during the selection of the centers of the training
  folds. These fold centers are selected by thinning the data until a
  number of folds equal or lower than `repetitions` is reached. Its
  default value is 1/1000th the maximum distance within records in `xy`.
  Reduce it if the number of training folds is lower than expected.

- distance.step.x:

  Numeric, argument `distance.step.x` of
  [`make_spatial_folds()`](https://blasbenito.github.io/spatialRF/reference/make_spatial_folds.md).
  Distance step used during the growth in the x axis of the buffers
  defining the training folds. Default: `NULL` (1/1000th the range of
  the x coordinates).

- distance.step.y:

  Numeric, argument `distance.step.x` of
  [`make_spatial_folds()`](https://blasbenito.github.io/spatialRF/reference/make_spatial_folds.md).
  Distance step used during the growth in the y axis of the buffers
  defining the training folds. Default: `NULL` (1/1000th the range of
  the y coordinates).

- fill.color:

  Character vector with hexadecimal codes (e.g. "#440154FF" "#21908CFF"
  "#FDE725FF"), or function generating a palette (e.g.
  `viridis::viridis(100)`). Default:
  `viridis::viridis(100, option = "F", direction = -1)`

- line.color:

  Character string, color of the line produced by
  [`ggplot2::geom_smooth()`](https://ggplot2.tidyverse.org/reference/geom_smooth.html).
  Default: `"gray30"`

- seed:

  Integer, random seed to facilitate reproduciblity. If set to a given
  number, the results of the function are always the same. Default: `1`.

- verbose:

  Logical. If `TRUE`, messages and plots generated during the execution
  of the function are displayed, Default: `TRUE`

- n.cores:

  Integer, number of cores to use for parallel execution. Creates a
  socket cluster with
  [`parallel::makeCluster()`](https://rdrr.io/r/parallel/makeCluster.html),
  runs operations in parallel with `foreach` and `%dopar%`, and stops
  the cluster with `parallel::clusterStop()` when the job is done.
  Default: `parallel::detectCores() - 1`

- cluster:

  A cluster definition generated with
  [`parallel::makeCluster()`](https://rdrr.io/r/parallel/makeCluster.html).
  If provided, overrides `n.cores`. When `cluster = NULL` (default
  value), and `model` is provided, the cluster in `model`, if any, is
  used instead. If this cluster is `NULL`, then the function uses
  `n.cores` instead. The function does not stop a provided cluster, so
  it should be stopped with
  [`parallel::stopCluster()`](https://rdrr.io/r/parallel/makeCluster.html)
  afterwards. The cluster definition is stored in the output list under
  the name "cluster" so it can be passed to other functions via the
  `model` argument, or using the `%>%` pipe. Default: `NULL`

## Value

A list with three slots:

- `comparison.df`: Data frame with one performance value per spatial
  fold, metric, and model.

- `spatial.folds`: List with the indices of the training and testing
  records for each evaluation repetition.

- `plot`: Violin-plot of `comparison.df`.

## See also

[`rf_evaluate()`](https://blasbenito.github.io/spatialRF/reference/rf_evaluate.md)

Other model_workflow:
[`rf_evaluate()`](https://blasbenito.github.io/spatialRF/reference/rf_evaluate.md),
[`rf_importance()`](https://blasbenito.github.io/spatialRF/reference/rf_importance.md),
[`rf_repeat()`](https://blasbenito.github.io/spatialRF/reference/rf_repeat.md),
[`rf_tuning()`](https://blasbenito.github.io/spatialRF/reference/rf_tuning.md)

## Examples

``` r
if(interactive()){

  data(
    plants_rf,
    plants_rf_spatial,
    plants_xy
  )

  comparison <- rf_compare(
    models = list(
      `Non spatial` = plants_rf,
      Spatial = plants_rf_spatial
    ),
    repetitions = 5,
    xy = plants_xy,
    metrics = "rmse",
    n.cores = 1
  )

}
```
