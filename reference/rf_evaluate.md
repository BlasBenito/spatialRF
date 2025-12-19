# Evaluates random forest models with spatial cross-validation

Evaluates the performance of random forest on unseen data over
independent spatial folds.

## Usage

``` r
rf_evaluate(
  model = NULL,
  xy = NULL,
  repetitions = 30,
  training.fraction = 0.75,
  metrics = c("r.squared", "pseudo.r.squared", "rmse", "nrmse", "auc"),
  distance.step = NULL,
  distance.step.x = NULL,
  distance.step.y = NULL,
  grow.testing.folds = FALSE,
  seed = 1,
  verbose = TRUE,
  n.cores = parallel::detectCores() - 1,
  cluster = NULL
)
```

## Arguments

- model:

  Model fitted with
  [`rf()`](https://blasbenito.github.io/spatialRF/reference/rf.md),
  [`rf_repeat()`](https://blasbenito.github.io/spatialRF/reference/rf_repeat.md),
  or
  [`rf_spatial()`](https://blasbenito.github.io/spatialRF/reference/rf_spatial.md).

- xy:

  Data frame or matrix with two columns containing coordinates and named
  "x" and "y". If `NULL`, the function will throw an error. Default:
  `NULL`

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
  (`rmse/(quantile(obs, 0.75) - quantile(obs, 0.25))`), and "auc" (only
  for binary responses with values 1 and 0). Default:
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

- grow.testing.folds:

  Logic. By default, this function grows contiguous training folds to
  keep the spatial structure of the data as intact as possible. However,
  when setting `grow.testing.folds = TRUE`, the argument
  `training.fraction` is set to `1 - training.fraction`, and the
  training and testing folds are switched. This option might be useful
  when the training data has a spatial structure that does not match
  well with the default behavior of the function. Default: `FALSE`

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

A model of the class "rf_evaluate" with a new slot named "evaluation",
that is a list with the following slots:

- `training.fraction`: Value of the argument `training.fraction`.

- `spatial.folds`: Result of applying
  [`make_spatial_folds()`](https://blasbenito.github.io/spatialRF/reference/make_spatial_folds.md)
  on the data coordinates. It is a list with as many slots as
  `repetitions` are indicated by the user. Each slot has two slots named
  "training" and "testing", each one having the indices of the cases
  used on the training and testing models.

- `per.fold`: Data frame with the evaluation results per spatial fold
  (or repetition). It contains the ID of each fold, it's central
  coordinates, the number of training and testing cases, and the
  training and testing performance measures: R squared, pseudo R squared
  (cor(observed, predicted)), rmse, and normalized rmse.

- `per.model`: Same data as above, but organized per fold and model
  ("Training", "Testing", and "Full").

- `aggregated`: Same data, but aggregated by model and performance
  measure.

## Details

The evaluation algorithm works as follows: the number of `repetitions`
and the input dataset (stored in `model$ranger.arguments$data`) are used
as inputs for the function
[`thinning_til_n()`](https://blasbenito.github.io/spatialRF/reference/thinning_til_n.md),
that applies
[`thinning()`](https://blasbenito.github.io/spatialRF/reference/thinning.md)
to the input data until as many cases as `repetitions` are left, and as
separated as possible. Each of these remaining records will be used as a
"fold center". From that point, the fold grows, until a number of points
equal (or close) to `training.fraction` is reached. The indices of the
records within the grown spatial fold are stored as "training" in the
output list, and the remaining ones as "testing". Then, for each spatial
fold, a "training model" is fitted using the cases corresponding with
the training indices, and predicted over the cases corresponding with
the testing indices. The model predictions on the "unseen" data are
compared with the observations, and the performance measures (R squared,
pseudo R squared, RMSE and NRMSE) computed.

## See also

Other model_workflow:
[`rf_compare()`](https://blasbenito.github.io/spatialRF/reference/rf_compare.md),
[`rf_importance()`](https://blasbenito.github.io/spatialRF/reference/rf_importance.md),
[`rf_repeat()`](https://blasbenito.github.io/spatialRF/reference/rf_repeat.md),
[`rf_tuning()`](https://blasbenito.github.io/spatialRF/reference/rf_tuning.md)

## Examples

``` r
if(interactive()){

data(
  plants_rf,
  plants_xy
)

plants_rf <- rf_evaluate(
  model = plants_rf,
  xy = plants_xy,
  repetitions = 5,
  n.cores = 1
)

plot_evaluation(plants_rf, notch = FALSE)

print_evaluation(plants_rf)

get_evaluation(plants_rf)

}
```
