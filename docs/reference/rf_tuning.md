# Tuning of random forest hyperparameters via spatial cross-validation

Finds the optimal set of random forest hyperparameters `num.trees`,
`mtry`, and `min.node.size` via grid search by maximizing the model's R
squared, or AUC, if the response variable is binomial, via spatial
cross-validation performed with
[`rf_evaluate()`](https://blasbenito.github.io/spatialRF/reference/rf_evaluate.md).

## Usage

``` r
rf_tuning(
  model = NULL,
  num.trees = NULL,
  mtry = NULL,
  min.node.size = NULL,
  xy = NULL,
  repetitions = 30,
  training.fraction = 0.75,
  seed = 1,
  verbose = TRUE,
  n.cores = parallel::detectCores() - 1,
  cluster = NULL
)
```

## Arguments

- model:

  A model fitted with
  [`rf()`](https://blasbenito.github.io/spatialRF/reference/rf.md). If
  provided, the training data is taken directly from the model
  definition (stored in `model$ranger.arguments`). Default: `NULL`

- num.trees:

  Numeric integer vector with the number of trees to fit on each model
  repetition. Default: `c(500, 1000, 2000)`.

- mtry:

  Numeric integer vector, number of predictors to randomly select from
  the complete pool of predictors on each tree split. Default:
  `floor(seq(1, length(predictor.variable.names), length.out = 4))`

- min.node.size:

  Numeric integer, minimal number of cases in a terminal node. Default:
  `c(5, 10, 20, 40)`

- xy:

  Data frame or matrix with two columns containing coordinates and named
  "x" and "y". If `NULL`, the function will throw an error. Default:
  `NULL`

- repetitions:

  Integer, number of independent spatial folds to use during the
  cross-validation. Default: `30`.

- training.fraction:

  Proportion between 0.2 and 0.9 indicating the number of records to be
  used in model training. Default: `0.75`

- seed:

  Integer, random seed to facilitate reproduciblity. If set to a given
  number, the results of the function are always the same. Default: `1`.

- verbose:

  Logical. If TRUE, messages and plots generated during the execution of
  the function are displayed, Default: `TRUE`

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

A model with a new slot named `tuning`, with a data frame with the
results of the tuning analysis.

## See also

[`rf_evaluate()`](https://blasbenito.github.io/spatialRF/reference/rf_evaluate.md)

Other model_workflow:
[`rf_compare()`](https://blasbenito.github.io/spatialRF/reference/rf_compare.md),
[`rf_evaluate()`](https://blasbenito.github.io/spatialRF/reference/rf_evaluate.md),
[`rf_importance()`](https://blasbenito.github.io/spatialRF/reference/rf_importance.md),
[`rf_repeat()`](https://blasbenito.github.io/spatialRF/reference/rf_repeat.md)

## Examples

``` r
if(interactive()){
  data(
    plants_rf,
    plants_xy
  )

  plants_rf_tuned <- rf_tuning(
    model = plants_rf,
    num.trees = c(25, 50),
    mtry = c(5, 10),
    min.node.size = c(10, 20),
    xy = plants_xy,
    repetitions = 5,
    n.cores = 1
  )

  plot_tuning(plants_rf_tuned)
}
```
