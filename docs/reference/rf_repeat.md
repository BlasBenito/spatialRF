# Fits several random forest models on the same data

Fits several random forest models on the same data in order to capture
the effect of the algorithm's stochasticity on the variable importance
scores, predictions, residuals, and performance measures. The function
relies on the median to aggregate performance and importance values
across repetitions. It is recommended to use it after a model is fitted
([`rf()`](https://blasbenito.github.io/spatialRF/reference/rf.md) or
[`rf_spatial()`](https://blasbenito.github.io/spatialRF/reference/rf_spatial.md)),
tuned
([`rf_tuning()`](https://blasbenito.github.io/spatialRF/reference/rf_tuning.md)),
and/or evaluated
([`rf_evaluate()`](https://blasbenito.github.io/spatialRF/reference/rf_evaluate.md)).
This function is designed to be used after fitting a model with
[`rf()`](https://blasbenito.github.io/spatialRF/reference/rf.md) or
[`rf_spatial()`](https://blasbenito.github.io/spatialRF/reference/rf_spatial.md),
tuning it with
[`rf_tuning()`](https://blasbenito.github.io/spatialRF/reference/rf_tuning.md)
and evaluating it with
[`rf_evaluate()`](https://blasbenito.github.io/spatialRF/reference/rf_evaluate.md).

## Usage

``` r
rf_repeat(
  model = NULL,
  data = NULL,
  dependent.variable.name = NULL,
  predictor.variable.names = NULL,
  distance.matrix = NULL,
  distance.thresholds = NULL,
  xy = NULL,
  ranger.arguments = NULL,
  scaled.importance = FALSE,
  repetitions = 10,
  keep.models = TRUE,
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
  provided, the data and ranger arguments are taken directly from the
  model definition (stored in `model$ranger.arguments`). Default: `NULL`

- data:

  Data frame with a response variable and a set of predictors. Default:
  `NULL`

- dependent.variable.name:

  Character string with the name of the response variable. Must be in
  the column names of `data`. If the dependent variable is binary with
  values 1 and 0, the argument `case.weights` of `ranger` is populated
  by the function
  [`case_weights()`](https://blasbenito.github.io/spatialRF/reference/case_weights.md).
  Default: `NULL`

- predictor.variable.names:

  Character vector with the names of the predictive variables. Every
  element of this vector must be in the column names of `data`. Default:
  `NULL`

- distance.matrix:

  Squared matrix with the distances among the records in `data`. The
  number of rows of `distance.matrix` and `data` must be the same. If
  not provided, the computation of the Moran's I of the residuals is
  omitted. Default: `NULL`

- distance.thresholds:

  Numeric vector with neighborhood distances. All distances in the
  distance matrix below each value in `dustance.thresholds` are set to 0
  for the computation of Moran's I. If `NULL`, it defaults to seq(0,
  max(distance.matrix), length.out = 4). Default: `NULL`

- xy:

  (optional) Data frame or matrix with two columns containing
  coordinates and named "x" and "y". It is not used by this function,
  but it is stored in the slot `ranger.arguments$xy` of the model, so it
  can be used by
  [`rf_evaluate()`](https://blasbenito.github.io/spatialRF/reference/rf_evaluate.md)
  and
  [`rf_tuning()`](https://blasbenito.github.io/spatialRF/reference/rf_tuning.md).
  Default: `NULL`

- ranger.arguments:

  Named list with
  [ranger](http://imbs-hl.github.io/ranger/reference/ranger.md)
  arguments (other arguments of this function can also go here). All
  [ranger](http://imbs-hl.github.io/ranger/reference/ranger.md)
  arguments are set to their default values except for 'importance',
  that is set to 'permutation' rather than 'none'. Please, consult the
  help file of
  [ranger](http://imbs-hl.github.io/ranger/reference/ranger.md) if you
  are not familiar with the arguments of this function.

- scaled.importance:

  Logical. If `TRUE`, and 'importance = "permutation', the function
  scales 'data' with [scale](https://rdrr.io/r/base/scale.html) and fits
  a new model to compute scaled variable importance scores. Default:
  `FALSE`

- repetitions:

  Integer, number of random forest models to fit. Default: `10`

- keep.models:

  Logical, if `TRUE`, the fitted models are returned in the `models`
  slot. Set to `FALSE` if the accumulation of models is creating issues
  with the RAM memory available. Default: `TRUE`.

- seed:

  Integer, random seed to facilitate reproduciblity. If set to a given
  number, the results of the function are always the same. Default: `1`.

- verbose:

  Logical, ff `TRUE`, messages and plots generated during the execution
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

A ranger model with several new slots:

- `ranger.arguments`: Stores the values of the arguments used to fit the
  ranger model.

- `importance`: A list containing a data frame with the predictors
  ordered by their importance, a ggplot showing the importance values,
  and local importance scores.

- `performance`: out-of-bag performance scores: R squared, pseudo R
  squared, RMSE, and normalized RMSE (NRMSE).

- `pseudo.r.squared`: computed as the correlation between the
  observations and the predictions.

- `residuals`: residuals, normality test of the residuals computed with
  [`residuals_test()`](https://blasbenito.github.io/spatialRF/reference/normality.md),
  and spatial autocorrelation of the residuals computed with
  [`moran_multithreshold()`](https://blasbenito.github.io/spatialRF/reference/moran_multithreshold.md).

## See also

Other model_workflow:
[`rf_compare()`](https://blasbenito.github.io/spatialRF/reference/rf_compare.md),
[`rf_evaluate()`](https://blasbenito.github.io/spatialRF/reference/rf_evaluate.md),
[`rf_importance()`](https://blasbenito.github.io/spatialRF/reference/rf_importance.md),
[`rf_tuning()`](https://blasbenito.github.io/spatialRF/reference/rf_tuning.md)

## Examples

``` r
if(interactive()){

  data(plants_rf)

  m_repeat <- rf_repeat(
    model = plants_rf,
    repetitions = 5,
    n.cores = 1
  )

  #performance scores across repetitions
  m_repeat$performance
  print_performance(m_repeat)

  #variable importance
  plot_importance(m_repeat)

  #response curves
  plot_response_curves(
    model = m_repeat,
    variables = "climate_bio1_average",
    quantiles = 0.5
  )

}
```
