# Suggest variable interactions and composite features for random forest models

Suggests candidate variable interactions and composite features able to
improve predictive accuracy over data not used to train the model via
spatial cross-validation with
[`rf_evaluate()`](https://blasbenito.github.io/spatialRF/reference/rf_evaluate.md).
For a pair of predictors `a` and `b`, interactions are build via
multiplication (`a * b`), while composite features are built by
extracting the first factor of a principal component analysis performed
with [`pca()`](https://blasbenito.github.io/spatialRF/reference/pca.md),
after rescaling `a` and `b` between 1 and 100. Interactions and
composite features are named `a..x..b` and `a..pca..b` respectively.

Candidate variables `a` and `b` are selected from those predictors in
`predictor.variable.names` with a variable importance above
`importance.threshold` (set by default to the median of the importance
scores).

For each interaction and composite feature, a model including all the
predictors plus the interaction or composite feature is fitted, and it's
R squared (or AUC if the response is binary) computed via spatial
cross-validation (see
[`rf_evaluate()`](https://blasbenito.github.io/spatialRF/reference/rf_evaluate.md))
is compared with the R squared of the model without interactions or
composite features.

From all the potential interactions screened, only those with a positive
increase in R squared (or AUC when the response is binomial) of the
model, a variable importance above the median, and a maximum correlation
among themselves and with the predictors in `predictor.variable.names`
not higher than `cor.threshold` (set to 0.5 by default) are selected.
Such a restrictive set of rules ensures that the selected interactions
can be used right away for modeling purposes without increasing model
complexity unnecessarily. However, the suggested variable interactions
might not make sense from a domain expertise standpoint, so please,
examine them with care.

The function returns the criteria used to select the interactions, and
the data required to use these interactions a model.

## Usage

``` r
the_feature_engineer(
  data = NULL,
  dependent.variable.name = NULL,
  predictor.variable.names = NULL,
  xy = NULL,
  ranger.arguments = NULL,
  repetitions = 30,
  training.fraction = 0.75,
  importance.threshold = 0.75,
  cor.threshold = 0.75,
  point.color = viridis::viridis(100, option = "F", alpha = 0.8),
  seed = NULL,
  verbose = TRUE,
  n.cores = parallel::detectCores() - 1,
  cluster = NULL
)
```

## Arguments

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

  Character vector with the names of the predictive variables, or object
  of class `"variable_selection"` produced by
  [`auto_vif()`](https://blasbenito.github.io/spatialRF/reference/auto_vif.md)
  and/or
  [`auto_cor()`](https://blasbenito.github.io/spatialRF/reference/auto_cor.md).
  Every element of this vector must be in the column names of `data`.
  Default: `NULL`

- xy:

  Data frame or matrix with two columns containing coordinates and named
  "x" and "y". If not provided, the comparison between models with and
  without variable interactions is not done.

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

- repetitions:

  Integer, number of spatial folds to use during cross-validation. Must
  be lower than the total number of rows available in the model's data.
  Default: `30`

- training.fraction:

  Proportion between 0.5 and 0.9 indicating the proportion of records to
  be used as training set during spatial cross-validation. Default:
  `0.75`

- importance.threshold:

  Numeric between 0 and 1, quantile of variable importance scores over
  which to select individual predictors to explore interactions among
  them. Larger values reduce the number of potential interactions
  explored. Default: `0.75`

- cor.threshold:

  Numeric, maximum Pearson correlation between any pair of the selected
  interactions, and between any interaction and the predictors in
  `predictor.variable.names`. Default: `0.75`

- point.color:

  Colors of the plotted points. Can be a single color name (e.g.
  "red4"), a character vector with hexadecimal codes (e.g. "#440154FF"
  "#21908CFF" "#FDE725FF"), or function generating a palette (e.g.
  `viridis::viridis(100)`). Default:
  `viridis::viridis(100, option = "F", alpha = 0.8)`

- seed:

  Integer, random seed to facilitate reproduciblity. If set to a given
  number, the results of the function are always the same. Default:
  `NULL`

- verbose:

  Logical. If `TRUE`, messages and plots generated during the execution
  of the function are displayed. Default: `TRUE`

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

A list with seven slots:

- `screening`: Data frame with selection scores of all the interactions
  considered.

- `selected`: Data frame with selection scores of the selected
  interactions.

- `df`: Data frame with the computed interactions.

- `plot`: List of plots of the selected interactions versus the response
  variable. The output list can be plotted all at once with
  `patchwork::wrap_plots(p)` or `cowplot::plot_grid(plotlist = p)`, or
  one by one by extracting each plot from the list.

- `data`: Data frame with the response variable, the predictors, and the
  selected interactions, ready to be used as `data` argument in the
  package functions.

- `dependent.variable.name`: Character, name of the response.

- `predictor.variable.names`: Character vector with the names of the
  predictors and the selected interactions.

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
[`weights_from_distance_matrix()`](https://blasbenito.github.io/spatialRF/reference/weights_from_distance_matrix.md)

## Examples

``` r
if (interactive()) {
  data(
    plants_df,
    plants_response,
    plants_predictors,
    plants_xy,
    plants_rf
  )

  #get five most important predictors from plants_rf to speed-up example
  predictors <- get_importance(plants_rf)[1:5, "variable"]

  #subset to speed-up example
  idx <- 1:30
  plants_df <- plants_df[idx, ]
  plants_xy <- plants_xy[idx, ]

  #data subsetted to speed-up example runtime
  y <- the_feature_engineer(
    data = plants_df,
    dependent.variable.name = plants_response,
    predictor.variable.names = predictors,
    xy = plants_xy,
    repetitions = 5,
    n.cores = 1,
    ranger.arguments = list(
      num.trees = 30
    ),
    verbose = TRUE
  )

  #all tested interactions
  y$screening

  #selected interaction (same as above in this case)
  y$selected

  #new column added to data
  head(y$data[, y$selected$interaction.name])
}
```
