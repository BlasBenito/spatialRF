# Fits spatial random forest models

Fits spatial random forest models using different methods to generate,
rank, and select spatial predictors acting as proxies of spatial
processes not considered by the non-spatial predictors. The end goal is
providing the model with information about the spatial structure of the
data to minimize the spatial correlation (Moran's I) of the model
residuals and generate honest variable importance scores.

## Usage

``` r
rf_spatial(
  model = NULL,
  data = NULL,
  dependent.variable.name = NULL,
  predictor.variable.names = NULL,
  distance.matrix = NULL,
  distance.thresholds = NULL,
  xy = NULL,
  ranger.arguments = NULL,
  scaled.importance = TRUE,
  method = c("mem.moran.sequential", "mem.effect.sequential", "mem.effect.recursive",
    "hengl", "hengl.moran.sequential", "hengl.effect.sequential",
    "hengl.effect.recursive", "pca.moran.sequential", "pca.effect.sequential",
    "pca.effect.recursive"),
  max.spatial.predictors = NULL,
  weight.r.squared = NULL,
  weight.penalization.n.predictors = NULL,
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
  used, the arguments `data`, `dependent.variable.name`,
  `predictor.variable.names`, `distance.matrix`, `distance.thresholds`,
  `ranger.arguments`, and `scaled.importance` are taken directly from
  the model definition. Default: NULL

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

  Numeric vector with distances in the same units as `distance.matrix`
  Distances below each distance threshold are set to 0 on separated
  copies of the distance matrix to compute Moran's I at different
  neighborhood distances. If `NULL`, it defaults to
  `seq(0, max(distance.matrix)/2, length.out = 4)` (defined by
  [`default_distance_thresholds()`](https://blasbenito.github.io/spatialRF/reference/default_distance_thresholds.md)).
  Default: `NULL`

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
  `TRUE`

- method:

  Character, method to build, rank, and select spatial predictors. One
  of:

  - "hengl"

  - "hengl.moran.sequential" (experimental)

  - "hengl.effect.sequential" (experimental)

  - "hengl.effect.recursive" (experimental)

  - "pca.moran.sequential" (experimental)

  - "pca.effect.sequential" (experimental)

  - "pca.effect.recursive" (experimental)

  - "mem.moran.sequential"

  - "mem.effect.sequential"

  - "mem.effect.recursive"

- max.spatial.predictors:

  Integer, maximum number of spatial predictors to generate. Useful when
  memory problems arise due to a large number of spatial predictors,
  Default: `NULL`

- weight.r.squared:

  Numeric between 0 and 1, weight of R-squared in the selection of
  spatial components. See Details, Default: `NULL`

- weight.penalization.n.predictors:

  Numeric between 0 and 1, weight of the penalization for adding an
  increasing number of spatial predictors during selection. Default:
  `NULL`

- seed:

  Integer, random seed to facilitate reproducibility. Default: `1`.

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

A ranger model with several new slots:

- `ranger.arguments`: Values of the arguments used to fit the ranger
  model.

- `importance`: A list containing the vector of variable importance as
  originally returned by ranger (scaled or not depending on the value of
  'scaled.importance'), a data frame with the predictors ordered by
  their importance, and a ggplot showing the importance values.

- `performance`: With the out-of-bag R squared, pseudo R squared, RMSE
  and NRMSE of the model.

- `residuals`: residuals, normality test of the residuals computed with
  [`residuals_test()`](https://blasbenito.github.io/spatialRF/reference/normality.md),
  and spatial autocorrelation of the residuals computed with
  [`moran_multithreshold()`](https://blasbenito.github.io/spatialRF/reference/moran_multithreshold.md).

- `spatial`: A list with four slots:

  - `method`: Character, method used to generate, rank, and select
    spatial predictors.

  - `names`: Character vector with the names of the selected spatial
    predictors. Not returned if the method is "hengl".

  - `optimization`: Criteria used to select the spatial predictors. Not
    returned if the method is "hengl".

  - `plot`: Plot of the criteria used to select the spatial predictors.
    Not returned if the method is "hengl".

## Details

The function uses three different methods to generate spatial predictors
("hengl", "pca", and "mem"), two methods to rank them in order to define
in what order they are introduced in the model ("effect" and "moran),
and two methods to select the spatial predictors that minimize the
spatial correlation of the model residuals ("sequential" and
"recursive"). All method names but "hengl" (that uses the complete
distance matrix as predictors in the spatial model) are named by
combining a method to generate the spatial predictors, a method to rank
them, and a method to select them, separated by a point. Examples are
"mem.moran.sequential" or "mem.effect.recursive". All combinations are
not possible, since the ranking method "moran" cannot be used with the
selection method "recursive" (because the logics behind them are very
different, see below). Methods to generate spatial predictors:

- `"hengl"`: named after the method RFsp presented in the paper "Random
  forest as a generic framework for predictive modeling of spatial and
  spatio-temporal variables", by Hengl et al. (2018), where the authors
  propose to use the distance matrix among records as predictors in
  spatial random forest models (RFsp method). In this function, all
  methods starting with "hengl" use either the complete distance matrix,
  or select columns of the distance matrix as spatial predictors.

- `"mem"`: Generates Moran's Eigenvector Maps, that is, the eigenvectors
  of the double-centered weights of the distance matrix. The method is
  described in "Spatial modelling: a comprehensive framework for
  principal coordinate analysis of neighbour matrices (PCNM)", by Dray
  et al. (2006), and "Statistical methods for temporal and space–time
  analysis of community composition data", by Legendre and Gauthier
  (2014).

- `"pca"`: Computes spatial predictors from the principal component
  analysis of a weighted distance matrix (see
  [`weights_from_distance_matrix()`](https://blasbenito.github.io/spatialRF/reference/weights_from_distance_matrix.md)).
  This is an experimental method, use with caution.

Methods to rank spatial predictors (see
[`rank_spatial_predictors()`](https://blasbenito.github.io/spatialRF/reference/rank_spatial_predictors.md)):

- `"moran"`: Computes the Moran's I of each spatial predictor, selects
  the ones with positive values, and ranks them from higher to lower
  Moran's I.

- `"effect"`: If a given non-spatial random forest model is defined as
  `y = p1 + ... + pn`, being `p1 + ... + pn` the set of predictors, for
  every spatial predictor generated (`spX`) a spatial model
  `y = p1 + ... + pn + spX` is fitted, and the Moran's I of its
  residuals is computed. The spatial predictors are then ranked by how
  much they help to reduce spatial autocorrelation between the
  non-spatial and the spatial model.

Methods to select spatial predictors:

- `"sequential"` (see
  [`select_spatial_predictors_sequential()`](https://blasbenito.github.io/spatialRF/reference/select_spatial_predictors_sequential.md)):
  The spatial predictors are added one by one in the order they were
  ranked, and once all spatial predictors are introduced, the best first
  n predictors are selected. This method is similar to the one employed
  in the MEM methodology (Moran's Eigenvector Maps) described in the
  paper "Spatial modelling: a comprehensive framework for principal
  coordinate analysis of neighbour matrices (PCNM)", by Dray et al.
  (2006), and "Statistical methods for temporal and space–time analysis
  of community composition data", by Legendre and Gauthier (2014). This
  method generally introduces tens of predictors into the model, but
  usually offers good results.

- `"recursive"` (see
  [`select_spatial_predictors_recursive()`](https://blasbenito.github.io/spatialRF/reference/select_spatial_predictors_recursive.md)):
  This method tries to find the smallest combination of spatial
  predictors that reduce the spatial correlation of the model's
  residuals the most. The algorithm goes as follows: 1. The first ranked
  spatial predictor is introduced into the model; 2. the remaining
  predictors are ranked again using the "effect" method, using the model
  in 1. as reference. The first spatial predictor in the resulting
  ranking is then introduced into the model, and the steps 1. and 2. are
  repeated until spatial predictors stop having an effect in reducing
  the Moran's I of the model residuals. This method takes longer to
  compute, but generates smaller sets of spatial predictors. This is an
  experimental method, use with caution.

Once ranking procedure is completed, an algorithm is used to select the
minimal subset of spatial predictors that reduce the most the Moran's I
of the residuals: for each new spatial predictor introduced in the
model, the Moran's I of the residuals, it's p-value, a binary version of
the p-value (0 if \< 0.05 and 1 if \>= 0.05), the R-squared of the
model, and a penalization linear with the number of spatial predictors
introduced (computed as
`(1 / total spatial predictors) * introduced spatial predictors`) are
rescaled between 0 and 1. Then, the optimization criteria is computed as
`max(1 - Moran's I, p-value binary) + (weight.r.squared * R-squared) - (weight.penalization.n.predictors * penalization)`.
The predictors from the first one to the one with the highest
optimization criteria are then selected as the best ones in reducing the
spatial correlation of the model residuals, and used along with `data`
to fit the final spatial model.

## See also

Other main_models:
[`rf()`](https://blasbenito.github.io/spatialRF/reference/rf.md)

## Examples

``` r
if (interactive()) {
  data(
    plants_df,
    plants_response,
    plants_predictors,
    plants_distance,
    plants_rf
  )

  #subset to speed up example
  idx <- 1:100
  plants_df <- plants_df[idx, ]
  plants_distance <- plants_distance[idx, idx]

  #fit spatial model from scratch
  m_spatial <- rf_spatial(
    data = plants_df,
    dependent.variable.name = plants_response,
    predictor.variable.names = plants_predictors,
    distance.matrix = plants_distance,
    distance.thresholds = c(100, 1000, 2000),
    method = "mem.moran.sequential",
    ranger.arguments = list(num.trees = 30),
    n.cores = 1
  )

  plot_residuals_diagnostics(m_spatial)

  #optimization of MEM selection
  plot_optimization(m_spatial)

  #from non-spatial to spatial model
  m_spatial <- rf_spatial(
    model = plants_rf
    )

}

```
