# Moran's I plots of a training data frame

Plots the the Moran's I test of the response and the predictors in a
training data frame.

## Usage

``` r
plot_training_df_moran(
  data = NULL,
  dependent.variable.name = NULL,
  predictor.variable.names = NULL,
  distance.matrix = NULL,
  distance.thresholds = NULL,
  fill.color = viridis::viridis(100, option = "F", direction = -1),
  point.color = "gray30"
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

  Character vector with the names of the predictive variables. Every
  element of this vector must be in the column names of `data`.
  Optionally, the result of
  [`auto_cor()`](https://blasbenito.github.io/spatialRF/reference/auto_cor.md)
  or
  [`auto_vif()`](https://blasbenito.github.io/spatialRF/reference/auto_vif.md)
  Default: `NULL`

- distance.matrix:

  Squared matrix with the distances among the records in `data`. The
  number of rows of `distance.matrix` and `data` must be the same. If
  not provided, the computation of the Moran's I of the residuals is
  omitted. Default: `NULL`

- distance.thresholds:

  Numeric vector, distances below each value are set to 0 on separated
  copies of the distance matrix for the computation of Moran's I at
  different neighborhood distances. If `NULL`, it defaults to
  `seq(0, max(distance.matrix)/4, length.out = 2)`. Default: `NULL`

- fill.color:

  Character vector with hexadecimal codes (e.g. "#440154FF" "#21908CFF"
  "#FDE725FF"), or function generating a palette (e.g.
  `viridis::viridis(100)`). Default:
  `viridis::viridis(100, option = "F", direction = -1)`

- point.color:

  Character vector with a color name (e.g. "red4"). Default: `gray30`

## Value

A ggplot2 object.

## See also

Other visualization:
[`plot_evaluation()`](https://blasbenito.github.io/spatialRF/reference/plot_evaluation.md),
[`plot_importance()`](https://blasbenito.github.io/spatialRF/reference/plot_importance.md),
[`plot_moran()`](https://blasbenito.github.io/spatialRF/reference/plot_moran.md),
[`plot_optimization()`](https://blasbenito.github.io/spatialRF/reference/plot_optimization.md),
[`plot_residuals_diagnostics()`](https://blasbenito.github.io/spatialRF/reference/plot_residuals_diagnostics.md),
[`plot_response_curves()`](https://blasbenito.github.io/spatialRF/reference/plot_response_curves.md),
[`plot_response_surface()`](https://blasbenito.github.io/spatialRF/reference/plot_response_surface.md),
[`plot_training_df()`](https://blasbenito.github.io/spatialRF/reference/plot_training_df.md),
[`plot_tuning()`](https://blasbenito.github.io/spatialRF/reference/plot_tuning.md)

## Examples

``` r
data(
  plants_df,
  plants_response,
  plants_predictors,
  plants_distance
)

plot_training_df_moran(
  data = plants_df,
  dependent.variable.name = plants_response,
  predictor.variable.names = plants_predictors[1:4],
  distance.matrix = plants_distance,
  distance.thresholds = c(1000, 2000, 4000)
)

```
