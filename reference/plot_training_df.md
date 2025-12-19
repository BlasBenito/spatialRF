# Scatterplots of a training data frame

Plots the dependent variable against each predictor.

## Usage

``` r
plot_training_df(
  data = NULL,
  dependent.variable.name = NULL,
  predictor.variable.names = NULL,
  ncol = 4,
  method = "loess",
  point.color = viridis::viridis(100, option = "F"),
  line.color = "gray30"
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

- ncol:

  Number of columns of the plot. Argument `ncol` of
  [wrap_plots](https://patchwork.data-imaginist.com/reference/wrap_plots.html).

- method:

  Method for
  [geom_smooth](https://ggplot2.tidyverse.org/reference/geom_smooth.html),
  one of: "lm", "glm", "gam", "loess", or a function, for example
  [`mgcv::gam`](https://rdrr.io/pkg/mgcv/man/gam.html) Default: 'loess'

- point.color:

  Colors of the plotted points. Can be a single color name (e.g.
  "red4"), a character vector with hexadecimal codes (e.g. "#440154FF"
  "#21908CFF" "#FDE725FF"), or function generating a palette (e.g.
  `viridis::viridis(100)`). Default:
  `viridis::viridis(100, option = "F")`

- line.color:

  Character string, color of the line produced by
  [`ggplot2::geom_smooth()`](https://ggplot2.tidyverse.org/reference/geom_smooth.html).
  Default: `"gray30"`

## Value

A
[wrap_plots](https://patchwork.data-imaginist.com/reference/wrap_plots.html)
object.

## See also

Other visualization:
[`plot_evaluation()`](https://blasbenito.github.io/spatialRF/reference/plot_evaluation.md),
[`plot_importance()`](https://blasbenito.github.io/spatialRF/reference/plot_importance.md),
[`plot_moran()`](https://blasbenito.github.io/spatialRF/reference/plot_moran.md),
[`plot_optimization()`](https://blasbenito.github.io/spatialRF/reference/plot_optimization.md),
[`plot_residuals_diagnostics()`](https://blasbenito.github.io/spatialRF/reference/plot_residuals_diagnostics.md),
[`plot_response_curves()`](https://blasbenito.github.io/spatialRF/reference/plot_response_curves.md),
[`plot_response_surface()`](https://blasbenito.github.io/spatialRF/reference/plot_response_surface.md),
[`plot_training_df_moran()`](https://blasbenito.github.io/spatialRF/reference/plot_training_df_moran.md),
[`plot_tuning()`](https://blasbenito.github.io/spatialRF/reference/plot_tuning.md)

## Examples

``` r
data(
  plants_df,
  plants_response,
  plants_predictors
)

plot_training_df(
  data = plants_df,
  dependent.variable.name = plants_response,
  predictor.variable.names = plants_predictors[1:4]
)

```
