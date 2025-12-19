# Plots the response surfaces of a random forest model

Plots response surfaces for any given pair of predictors in a
[`rf()`](https://blasbenito.github.io/spatialRF/reference/rf.md),
[`rf_repeat()`](https://blasbenito.github.io/spatialRF/reference/rf_repeat.md),
or
[`rf_spatial()`](https://blasbenito.github.io/spatialRF/reference/rf_spatial.md)
model.

## Usage

``` r
plot_response_surface(
  model = NULL,
  a = NULL,
  b = NULL,
  quantiles = 0.5,
  grid.resolution = 100,
  point.size.range = c(0.5, 2.5),
  point.alpha = 1,
  fill.color = viridis::viridis(100, option = "F", direction = -1, alpha = 0.9),
  point.color = "gray30",
  verbose = TRUE
)
```

## Arguments

- model:

  A model fitted with
  [`rf()`](https://blasbenito.github.io/spatialRF/reference/rf.md),
  [`rf_repeat()`](https://blasbenito.github.io/spatialRF/reference/rf_repeat.md),
  or
  [`rf_spatial()`](https://blasbenito.github.io/spatialRF/reference/rf_spatial.md).
  Default `NULL`

- a:

  Character string, name of a model predictor. If `NULL`, the most
  important variable in `model` is selected. Default: `NULL`

- b:

  Character string, name of a model predictor. If `NULL`, the second
  most important variable in `model` is selected. Default: `NULL`

- quantiles:

  Numeric vector between 0 and 1. Argument `probs` of the function
  [quantile](https://rdrr.io/r/stats/quantile.html). Quantiles to set
  the other variables to. Default: `0.5`

- grid.resolution:

  Integer between 20 and 500. Resolution of the plotted surface Default:
  `100`

- point.size.range:

  Numeric vector of length 2 with the range of point sizes used by
  [geom_point](https://ggplot2.tidyverse.org/reference/geom_point.html).
  Using `c(-1, -1)` removes the points. Default: `c(0.5, 2.5)`

- point.alpha:

  Numeric between 0 and 1, transparency of the points. Setting it to `0`
  removes all points. Default: `1`.

- fill.color:

  Character vector with hexadecimal codes (e.g. "#440154FF" "#21908CFF"
  "#FDE725FF"), or function generating a palette (e.g.
  `viridis::viridis(100)`). Default:
  `viridis::viridis(100, option = "F", direction = -1, alpha = 0.9)`

- point.color:

  Character vector with a color name (e.g. "red4"). Default: `gray30`

- verbose:

  Logical, if TRUE the plot is printed. Default: `TRUE`

## Value

A list with slots named after the selected `quantiles`, each one with a
ggplot.

## Details

All variables that are not `a` or `b` in a response curve are set to the
values of their respective quantiles to plot the response surfaces. The
output list can be plotted all at once with `patchwork::wrap_plots(p)`
or `cowplot::plot_grid(plotlist = p)`, or one by one by extracting each
plot from the list.

## See also

[`plot_response_curves()`](https://blasbenito.github.io/spatialRF/reference/plot_response_curves.md)

Other visualization:
[`plot_evaluation()`](https://blasbenito.github.io/spatialRF/reference/plot_evaluation.md),
[`plot_importance()`](https://blasbenito.github.io/spatialRF/reference/plot_importance.md),
[`plot_moran()`](https://blasbenito.github.io/spatialRF/reference/plot_moran.md),
[`plot_optimization()`](https://blasbenito.github.io/spatialRF/reference/plot_optimization.md),
[`plot_residuals_diagnostics()`](https://blasbenito.github.io/spatialRF/reference/plot_residuals_diagnostics.md),
[`plot_response_curves()`](https://blasbenito.github.io/spatialRF/reference/plot_response_curves.md),
[`plot_training_df()`](https://blasbenito.github.io/spatialRF/reference/plot_training_df.md),
[`plot_training_df_moran()`](https://blasbenito.github.io/spatialRF/reference/plot_training_df_moran.md),
[`plot_tuning()`](https://blasbenito.github.io/spatialRF/reference/plot_tuning.md)

## Examples

``` r
data(plants_rf)

plot_response_surface(
  model = plants_rf,
  a = "climate_bio1_average",
  b = "human_population",
  grid.resolution = 50
)

```
