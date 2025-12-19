# Plots the response curves of a model.

Plots the response curves of models fitted with
[`rf()`](https://blasbenito.github.io/spatialRF/reference/rf.md),
[`rf_repeat()`](https://blasbenito.github.io/spatialRF/reference/rf_repeat.md),
or
[`rf_spatial()`](https://blasbenito.github.io/spatialRF/reference/rf_spatial.md).

## Usage

``` r
plot_response_curves(
  model = NULL,
  variables = NULL,
  quantiles = c(0.1, 0.5, 0.9),
  grid.resolution = 200,
  line.color = viridis::viridis(length(quantiles), option = "F", end = 0.9),
  ncol = 2,
  show.data = FALSE,
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

- variables:

  Character vector, names of predictors to plot. If `NULL`, the most
  important variables (importance higher than the median) in `x` are
  selected. Default: `NULL`.

- quantiles:

  Numeric vector with values between 0 and 1, argument `probs` of
  [quantile](https://rdrr.io/r/stats/quantile.html). Quantiles to set
  the other variables to. Default: `c(0.1, 0.5, 0.9)`

- grid.resolution:

  Integer between 20 and 500. Resolution of the plotted curve Default:
  `100`

- line.color:

  Character vector with colors, or function to generate colors for the
  lines representing `quantiles`. Must have the same number of colors as
  `quantiles` are defined. Default:
  `viridis::viridis(length(quantiles), option = "F", end = 0.9)`

- ncol:

  Integer, argument of
  [wrap_plots](https://patchwork.data-imaginist.com/reference/wrap_plots.html).
  Defaults to the rounded squared root of the number of plots. Default:
  `2`

- show.data:

  Logical, if `TRUE`, the observed data is plotted along with the
  response curves. Default: `FALSE`

- verbose:

  Logical, if TRUE the plot is printed. Default: `TRUE`

## Value

A list with slots named after the selected `variables`, with one ggplot
each.

## Details

All variables that are not plotted in a particular response curve are
set to the values of their respective quantiles, and the response curve
for each one of these quantiles is shown in the plot. When the input
model was fitted with
[`rf_repeat()`](https://blasbenito.github.io/spatialRF/reference/rf_repeat.md)
with `keep.models = TRUE`, then the plot shows the median of all model
runs, and each model run separately as a thinner line. The output list
can be plotted all at once with `patchwork::wrap_plots(p)` or
`cowplot::plot_grid(plotlist = p)`, or one by one by extracting each
plot from the list.

## See also

[`plot_response_surface()`](https://blasbenito.github.io/spatialRF/reference/plot_response_surface.md)

Other visualization:
[`plot_evaluation()`](https://blasbenito.github.io/spatialRF/reference/plot_evaluation.md),
[`plot_importance()`](https://blasbenito.github.io/spatialRF/reference/plot_importance.md),
[`plot_moran()`](https://blasbenito.github.io/spatialRF/reference/plot_moran.md),
[`plot_optimization()`](https://blasbenito.github.io/spatialRF/reference/plot_optimization.md),
[`plot_residuals_diagnostics()`](https://blasbenito.github.io/spatialRF/reference/plot_residuals_diagnostics.md),
[`plot_response_surface()`](https://blasbenito.github.io/spatialRF/reference/plot_response_surface.md),
[`plot_training_df()`](https://blasbenito.github.io/spatialRF/reference/plot_training_df.md),
[`plot_training_df_moran()`](https://blasbenito.github.io/spatialRF/reference/plot_training_df_moran.md),
[`plot_tuning()`](https://blasbenito.github.io/spatialRF/reference/plot_tuning.md)

## Examples

``` r
data(plants_rf)

plot_response_curves(
  model = plants_rf,
  variables = "climate_bio1_average"
)


plot_response_curves(
  model = plants_rf,
  variables = "climate_bio1_average",
  show.data = TRUE
)

```
