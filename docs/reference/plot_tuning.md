# Plots a tuning object produced by [`rf_tuning()`](https://blasbenito.github.io/spatialRF/reference/rf_tuning.md)

Plots the tuning of the hyperparameters `num.trees`, `mtry`, and
`min.node.size` performed by
[`rf_tuning()`](https://blasbenito.github.io/spatialRF/reference/rf_tuning.md).

## Usage

``` r
plot_tuning(
  model,
  point.color = viridis::viridis(
    100,
    option = "F"
  ),
  verbose = TRUE
)
```

## Arguments

- model:

  A model fitted with
  [`rf_tuning()`](https://blasbenito.github.io/spatialRF/reference/rf_tuning.md).
  Default: `NULL`

- point.color:

  Colors of the plotted points. Can be a single color name (e.g.
  "red4"), a character vector with hexadecimal codes (e.g. "#440154FF"
  "#21908CFF" "#FDE725FF"), or function generating a palette (e.g.
  `viridis::viridis(100)`). Default:
  `viridis::viridis(100, option = "F")`

- verbose:

  Logical, if `TRUE`, the plot is printed. Default: `TRUE`

## Value

A ggplot.

## See also

[`rf_tuning()`](https://blasbenito.github.io/spatialRF/reference/rf_tuning.md)

Other visualization:
[`plot_evaluation()`](https://blasbenito.github.io/spatialRF/reference/plot_evaluation.md),
[`plot_importance()`](https://blasbenito.github.io/spatialRF/reference/plot_importance.md),
[`plot_moran()`](https://blasbenito.github.io/spatialRF/reference/plot_moran.md),
[`plot_optimization()`](https://blasbenito.github.io/spatialRF/reference/plot_optimization.md),
[`plot_residuals_diagnostics()`](https://blasbenito.github.io/spatialRF/reference/plot_residuals_diagnostics.md),
[`plot_response_curves()`](https://blasbenito.github.io/spatialRF/reference/plot_response_curves.md),
[`plot_response_surface()`](https://blasbenito.github.io/spatialRF/reference/plot_response_surface.md),
[`plot_training_df()`](https://blasbenito.github.io/spatialRF/reference/plot_training_df.md),
[`plot_training_df_moran()`](https://blasbenito.github.io/spatialRF/reference/plot_training_df_moran.md)

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
