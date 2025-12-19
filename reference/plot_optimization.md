# Optimization plot of a selection of spatial predictors

Plots optimization data frames produced by
[`select_spatial_predictors_sequential()`](https://blasbenito.github.io/spatialRF/reference/select_spatial_predictors_sequential.md)
and
[`select_spatial_predictors_recursive()`](https://blasbenito.github.io/spatialRF/reference/select_spatial_predictors_recursive.md).

## Usage

``` r
plot_optimization(
  model,
  point.color = viridis::viridis(
    100,
    option = "F",
    direction = -1
  ),
  verbose = TRUE
)
```

## Arguments

- model:

  A model produced by
  [`rf_spatial()`](https://blasbenito.github.io/spatialRF/reference/rf_spatial.md),
  or an optimization data frame produced by
  [`select_spatial_predictors_sequential()`](https://blasbenito.github.io/spatialRF/reference/select_spatial_predictors_sequential.md)
  or
  [`select_spatial_predictors_recursive()`](https://blasbenito.github.io/spatialRF/reference/select_spatial_predictors_recursive.md).

- point.color:

  Colors of the plotted points. Can be a single color name (e.g.
  "red4"), a character vector with hexadecimal codes (e.g. "#440154FF"
  "#21908CFF" "#FDE725FF"), or function generating a palette (e.g.
  `viridis::viridis(100)`). Default:
  `viridis::viridis(100, option = "F", direction = -1)`

- verbose:

  Logical, if `TRUE` the plot is printed. Default: `TRUE`

## Value

A ggplot, or `NULL` invisibly if no optimization data is available.

## Details

The function returns `NULL` invisibly (without plotting) when:

- The method used to fit a model with
  [`rf_spatial()`](https://blasbenito.github.io/spatialRF/reference/rf_spatial.md)
  is "hengl" (no optimization required)

- No spatial predictors were selected during model fitting

- The model is non-spatial

## See also

Other visualization:
[`plot_evaluation()`](https://blasbenito.github.io/spatialRF/reference/plot_evaluation.md),
[`plot_importance()`](https://blasbenito.github.io/spatialRF/reference/plot_importance.md),
[`plot_moran()`](https://blasbenito.github.io/spatialRF/reference/plot_moran.md),
[`plot_residuals_diagnostics()`](https://blasbenito.github.io/spatialRF/reference/plot_residuals_diagnostics.md),
[`plot_response_curves()`](https://blasbenito.github.io/spatialRF/reference/plot_response_curves.md),
[`plot_response_surface()`](https://blasbenito.github.io/spatialRF/reference/plot_response_surface.md),
[`plot_training_df()`](https://blasbenito.github.io/spatialRF/reference/plot_training_df.md),
[`plot_training_df_moran()`](https://blasbenito.github.io/spatialRF/reference/plot_training_df_moran.md),
[`plot_tuning()`](https://blasbenito.github.io/spatialRF/reference/plot_tuning.md)

## Examples

``` r
data(plants_rf_spatial)

plot_optimization(plants_rf_spatial)

```
