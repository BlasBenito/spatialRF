# Visualize variable importance scores

Creates a visualization of variable importance scores from models fitted
with [`rf()`](https://blasbenito.github.io/spatialRF/reference/rf.md),
[`rf_repeat()`](https://blasbenito.github.io/spatialRF/reference/rf_repeat.md),
or
[`rf_spatial()`](https://blasbenito.github.io/spatialRF/reference/rf_spatial.md).
For single-run models
([`rf()`](https://blasbenito.github.io/spatialRF/reference/rf.md),
[`rf_spatial()`](https://blasbenito.github.io/spatialRF/reference/rf_spatial.md)),
displays points ordered by importance. For repeated models
([`rf_repeat()`](https://blasbenito.github.io/spatialRF/reference/rf_repeat.md)),
displays violin plots showing the distribution of importance scores
across model repetitions.

## Usage

``` r
plot_importance(
  model,
  fill.color = viridis::viridis(100, option = "F", direction = -1, alpha = 1, end = 0.9),
  line.color = "white",
  verbose = TRUE
)
```

## Arguments

- model:

  Model fitted with
  [`rf()`](https://blasbenito.github.io/spatialRF/reference/rf.md),
  [`rf_repeat()`](https://blasbenito.github.io/spatialRF/reference/rf_repeat.md),
  or
  [`rf_spatial()`](https://blasbenito.github.io/spatialRF/reference/rf_spatial.md).
  Alternatively, a data frame with variable importance scores (for
  internal use only).

- fill.color:

  Character vector of colors or a function generating a color palette.
  Accepts hexadecimal codes (e.g.,
  `c("#440154FF", "#21908CFF", "#FDE725FF")`) or palette functions
  (e.g., `viridis::viridis(100)`). For single-run models, creates a
  continuous gradient. For repeated models, assigns discrete colors to
  variables. Default:
  `viridis::viridis(100, option = "F", direction = -1, alpha = 1, end = 0.9)`.

- line.color:

  Character string specifying the color of point borders (single-run
  models) or violin plot outlines (repeated models). Default: `"white"`.

- verbose:

  Logical. If `TRUE`, prints the plot to the graphics device. Default:
  `TRUE`.

## Value

ggplot object that can be further customized or saved. The plot displays
variable importance on the x-axis and variable names on the y-axis,
ordered by importance (highest at top).

## Details

This function creates different visualizations depending on the model
type:

**Single-run models**
([`rf()`](https://blasbenito.github.io/spatialRF/reference/rf.md),
[`rf_spatial()`](https://blasbenito.github.io/spatialRF/reference/rf_spatial.md)
without repetitions):

- Displays points showing the importance value for each variable

- Variables ordered top-to-bottom by importance (most important at top)

- Point color represents importance magnitude using a continuous
  gradient

**Repeated models**
([`rf_repeat()`](https://blasbenito.github.io/spatialRF/reference/rf_repeat.md),
[`rf_spatial()`](https://blasbenito.github.io/spatialRF/reference/rf_spatial.md)
with repetitions):

- Displays violin plots showing the distribution of importance across
  repetitions

- Variables ordered top-to-bottom by median importance (most important
  at top)

- The median line within each violin shows the center of the
  distribution

- Width of violin reflects the density of importance values at each
  level

- Each variable receives a distinct fill color

**Importance metric:**

The x-axis shows permutation importance, which measures the increase in
prediction error when a variable's values are randomly shuffled. Higher
values indicate more important variables. Importance is computed on
out-of-bag (OOB) samples, providing an unbiased estimate of variable
contribution.

**Spatial predictors:**

In
[`rf_spatial()`](https://blasbenito.github.io/spatialRF/reference/rf_spatial.md)
models, all spatial predictors (MEMs or PCA factors) are grouped into a
single category labeled "spatial_predictors" to simplify comparison with
non-spatial predictors.

**Note on violin plots:**

Violin plots display kernel density estimates. The median line shown is
the median of the density estimate, which may differ slightly from the
actual data median. However, variables are always ordered by the true
median importance to ensure accurate ranking.

**Cross-validated importance:**

This function does not plot results from
[`rf_importance()`](https://blasbenito.github.io/spatialRF/reference/rf_importance.md).
For cross-validated importance plots, access
`model$importance$cv.per.variable.plot` after running
[`rf_importance()`](https://blasbenito.github.io/spatialRF/reference/rf_importance.md).

## See also

[`print_importance()`](https://blasbenito.github.io/spatialRF/reference/print_importance.md),
[`get_importance()`](https://blasbenito.github.io/spatialRF/reference/get_importance.md),
[`rf_importance()`](https://blasbenito.github.io/spatialRF/reference/rf_importance.md)

Other visualization:
[`plot_evaluation()`](https://blasbenito.github.io/spatialRF/reference/plot_evaluation.md),
[`plot_moran()`](https://blasbenito.github.io/spatialRF/reference/plot_moran.md),
[`plot_optimization()`](https://blasbenito.github.io/spatialRF/reference/plot_optimization.md),
[`plot_residuals_diagnostics()`](https://blasbenito.github.io/spatialRF/reference/plot_residuals_diagnostics.md),
[`plot_response_curves()`](https://blasbenito.github.io/spatialRF/reference/plot_response_curves.md),
[`plot_response_surface()`](https://blasbenito.github.io/spatialRF/reference/plot_response_surface.md),
[`plot_training_df()`](https://blasbenito.github.io/spatialRF/reference/plot_training_df.md),
[`plot_training_df_moran()`](https://blasbenito.github.io/spatialRF/reference/plot_training_df_moran.md),
[`plot_tuning()`](https://blasbenito.github.io/spatialRF/reference/plot_tuning.md)

## Examples

``` r
if(interactive()){

data(plants_rf, plants_rf_spatial)

# Plot importance from Random Forest model
plot_importance(plants_rf)

# Plot importance from Spatial Random Forest model
plot_importance(plants_rf_spatial)

}

```
