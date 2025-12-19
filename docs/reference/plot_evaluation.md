# Visualize spatial cross-validation results

Creates boxplots comparing model performance metrics across training,
testing, and full datasets from spatial cross-validation performed by
[`rf_evaluate()`](https://blasbenito.github.io/spatialRF/reference/rf_evaluate.md).
Displays distributions of R-squared, RMSE, and other metrics across all
spatial folds.

## Usage

``` r
plot_evaluation(
  model,
  fill.color = viridis::viridis(3, option = "F", alpha = 0.8, direction = -1),
  line.color = "gray30",
  verbose = TRUE,
  notch = TRUE
)
```

## Arguments

- model:

  Model fitted with
  [`rf_evaluate()`](https://blasbenito.github.io/spatialRF/reference/rf_evaluate.md).
  Must be of class "rf_evaluate".

- fill.color:

  Character vector with three colors (one for each model type: Testing,
  Training, Full) or a function that generates a color palette. Accepts
  hexadecimal codes (e.g., `c("#440154FF", "#21908CFF", "#FDE725FF")`)
  or palette functions (e.g., `viridis::viridis(3)`). Default:
  `viridis::viridis(3, option = "F", alpha = 0.8, direction = -1)`.

- line.color:

  Character string specifying the color of boxplot borders. Default:
  `"gray30"`.

- verbose:

  Logical. If `TRUE`, prints the plot to the graphics device. Default:
  `TRUE`.

- notch:

  Logical. If `TRUE`, displays notched boxplots where notches represent
  approximate 95% confidence intervals around the median.
  Non-overlapping notches suggest significant differences between
  medians. Default: `TRUE`.

## Value

ggplot object that can be further customized or saved. The plot displays
boxplots of performance metrics (R-squared, RMSE, NRMSE, pseudo
R-squared, or AUC depending on model type) across spatial folds, faceted
by metric.

## Details

This function visualizes the distribution of performance metrics across
spatial folds, with separate boxplots for three model variants:

- **Testing**: Performance on spatially independent testing folds (most
  reliable estimate of generalization)

- **Training**: Performance on training folds (typically optimistic)

- **Full**: Performance on the complete dataset (reference baseline)

**Interpreting the plot:**

The boxplots show the distribution of each metric across all spatial
folds. Ideally:

- Testing performance should be reasonably close to training performance
  (indicates good generalization)

- Large gaps between training and testing suggest overfitting

- Low variance across folds indicates stable, consistent model
  performance

- High variance suggests performance depends strongly on spatial
  location

The plot includes a title showing the number of spatial folds used in
the evaluation.

**Available metrics:**

Displayed metrics depend on the response variable type:

- **Continuous response**: R-squared, RMSE (Root Mean Squared Error),
  NRMSE (Normalized RMSE)

- **Binary response**: AUC (Area Under ROC Curve), pseudo R-squared

## See also

[`rf_evaluate()`](https://blasbenito.github.io/spatialRF/reference/rf_evaluate.md),
[`get_evaluation()`](https://blasbenito.github.io/spatialRF/reference/get_evaluation.md),
[`print_evaluation()`](https://blasbenito.github.io/spatialRF/reference/print_evaluation.md)

Other visualization:
[`plot_importance()`](https://blasbenito.github.io/spatialRF/reference/plot_importance.md),
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

data(plants_rf, plants_xy)

# Perform spatial cross-validation
plants_rf <- rf_evaluate(
  model = plants_rf,
  xy = plants_xy,
  repetitions = 5,
  n.cores = 1
)

# Visualize evaluation results
plot_evaluation(plants_rf)

# Without notches for simpler boxplots
plot_evaluation(plants_rf, notch = FALSE)

# Custom colors
plot_evaluation(
  plants_rf,
  fill.color = c("#E64B35FF", "#4DBBD5FF", "#00A087FF")
)

# Print summary statistics
print_evaluation(plants_rf)

# Extract evaluation data for custom analysis
evaluation_data <- get_evaluation(plants_rf)
head(evaluation_data)

}
```
