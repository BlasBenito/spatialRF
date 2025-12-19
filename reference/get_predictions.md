# Extract fitted predictions from model

Extracts fitted (in-sample) predictions from models fitted with
[`rf()`](https://blasbenito.github.io/spatialRF/reference/rf.md),
[`rf_repeat()`](https://blasbenito.github.io/spatialRF/reference/rf_repeat.md),
or
[`rf_spatial()`](https://blasbenito.github.io/spatialRF/reference/rf_spatial.md).

## Usage

``` r
get_predictions(model)
```

## Arguments

- model:

  Model object from
  [`rf()`](https://blasbenito.github.io/spatialRF/reference/rf.md),
  [`rf_repeat()`](https://blasbenito.github.io/spatialRF/reference/rf_repeat.md),
  or
  [`rf_spatial()`](https://blasbenito.github.io/spatialRF/reference/rf_spatial.md).

## Value

Numeric vector of fitted predictions with length equal to the number of
training observations. For
[`rf_repeat()`](https://blasbenito.github.io/spatialRF/reference/rf_repeat.md)
models, returns the median prediction across repetitions.

## Details

This function returns fitted predictions for the training data used to
build the model, not predictions for new data. For out-of-sample
predictions on new data use
[`stats::predict()`](https://rdrr.io/r/stats/predict.html).

## See also

[`rf()`](https://blasbenito.github.io/spatialRF/reference/rf.md),
[`rf_repeat()`](https://blasbenito.github.io/spatialRF/reference/rf_repeat.md),
[`rf_spatial()`](https://blasbenito.github.io/spatialRF/reference/rf_spatial.md),
[`get_residuals()`](https://blasbenito.github.io/spatialRF/reference/get_residuals.md)

Other model_info:
[`get_evaluation()`](https://blasbenito.github.io/spatialRF/reference/get_evaluation.md),
[`get_importance()`](https://blasbenito.github.io/spatialRF/reference/get_importance.md),
[`get_importance_local()`](https://blasbenito.github.io/spatialRF/reference/get_importance_local.md),
[`get_moran()`](https://blasbenito.github.io/spatialRF/reference/get_moran.md),
[`get_performance()`](https://blasbenito.github.io/spatialRF/reference/get_performance.md),
[`get_residuals()`](https://blasbenito.github.io/spatialRF/reference/get_residuals.md),
[`get_response_curves()`](https://blasbenito.github.io/spatialRF/reference/get_response_curves.md),
[`get_spatial_predictors()`](https://blasbenito.github.io/spatialRF/reference/get_spatial_predictors.md),
[`print.rf()`](https://blasbenito.github.io/spatialRF/reference/print.md),
[`print_evaluation()`](https://blasbenito.github.io/spatialRF/reference/print_evaluation.md),
[`print_importance()`](https://blasbenito.github.io/spatialRF/reference/print_importance.md),
[`print_moran()`](https://blasbenito.github.io/spatialRF/reference/print_moran.md),
[`print_performance()`](https://blasbenito.github.io/spatialRF/reference/print_performance.md)

## Examples

``` r
data(plants_rf)

# Extract fitted predictions
predictions <- get_predictions(plants_rf)
head(predictions)
#> [1] 4893.039 5554.766 1520.714 6482.056 9677.114 2902.869

# Check length matches number of observations
length(predictions)
#> [1] 227

# Compare with observed values to assess fit
# (observed values would be in original data)
```
