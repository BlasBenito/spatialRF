# Extract evaluation metrics from cross-validated model

Extracts aggregated performance metrics from a model evaluated with
[`rf_evaluate()`](https://blasbenito.github.io/spatialRF/reference/rf_evaluate.md).

## Usage

``` r
get_evaluation(model)
```

## Arguments

- model:

  Model object with class `rf_evaluate` from
  [`rf_evaluate()`](https://blasbenito.github.io/spatialRF/reference/rf_evaluate.md).

## Value

Data frame with aggregated evaluation metrics containing:

- `model`: Model type - "Full" (original model), "Training" (trained on
  training folds), or "Testing" (performance on testing folds,
  representing generalization ability).

- `metric`: Metric name - "rmse", "nrmse", "r.squared", or
  "pseudo.r.squared".

- `mean`, `sd`, `min`, `max`: Summary statistics across cross-validation
  repetitions.

## Details

This function returns aggregated statistics across all cross-validation
repetitions. The "Testing" model metrics indicate the model's ability to
generalize to unseen spatial locations.

## See also

[`rf_evaluate()`](https://blasbenito.github.io/spatialRF/reference/rf_evaluate.md),
[`plot_evaluation()`](https://blasbenito.github.io/spatialRF/reference/plot_evaluation.md),
[`print_evaluation()`](https://blasbenito.github.io/spatialRF/reference/print_evaluation.md)

Other model_info:
[`get_importance()`](https://blasbenito.github.io/spatialRF/reference/get_importance.md),
[`get_importance_local()`](https://blasbenito.github.io/spatialRF/reference/get_importance_local.md),
[`get_moran()`](https://blasbenito.github.io/spatialRF/reference/get_moran.md),
[`get_performance()`](https://blasbenito.github.io/spatialRF/reference/get_performance.md),
[`get_predictions()`](https://blasbenito.github.io/spatialRF/reference/get_predictions.md),
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
if(interactive()){

data(plants_rf, plants_xy)

# Evaluate model with spatial cross-validation
m_evaluated <- rf_evaluate(
  model = plants_rf,
  xy = plants_xy,
  repetitions = 5,
  n.cores = 1
)

# Extract evaluation metrics
eval_metrics <- get_evaluation(m_evaluated)
eval_metrics

}
```
