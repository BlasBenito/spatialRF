# Prints cross-validation results

Prints the results of an spatial cross-validation performed with
[`rf_evaluate()`](https://blasbenito.github.io/spatialRF/reference/rf_evaluate.md).

## Usage

``` r
print_evaluation(model)
```

## Arguments

- model:

  A model resulting from
  [`rf_evaluate()`](https://blasbenito.github.io/spatialRF/reference/rf_evaluate.md).

## Value

A table printed to the standard output.

## See also

[`plot_evaluation()`](https://blasbenito.github.io/spatialRF/reference/plot_evaluation.md),
[`get_evaluation()`](https://blasbenito.github.io/spatialRF/reference/get_evaluation.md)

Other model_info:
[`get_evaluation()`](https://blasbenito.github.io/spatialRF/reference/get_evaluation.md),
[`get_importance()`](https://blasbenito.github.io/spatialRF/reference/get_importance.md),
[`get_importance_local()`](https://blasbenito.github.io/spatialRF/reference/get_importance_local.md),
[`get_moran()`](https://blasbenito.github.io/spatialRF/reference/get_moran.md),
[`get_performance()`](https://blasbenito.github.io/spatialRF/reference/get_performance.md),
[`get_predictions()`](https://blasbenito.github.io/spatialRF/reference/get_predictions.md),
[`get_residuals()`](https://blasbenito.github.io/spatialRF/reference/get_residuals.md),
[`get_response_curves()`](https://blasbenito.github.io/spatialRF/reference/get_response_curves.md),
[`get_spatial_predictors()`](https://blasbenito.github.io/spatialRF/reference/get_spatial_predictors.md),
[`print.rf()`](https://blasbenito.github.io/spatialRF/reference/print.md),
[`print_importance()`](https://blasbenito.github.io/spatialRF/reference/print_importance.md),
[`print_moran()`](https://blasbenito.github.io/spatialRF/reference/print_moran.md),
[`print_performance()`](https://blasbenito.github.io/spatialRF/reference/print_performance.md)

## Examples

``` r
if(interactive()){

data(
  plants_rf,
  plants_xy
)

plants_rf <- rf_evaluate(
  model = plants_rf,
  xy = plants_xy,
  repetitions = 5,
  n.cores = 1
)

print_evaluation(plants_rf)

}
```
