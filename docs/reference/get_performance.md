# Extract out-of-bag performance metrics from model

Extracts out-of-bag (OOB) performance metrics from models fitted with
[`rf()`](https://blasbenito.github.io/spatialRF/reference/rf.md),
[`rf_repeat()`](https://blasbenito.github.io/spatialRF/reference/rf_repeat.md),
or
[`rf_spatial()`](https://blasbenito.github.io/spatialRF/reference/rf_spatial.md).

## Usage

``` r
get_performance(model)
```

## Arguments

- model:

  Model object from
  [`rf()`](https://blasbenito.github.io/spatialRF/reference/rf.md),
  [`rf_repeat()`](https://blasbenito.github.io/spatialRF/reference/rf_repeat.md),
  or
  [`rf_spatial()`](https://blasbenito.github.io/spatialRF/reference/rf_spatial.md).

## Value

Data frame with performance metrics:

- For [`rf()`](https://blasbenito.github.io/spatialRF/reference/rf.md)
  and
  [`rf_spatial()`](https://blasbenito.github.io/spatialRF/reference/rf_spatial.md):
  columns `metric` and `value`

- For
  [`rf_repeat()`](https://blasbenito.github.io/spatialRF/reference/rf_repeat.md):
  columns `metric`, `median`, and `median_absolute_deviation` (MAD
  across repetitions)

## Details

Out-of-bag (OOB) performance is computed using observations not included
in bootstrap samples during model training. Metrics typically include
R-squared, pseudo R-squared, RMSE, and normalized RMSE. For repeated
models, the median and median absolute deviation summarize performance
across repetitions.

## See also

[`rf()`](https://blasbenito.github.io/spatialRF/reference/rf.md),
[`rf_repeat()`](https://blasbenito.github.io/spatialRF/reference/rf_repeat.md),
[`rf_spatial()`](https://blasbenito.github.io/spatialRF/reference/rf_spatial.md),
[`print_performance()`](https://blasbenito.github.io/spatialRF/reference/print_performance.md)

Other model_info:
[`get_evaluation()`](https://blasbenito.github.io/spatialRF/reference/get_evaluation.md),
[`get_importance()`](https://blasbenito.github.io/spatialRF/reference/get_importance.md),
[`get_importance_local()`](https://blasbenito.github.io/spatialRF/reference/get_importance_local.md),
[`get_moran()`](https://blasbenito.github.io/spatialRF/reference/get_moran.md),
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
data(plants_rf)

# Extract OOB performance metrics
performance <- get_performance(plants_rf)
performance
#>             metric        value
#> 1    r.squared.oob    0.5015626
#> 2        r.squared    0.8378728
#> 3 pseudo.r.squared    0.9153539
#> 4         rmse.oob 2379.2537244
#> 5             rmse 1609.6138000
#> 6            nrmse    0.4646691

# For repeated models, median and MAD are returned
# (example would require rf_repeat model)
```
