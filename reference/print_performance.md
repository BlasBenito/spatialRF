# print_performance

Prints the performance slot of a model fitted with
[`rf()`](https://blasbenito.github.io/spatialRF/reference/rf.md),
[`rf_repeat()`](https://blasbenito.github.io/spatialRF/reference/rf_repeat.md),
or
[`rf_spatial()`](https://blasbenito.github.io/spatialRF/reference/rf_spatial.md).
For models fitted with
[`rf_repeat()`](https://blasbenito.github.io/spatialRF/reference/rf_repeat.md)
it shows the median and the median absolute deviation of each
performance measure.

## Usage

``` r
print_performance(model)
```

## Arguments

- model:

  Model fitted with
  [`rf()`](https://blasbenito.github.io/spatialRF/reference/rf.md),
  [`rf_repeat()`](https://blasbenito.github.io/spatialRF/reference/rf_repeat.md),
  or
  [`rf_spatial()`](https://blasbenito.github.io/spatialRF/reference/rf_spatial.md).

## Value

Prints model performance scores to the console.

## See also

`print_performance()`,
[`get_performance()`](https://blasbenito.github.io/spatialRF/reference/get_performance.md)

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
[`print_evaluation()`](https://blasbenito.github.io/spatialRF/reference/print_evaluation.md),
[`print_importance()`](https://blasbenito.github.io/spatialRF/reference/print_importance.md),
[`print_moran()`](https://blasbenito.github.io/spatialRF/reference/print_moran.md)

## Examples

``` r
data(plants_rf)

print_performance(plants_rf)
#> 
#> Model performance 
#>   - R squared (oob):                  0.5015626
#>   - R squared (cor(obs, pred)^2):     0.8378728
#>   - Pseudo R squared (cor(obs, pred)):0.9153539
#>   - RMSE (oob):                       2379.254
#>   - RMSE:                             1609.614
#>   - Normalized RMSE:                  0.4646691
```
