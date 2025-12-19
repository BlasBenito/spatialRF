# Extract model residuals

Extracts residuals (observed - predicted values) from models fitted with
[`rf()`](https://blasbenito.github.io/spatialRF/reference/rf.md),
[`rf_repeat()`](https://blasbenito.github.io/spatialRF/reference/rf_repeat.md),
or
[`rf_spatial()`](https://blasbenito.github.io/spatialRF/reference/rf_spatial.md).

## Usage

``` r
get_residuals(model)
```

## Arguments

- model:

  Model object from
  [`rf()`](https://blasbenito.github.io/spatialRF/reference/rf.md),
  [`rf_repeat()`](https://blasbenito.github.io/spatialRF/reference/rf_repeat.md),
  or
  [`rf_spatial()`](https://blasbenito.github.io/spatialRF/reference/rf_spatial.md).

## Value

Numeric vector of residuals with length equal to the number of training
observations. For
[`rf_repeat()`](https://blasbenito.github.io/spatialRF/reference/rf_repeat.md)
models, returns the median residual across repetitions.

## Details

Residuals are calculated as observed minus predicted values. They can be
used to assess model fit, check assumptions, and diagnose patterns such
as spatial autocorrelation (see
[`get_moran()`](https://blasbenito.github.io/spatialRF/reference/get_moran.md)).
Ideally, residuals should be randomly distributed with no systematic
patterns.

## See also

[`rf()`](https://blasbenito.github.io/spatialRF/reference/rf.md),
[`rf_repeat()`](https://blasbenito.github.io/spatialRF/reference/rf_repeat.md),
[`rf_spatial()`](https://blasbenito.github.io/spatialRF/reference/rf_spatial.md),
[`get_predictions()`](https://blasbenito.github.io/spatialRF/reference/get_predictions.md),
[`get_moran()`](https://blasbenito.github.io/spatialRF/reference/get_moran.md),
[`plot_residuals_diagnostics()`](https://blasbenito.github.io/spatialRF/reference/plot_residuals_diagnostics.md)

Other model_info:
[`get_evaluation()`](https://blasbenito.github.io/spatialRF/reference/get_evaluation.md),
[`get_importance()`](https://blasbenito.github.io/spatialRF/reference/get_importance.md),
[`get_importance_local()`](https://blasbenito.github.io/spatialRF/reference/get_importance_local.md),
[`get_moran()`](https://blasbenito.github.io/spatialRF/reference/get_moran.md),
[`get_performance()`](https://blasbenito.github.io/spatialRF/reference/get_performance.md),
[`get_predictions()`](https://blasbenito.github.io/spatialRF/reference/get_predictions.md),
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

# Extract residuals
residuals <- get_residuals(plants_rf)
head(residuals)
#> [1]   -58.03859 -1194.76562  -158.71363  1335.94372   716.88626  -212.86886

# Check basic statistics
summary(residuals)
#>     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#> -3408.93  -847.75  -302.35   -61.71   325.94 11040.86 

# Plot distribution to check for patterns
hist(residuals, main = "Residual Distribution", xlab = "Residuals")

```
