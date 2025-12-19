# Extract Moran's I test results for model residuals

Extracts Moran's I test results for spatial autocorrelation in model
residuals from models fitted with
[`rf()`](https://blasbenito.github.io/spatialRF/reference/rf.md),
[`rf_repeat()`](https://blasbenito.github.io/spatialRF/reference/rf_repeat.md),
or
[`rf_spatial()`](https://blasbenito.github.io/spatialRF/reference/rf_spatial.md).

## Usage

``` r
get_moran(model)
```

## Arguments

- model:

  Model object from
  [`rf()`](https://blasbenito.github.io/spatialRF/reference/rf.md),
  [`rf_repeat()`](https://blasbenito.github.io/spatialRF/reference/rf_repeat.md),
  or
  [`rf_spatial()`](https://blasbenito.github.io/spatialRF/reference/rf_spatial.md).

## Value

Data frame with Moran's I statistics at multiple distance thresholds.
Columns include `distance.threshold`, `moran.i` (statistic), `p.value`,
`interpretation`, and `method`.

## Details

Moran's I tests for spatial autocorrelation in model residuals.
Significant positive values indicate residuals are spatially clustered,
suggesting the model hasn't fully captured spatial patterns. For spatial
models
([`rf_spatial()`](https://blasbenito.github.io/spatialRF/reference/rf_spatial.md)),
low or non-significant Moran's I values indicate successful removal of
spatial autocorrelation.

## See also

[`moran()`](https://blasbenito.github.io/spatialRF/reference/moran.md),
[`moran_multithreshold()`](https://blasbenito.github.io/spatialRF/reference/moran_multithreshold.md),
[`plot_moran()`](https://blasbenito.github.io/spatialRF/reference/plot_moran.md),
[`print_moran()`](https://blasbenito.github.io/spatialRF/reference/print_moran.md)

Other model_info:
[`get_evaluation()`](https://blasbenito.github.io/spatialRF/reference/get_evaluation.md),
[`get_importance()`](https://blasbenito.github.io/spatialRF/reference/get_importance.md),
[`get_importance_local()`](https://blasbenito.github.io/spatialRF/reference/get_importance_local.md),
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
data(plants_rf)

# Extract Moran's I test results
moran_results <- get_moran(plants_rf)
moran_results
#>   distance.threshold     moran.i moran.i.null      p.value
#> 1                100 0.163544964 -0.004424779 6.854407e-08
#> 2               1000 0.072542848 -0.004424779 5.285324e-07
#> 3               2000 0.026479300 -0.004424779 2.115511e-03
#> 4               4000 0.007600359 -0.004424779 3.166076e-02
#>                 interpretation
#> 1 Positive spatial correlation
#> 2 Positive spatial correlation
#> 3 Positive spatial correlation
#> 4 Positive spatial correlation

# Check for significant spatial autocorrelation
significant <- moran_results[moran_results$p.value < 0.05, ]
significant
#>   distance.threshold     moran.i moran.i.null      p.value
#> 1                100 0.163544964 -0.004424779 6.854407e-08
#> 2               1000 0.072542848 -0.004424779 5.285324e-07
#> 3               2000 0.026479300 -0.004424779 2.115511e-03
#> 4               4000 0.007600359 -0.004424779 3.166076e-02
#>                 interpretation
#> 1 Positive spatial correlation
#> 2 Positive spatial correlation
#> 3 Positive spatial correlation
#> 4 Positive spatial correlation
```
