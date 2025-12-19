# Prints results of a Moran's I test

Prints the results of a Moran's I test on the residuals of a model.

## Usage

``` r
print_moran(
  model,
  caption = NULL,
  verbose = TRUE
)
```

## Arguments

- model:

  A model fitted with
  [`rf()`](https://blasbenito.github.io/spatialRF/reference/rf.md),
  [`rf_repeat()`](https://blasbenito.github.io/spatialRF/reference/rf_repeat.md),
  or
  [`rf_spatial()`](https://blasbenito.github.io/spatialRF/reference/rf_spatial.md).

- caption:

  Character, caption of the output table, Default: `NULL`

- verbose:

  Logical, if `TRUE`, the resulting table is printed into the console,
  Default: `TRUE`

## Value

Prints a table in the console using the
[huxtable](https://hughjonesd.github.io/huxtable/reference/huxtable.html)
package.

## See also

[`moran()`](https://blasbenito.github.io/spatialRF/reference/moran.md),
[`moran_multithreshold()`](https://blasbenito.github.io/spatialRF/reference/moran_multithreshold.md),
[`get_moran()`](https://blasbenito.github.io/spatialRF/reference/get_moran.md),
[`plot_moran()`](https://blasbenito.github.io/spatialRF/reference/plot_moran.md)

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
[`print_performance()`](https://blasbenito.github.io/spatialRF/reference/print_performance.md)

## Examples

``` r
data(plants_rf)

print_moran(plants_rf)
#>              ┌──────────┬───────────┬─────────┬──────────────────┐
#>              │ Distance │ Moran's I │ P value │ Interpretation   │
#>              ├──────────┼───────────┼─────────┼──────────────────┤
#>              │    100.0 │     0.164 │   0.000 │ Positive spatial │
#>              │          │           │         │ correlation      │
#>              ├──────────┼───────────┼─────────┼──────────────────┤
#>              │   1000.0 │     0.073 │   0.000 │ Positive spatial │
#>              │          │           │         │ correlation      │
#>              ├──────────┼───────────┼─────────┼──────────────────┤
#>              │   2000.0 │     0.026 │   0.002 │ Positive spatial │
#>              │          │           │         │ correlation      │
#>              ├──────────┼───────────┼─────────┼──────────────────┤
#>              │   4000.0 │     0.008 │   0.032 │ Positive spatial │
#>              │          │           │         │ correlation      │
#>              └──────────┴───────────┴─────────┴──────────────────┘
```
