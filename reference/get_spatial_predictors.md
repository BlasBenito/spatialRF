# Extract spatial predictors from spatial model

Extracts the spatial predictors (Moran's Eigenvector Maps) used in a
model fitted with
[`rf_spatial()`](https://blasbenito.github.io/spatialRF/reference/rf_spatial.md).

## Usage

``` r
get_spatial_predictors(model)
```

## Arguments

- model:

  Model object from
  [`rf_spatial()`](https://blasbenito.github.io/spatialRF/reference/rf_spatial.md)
  (must have class `rf_spatial`).

## Value

Data frame containing the spatial predictor values for each observation,
with predictors ordered by decreasing importance.

## Details

Spatial predictors are Moran's Eigenvector Maps (MEMs) automatically
generated and selected by
[`rf_spatial()`](https://blasbenito.github.io/spatialRF/reference/rf_spatial.md)
to capture spatial autocorrelation patterns in the data. This function
extracts these predictors, which can be useful for understanding spatial
structure or for making predictions on new spatial locations.

## See also

[`rf_spatial()`](https://blasbenito.github.io/spatialRF/reference/rf_spatial.md),
[`mem()`](https://blasbenito.github.io/spatialRF/reference/mem.md),
[`mem_multithreshold()`](https://blasbenito.github.io/spatialRF/reference/mem_multithreshold.md),
[`get_importance()`](https://blasbenito.github.io/spatialRF/reference/get_importance.md)

Other model_info:
[`get_evaluation()`](https://blasbenito.github.io/spatialRF/reference/get_evaluation.md),
[`get_importance()`](https://blasbenito.github.io/spatialRF/reference/get_importance.md),
[`get_importance_local()`](https://blasbenito.github.io/spatialRF/reference/get_importance_local.md),
[`get_moran()`](https://blasbenito.github.io/spatialRF/reference/get_moran.md),
[`get_performance()`](https://blasbenito.github.io/spatialRF/reference/get_performance.md),
[`get_predictions()`](https://blasbenito.github.io/spatialRF/reference/get_predictions.md),
[`get_residuals()`](https://blasbenito.github.io/spatialRF/reference/get_residuals.md),
[`get_response_curves()`](https://blasbenito.github.io/spatialRF/reference/get_response_curves.md),
[`print.rf()`](https://blasbenito.github.io/spatialRF/reference/print.md),
[`print_evaluation()`](https://blasbenito.github.io/spatialRF/reference/print_evaluation.md),
[`print_importance()`](https://blasbenito.github.io/spatialRF/reference/print_importance.md),
[`print_moran()`](https://blasbenito.github.io/spatialRF/reference/print_moran.md),
[`print_performance()`](https://blasbenito.github.io/spatialRF/reference/print_performance.md)

## Examples

``` r
data(plants_rf_spatial)

# Extract spatial predictors
spatial_preds <- get_spatial_predictors(plants_rf_spatial)
head(spatial_preds)
#>   spatial_predictor_100_2 spatial_predictor_100_14 spatial_predictor_100_11
#> 1            -0.003914286              0.008308019               0.14698873
#> 2            -0.038636431              0.015520890              -0.02946903
#> 3             0.029156255             -0.017986416               0.00471761
#> 4            -0.004807265             -0.029052199              -0.01524803
#> 5            -0.001751023              0.086958559               0.17600097
#> 6            -0.001383862              0.025988150               0.14927785
#>   spatial_predictor_1000_66 spatial_predictor_1000_34 spatial_predictor_2000_13
#> 1               0.026757354               -0.01452556              -0.002455888
#> 2              -0.031106869               -0.08013209              -0.075605529
#> 3              -0.082326152               -0.02198527               0.004448900
#> 4              -0.083501222               -0.05692688              -0.028564751
#> 5               0.003938521                0.02887879               0.028114077
#> 6              -0.038232356                0.05676607               0.017592450
#>   spatial_predictor_100_16 spatial_predictor_1000_64 spatial_predictor_100_5
#> 1               0.16702874              -0.009341237             0.037762397
#> 2               0.04396603              -0.032701838            -0.285607771
#> 3              -0.01153076              -0.047211284             0.001661393
#> 4              -0.08005054              -0.032450026            -0.005621210
#> 5               0.13447905              -0.017794081             0.074424222
#> 6               0.07136303              -0.032329644             0.039083455
#>   spatial_predictor_1000_33
#> 1                0.03137691
#> 2               -0.05337929
#> 3               -0.11382103
#> 4                0.06537506
#> 5               -0.02509947
#> 6                0.02115162

# Check dimensions
dim(spatial_preds)
#> [1] 227  10

# View predictor names (ordered by importance)
colnames(spatial_preds)
#>  [1] "spatial_predictor_100_2"   "spatial_predictor_100_14" 
#>  [3] "spatial_predictor_100_11"  "spatial_predictor_1000_66"
#>  [5] "spatial_predictor_1000_34" "spatial_predictor_2000_13"
#>  [7] "spatial_predictor_100_16"  "spatial_predictor_1000_64"
#>  [9] "spatial_predictor_100_5"   "spatial_predictor_1000_33"
```
