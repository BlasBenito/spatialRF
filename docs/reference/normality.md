# Normality test of a numeric vector

Applies a Shapiro-Wilks test to a numeric vector, and returns a list
with the statistic W, its p-value, and a character string with the
interpretation.

## Usage

``` r
residuals_test(residuals)
```

## Arguments

- residuals:

  Numeric vector, model residuals.

## Value

A list with four slots:

- /item `w` W statistic returned by
  [`shapiro.test()`](https://rdrr.io/r/stats/shapiro.test.html). /item
  `p.value` p-value of the Shapiro test. /item `interpretation`
  Character vector, one of "x is normal", "x is not normal". /item
  `plot` A patchwork plot with the qq plot and the histogram of x.

## See also

Other spatial_analysis:
[`filter_spatial_predictors()`](https://blasbenito.github.io/spatialRF/reference/filter_spatial_predictors.md),
[`mem()`](https://blasbenito.github.io/spatialRF/reference/mem.md),
[`mem_multithreshold()`](https://blasbenito.github.io/spatialRF/reference/mem_multithreshold.md),
[`moran()`](https://blasbenito.github.io/spatialRF/reference/moran.md),
[`moran_multithreshold()`](https://blasbenito.github.io/spatialRF/reference/moran_multithreshold.md),
[`pca()`](https://blasbenito.github.io/spatialRF/reference/pca.md),
[`pca_multithreshold()`](https://blasbenito.github.io/spatialRF/reference/pca_multithreshold.md),
[`rank_spatial_predictors()`](https://blasbenito.github.io/spatialRF/reference/rank_spatial_predictors.md),
[`residuals_diagnostics()`](https://blasbenito.github.io/spatialRF/reference/residuals_diagnostics.md),
[`select_spatial_predictors_recursive()`](https://blasbenito.github.io/spatialRF/reference/select_spatial_predictors_recursive.md),
[`select_spatial_predictors_sequential()`](https://blasbenito.github.io/spatialRF/reference/select_spatial_predictors_sequential.md)

## Examples

``` r
residuals_test(residuals = runif(100))
#> $shapiro.w
#> [1] 0.9606738
#> 
#> $p.value
#> [1] 0.004492854
#> 
#> $interpretation
#> [1] "Residuals are not normal"
#> 
```
