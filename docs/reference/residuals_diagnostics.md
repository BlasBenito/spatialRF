# Normality test of a numeric vector

Applies a Shapiro-Wilks test to a numeric vector, and plots the qq plot
and the histogram.

## Usage

``` r
residuals_diagnostics(residuals, predictions)
```

## Arguments

- residuals:

  Numeric vector, model residuals.

- predictions:

  Numeric vector, model predictions.

## Value

A list with four slots:

- /item `w` W statistic returned by
  [`shapiro.test()`](https://rdrr.io/r/stats/shapiro.test.html). /item
  `p.value` p-value of the Shapiro test. /item `interpretation`
  Character vector, one of "x is normal", "x is not normal". /item
  `plot` A patchwork plot with the qq plot and the histogram of x.

## Details

The function
[`shapiro.test()`](https://rdrr.io/r/stats/shapiro.test.html) has a hard
limit of 5000 cases. If the model residuals have more than 5000 cases,
then `sample(x = residuals, size = 5000)` is applied to the model
residuals before the test.

## See also

[`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html),[`aes`](https://ggplot2.tidyverse.org/reference/aes.html),[`geom_qq_line`](https://ggplot2.tidyverse.org/reference/geom_qq.html),`ggtheme`,[`labs`](https://ggplot2.tidyverse.org/reference/labs.html),[`geom_freqpoly`](https://ggplot2.tidyverse.org/reference/geom_histogram.html),[`geom_abline`](https://ggplot2.tidyverse.org/reference/geom_abline.html)
[`plot_annotation`](https://patchwork.data-imaginist.com/reference/plot_annotation.html)

Other spatial_analysis:
[`filter_spatial_predictors()`](https://blasbenito.github.io/spatialRF/reference/filter_spatial_predictors.md),
[`mem()`](https://blasbenito.github.io/spatialRF/reference/mem.md),
[`mem_multithreshold()`](https://blasbenito.github.io/spatialRF/reference/mem_multithreshold.md),
[`moran()`](https://blasbenito.github.io/spatialRF/reference/moran.md),
[`moran_multithreshold()`](https://blasbenito.github.io/spatialRF/reference/moran_multithreshold.md),
[`pca()`](https://blasbenito.github.io/spatialRF/reference/pca.md),
[`pca_multithreshold()`](https://blasbenito.github.io/spatialRF/reference/pca_multithreshold.md),
[`rank_spatial_predictors()`](https://blasbenito.github.io/spatialRF/reference/rank_spatial_predictors.md),
[`residuals_test()`](https://blasbenito.github.io/spatialRF/reference/normality.md),
[`select_spatial_predictors_recursive()`](https://blasbenito.github.io/spatialRF/reference/select_spatial_predictors_recursive.md),
[`select_spatial_predictors_sequential()`](https://blasbenito.github.io/spatialRF/reference/select_spatial_predictors_sequential.md)

## Examples

``` r
data(plants_rf)

y <- residuals_diagnostics(
  residuals = get_residuals(plants_rf),
  predictions = get_predictions(plants_rf)
)
y
#> $shapiro.w
#> [1] 0.7976006
#> 
#> $p.value
#> [1] 1.87131e-16
#> 
#> $interpretation
#> [1] "Residuals are not normal"
#> 
#> $plot

#> 
```
