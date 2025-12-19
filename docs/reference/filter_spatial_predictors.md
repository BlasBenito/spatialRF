# Remove redundant spatial predictors

Removes spatial predictors that are highly correlated with other spatial
predictors or with non-spatial predictors. Particularly useful when
using multiple distance thresholds that produce correlated spatial
predictors.

## Usage

``` r
filter_spatial_predictors(
  data = NULL,
  predictor.variable.names = NULL,
  spatial.predictors.df = NULL,
  cor.threshold = 0.5
)
```

## Arguments

- data:

  Data frame containing the predictor variables. Default: `NULL`.

- predictor.variable.names:

  Character vector of non-spatial predictor names. Must match column
  names in `data`. Can also be a `variable_selection` object. Default:
  `NULL`.

- spatial.predictors.df:

  Data frame of spatial predictors (e.g., from
  [`mem_multithreshold()`](https://blasbenito.github.io/spatialRF/reference/mem_multithreshold.md)).
  Default: `NULL`.

- cor.threshold:

  Numeric between 0 and 1 (recommended: 0.5 to 0.75). Maximum allowed
  absolute Pearson correlation. Default: `0.50`.

## Value

Data frame containing only spatial predictors with correlations below
`cor.threshold` (both among themselves and with non-spatial predictors).

## Details

Filtering is performed in two steps:

1.  Remove spatial predictors correlated with each other (using
    [`auto_cor()`](https://blasbenito.github.io/spatialRF/reference/auto_cor.md))

2.  Remove spatial predictors correlated with non-spatial predictors

This two-step process ensures the retained spatial predictors are
independent of both each other and the environmental predictors,
improving model interpretability and reducing multicollinearity.

## See also

Other spatial_analysis:
[`mem()`](https://blasbenito.github.io/spatialRF/reference/mem.md),
[`mem_multithreshold()`](https://blasbenito.github.io/spatialRF/reference/mem_multithreshold.md),
[`moran()`](https://blasbenito.github.io/spatialRF/reference/moran.md),
[`moran_multithreshold()`](https://blasbenito.github.io/spatialRF/reference/moran_multithreshold.md),
[`pca()`](https://blasbenito.github.io/spatialRF/reference/pca.md),
[`pca_multithreshold()`](https://blasbenito.github.io/spatialRF/reference/pca_multithreshold.md),
[`rank_spatial_predictors()`](https://blasbenito.github.io/spatialRF/reference/rank_spatial_predictors.md),
[`residuals_diagnostics()`](https://blasbenito.github.io/spatialRF/reference/residuals_diagnostics.md),
[`residuals_test()`](https://blasbenito.github.io/spatialRF/reference/normality.md),
[`select_spatial_predictors_recursive()`](https://blasbenito.github.io/spatialRF/reference/select_spatial_predictors_recursive.md),
[`select_spatial_predictors_sequential()`](https://blasbenito.github.io/spatialRF/reference/select_spatial_predictors_sequential.md)

## Examples

``` r
data(
  plants_df,
  plants_predictors,
  plants_distance
)

# Generate spatial predictors using multiple distance thresholds
mem.df <- mem_multithreshold(
  distance.matrix = plants_distance,
  distance.thresholds = c(0, 1000)
)

# Filter spatial predictors to remove redundancy
# Removes spatial predictors correlated > 0.50 with each other
# or with environmental predictors
spatial.predictors.filtered <- filter_spatial_predictors(
  data = plants_df,
  predictor.variable.names = plants_predictors,
  spatial.predictors.df = mem.df,
  cor.threshold = 0.50
)

# Check dimensions
ncol(mem.df)  # original number
#> [1] 160
ncol(spatial.predictors.filtered)  # after filtering
#> [1] 154
```
