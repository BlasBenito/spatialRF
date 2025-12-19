# Example fitted random forest model

Fitted random forest model using
[plants_df](https://blasbenito.github.io/spatialRF/reference/plants_df.md).
Provided for testing and examples without requiring model fitting.
Fitted with reduced complexity for faster computation and smaller object
size.

## Usage

``` r
data(plants_rf)
```

## Format

An object of class `rf` fitted with the following parameters:

- `data`:
  [plants_df](https://blasbenito.github.io/spatialRF/reference/plants_df.md)

- `dependent.variable.name`:
  [plants_response](https://blasbenito.github.io/spatialRF/reference/plants_response.md)
  ("richness_species_vascular")

- `predictor.variable.names`:
  [plants_predictors](https://blasbenito.github.io/spatialRF/reference/plants_predictors.md)
  (17 variables)

- `distance.matrix`:
  [plants_distance](https://blasbenito.github.io/spatialRF/reference/plants_distance.md)

- `xy`:
  [plants_xy](https://blasbenito.github.io/spatialRF/reference/plants_xy.md)

- `distance.thresholds`: `c(100, 1000, 2000, 4000)`

- `num.trees`: 50

- `min.node.size`: 30

- `n.cores`: 1

## Details

This model uses reduced complexity (50 trees, min.node.size = 30) to
keep object size small for package distribution. For actual analyses,
use higher values (e.g., num.trees = 500, min.node.size = 5).

## See also

[`rf()`](https://blasbenito.github.io/spatialRF/reference/rf.md),
[plants_df](https://blasbenito.github.io/spatialRF/reference/plants_df.md),
[plants_response](https://blasbenito.github.io/spatialRF/reference/plants_response.md),
[plants_predictors](https://blasbenito.github.io/spatialRF/reference/plants_predictors.md)

Other data:
[`plants_df`](https://blasbenito.github.io/spatialRF/reference/plants_df.md),
[`plants_distance`](https://blasbenito.github.io/spatialRF/reference/plants_distance.md),
[`plants_predictors`](https://blasbenito.github.io/spatialRF/reference/plants_predictors.md),
[`plants_response`](https://blasbenito.github.io/spatialRF/reference/plants_response.md),
[`plants_rf_spatial`](https://blasbenito.github.io/spatialRF/reference/plants_rf_spatial.md),
[`plants_xy`](https://blasbenito.github.io/spatialRF/reference/plants_xy.md)
