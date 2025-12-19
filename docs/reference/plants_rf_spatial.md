# Example fitted spatial random forest model

Fitted spatial random forest model using
[plants_df](https://blasbenito.github.io/spatialRF/reference/plants_df.md)
with spatial predictors from Moran's Eigenvector Maps. Provided for
testing and examples without requiring model fitting. Fitted with
reduced complexity for faster computation and smaller object size.

## Usage

``` r
data(plants_rf_spatial)
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

- `method`: "mem.effect.recursive"

- `num.trees`: 50

- `min.node.size`: 30

- `n.cores`: 14

## Details

This spatial model includes spatial predictors (Moran's Eigenvector
Maps) selected using the recursive method to minimize residual spatial
autocorrelation. Uses reduced complexity (50 trees, min.node.size = 30)
to keep object size small for package distribution. For actual analyses,
use higher values (e.g., num.trees = 500, min.node.size = 5).

## See also

[`rf_spatial()`](https://blasbenito.github.io/spatialRF/reference/rf_spatial.md),
[`rf()`](https://blasbenito.github.io/spatialRF/reference/rf.md),
[plants_rf](https://blasbenito.github.io/spatialRF/reference/plants_rf.md),
[plants_df](https://blasbenito.github.io/spatialRF/reference/plants_df.md),
[plants_response](https://blasbenito.github.io/spatialRF/reference/plants_response.md),
[plants_predictors](https://blasbenito.github.io/spatialRF/reference/plants_predictors.md)

Other data:
[`plants_df`](https://blasbenito.github.io/spatialRF/reference/plants_df.md),
[`plants_distance`](https://blasbenito.github.io/spatialRF/reference/plants_distance.md),
[`plants_predictors`](https://blasbenito.github.io/spatialRF/reference/plants_predictors.md),
[`plants_response`](https://blasbenito.github.io/spatialRF/reference/plants_response.md),
[`plants_rf`](https://blasbenito.github.io/spatialRF/reference/plants_rf.md),
[`plants_xy`](https://blasbenito.github.io/spatialRF/reference/plants_xy.md)
