# Double-center a distance matrix

Double-centers a distance matrix by converting it to weights and
centering to zero row and column means. Required for computing Moran's
Eigenvector Maps.

## Usage

``` r
double_center_distance_matrix(distance.matrix = NULL, distance.threshold = 0)
```

## Arguments

- distance.matrix:

  Numeric distance matrix. Default: `NULL`.

- distance.threshold:

  Numeric distance threshold for weight calculation. Distances above
  this threshold are set to 0 during weighting. Default: `0`.

## Value

Double-centered numeric matrix with the same dimensions as
`distance.matrix`. The matrix has row means and column means of zero.

## Details

Double-centering is performed in two steps:

1.  Convert distances to weights using
    [`weights_from_distance_matrix()`](https://blasbenito.github.io/spatialRF/reference/weights_from_distance_matrix.md)

2.  Center the matrix: subtract row means, subtract column means, and
    add the grand mean

The resulting matrix is symmetric with zero row and column means,
suitable for Moran's Eigenvector Maps computation.

## See also

[`weights_from_distance_matrix()`](https://blasbenito.github.io/spatialRF/reference/weights_from_distance_matrix.md),
[`mem()`](https://blasbenito.github.io/spatialRF/reference/mem.md),
[`mem_multithreshold()`](https://blasbenito.github.io/spatialRF/reference/mem_multithreshold.md)

Other preprocessing:
[`auto_cor()`](https://blasbenito.github.io/spatialRF/reference/auto_cor.md),
[`auto_vif()`](https://blasbenito.github.io/spatialRF/reference/auto_vif.md),
[`case_weights()`](https://blasbenito.github.io/spatialRF/reference/case_weights.md),
[`default_distance_thresholds()`](https://blasbenito.github.io/spatialRF/reference/default_distance_thresholds.md),
[`is_binary()`](https://blasbenito.github.io/spatialRF/reference/is_binary.md),
[`make_spatial_fold()`](https://blasbenito.github.io/spatialRF/reference/make_spatial_fold.md),
[`make_spatial_folds()`](https://blasbenito.github.io/spatialRF/reference/make_spatial_folds.md),
[`the_feature_engineer()`](https://blasbenito.github.io/spatialRF/reference/the_feature_engineer.md),
[`weights_from_distance_matrix()`](https://blasbenito.github.io/spatialRF/reference/weights_from_distance_matrix.md)

## Examples

``` r
data(plants_distance)

# Double-center the distance matrix
centered <- double_center_distance_matrix(
  distance.matrix = plants_distance
)

# Verify row means are zero
head(rowMeans(centered))
#>             1             2             3             4             5 
#> -1.184503e-19  1.150114e-18  1.299132e-19  9.361393e-19  1.085157e-18 
#>             6 
#>  2.445425e-19 

# Verify column means are zero
head(colMeans(centered))
#>            1            2            3            4            5            6 
#> 2.025118e-19 2.598264e-19 1.108083e-19 2.502740e-19 3.668138e-19 5.234738e-19 
```
