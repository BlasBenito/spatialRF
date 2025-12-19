# Rescales a numeric vector into a new range

Rescales a numeric vector to a new range.

## Usage

``` r
rescale_vector(
  x = NULL,
  new.min = 0,
  new.max = 1,
  integer = FALSE
)
```

## Arguments

- x:

  Numeric vector. Default: `NULL`

- new.min:

  New minimum value. Default: `0`

- new.max:

  New maximum value. Default: `1`

- integer:

  Logical, if `TRUE`, coerces the output to integer. Default: `FALSE`

## Value

A numeric vector of the same length as x, but with its values rescaled
between `new.min` and `new.max.`

## See also

Other utilities:
[`.vif_to_df()`](https://blasbenito.github.io/spatialRF/reference/dot-vif_to_df.md),
[`auc()`](https://blasbenito.github.io/spatialRF/reference/auc.md),
[`beowulf_cluster()`](https://blasbenito.github.io/spatialRF/reference/beowulf_cluster.md),
[`objects_size()`](https://blasbenito.github.io/spatialRF/reference/objects_size.md),
[`optimization_function()`](https://blasbenito.github.io/spatialRF/reference/optimization_function.md),
[`prepare_importance_spatial()`](https://blasbenito.github.io/spatialRF/reference/prepare_importance_spatial.md),
[`root_mean_squared_error()`](https://blasbenito.github.io/spatialRF/reference/root_mean_squared_error.md),
[`setup_parallel_execution()`](https://blasbenito.github.io/spatialRF/reference/setup_parallel_execution.md),
[`standard_error()`](https://blasbenito.github.io/spatialRF/reference/standard_error.md),
[`statistical_mode()`](https://blasbenito.github.io/spatialRF/reference/statistical_mode.md),
[`thinning()`](https://blasbenito.github.io/spatialRF/reference/thinning.md),
[`thinning_til_n()`](https://blasbenito.github.io/spatialRF/reference/thinning_til_n.md)

## Examples

``` r
y <- rescale_vector(
  x = rnorm(100),
  new.min = 0,
  new.max = 100,
  integer = TRUE
)
y
#>   [1]  61  67  20  75  72  43  45  50  63  42  10  54  58  39  66  24  42  28
#>  [19]  33  67  47  73  66  60  58  35  41  37  67  33  46  22  48  56  33  44
#>  [37]  33  22  15  32  46  85  21  71  87  32  28  60  50  24  61  53  58  25
#>  [55]  62  27  41  41  83  66  33  41  42  35  36  12  66  42  37  50   7  31
#>  [73]  12  38  84  68  70  96  65  53  70  20  48  27  37   0  69  39  68  43
#>  [91]  45 100  12  30  28  42  34  28  28  78
```
