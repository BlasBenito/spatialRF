# Display sizes of objects in current R environment

Returns a summary of objects in the current R workspace, sorted from
largest to smallest by memory size. Useful for identifying
memory-intensive objects and diagnosing memory issues.

## Usage

``` r
objects_size(n = 10)
```

## Arguments

- n:

  Integer specifying the number of largest objects to display. Default:
  `10`.

## Value

Data frame with object names as row names and four columns:

- `Type`: Object class (e.g., "data.frame", "matrix", "list").

- `Size`: Memory size with automatic unit formatting (e.g., "1.2 Mb",
  "500 bytes").

- `Length/Rows`: Number of elements (for vectors) or rows (for data
  frames/matrices).

- `Columns`: Number of columns (for data frames/matrices; `NA` for
  vectors and other objects).

## Details

This utility function helps monitor memory usage by displaying the
largest objects in your workspace. It's particularly useful for:

- Identifying memory bottlenecks during large spatial analyses

- Deciding which objects to remove to free memory

- Understanding the memory footprint of different data structures

The function examines all objects in the global environment
(`.GlobalEnv`) and calculates their memory usage using
[`utils::object.size()`](https://rdrr.io/r/utils/object.size.html).
Objects are automatically sorted by size in descending order.

## See also

[`utils::object.size()`](https://rdrr.io/r/utils/object.size.html),
[`base::ls()`](https://rdrr.io/r/base/ls.html),
[`base::rm()`](https://rdrr.io/r/base/rm.html)

Other utilities:
[`.vif_to_df()`](https://blasbenito.github.io/spatialRF/reference/dot-vif_to_df.md),
[`auc()`](https://blasbenito.github.io/spatialRF/reference/auc.md),
[`beowulf_cluster()`](https://blasbenito.github.io/spatialRF/reference/beowulf_cluster.md),
[`optimization_function()`](https://blasbenito.github.io/spatialRF/reference/optimization_function.md),
[`prepare_importance_spatial()`](https://blasbenito.github.io/spatialRF/reference/prepare_importance_spatial.md),
[`rescale_vector()`](https://blasbenito.github.io/spatialRF/reference/rescale_vector.md),
[`root_mean_squared_error()`](https://blasbenito.github.io/spatialRF/reference/root_mean_squared_error.md),
[`setup_parallel_execution()`](https://blasbenito.github.io/spatialRF/reference/setup_parallel_execution.md),
[`standard_error()`](https://blasbenito.github.io/spatialRF/reference/standard_error.md),
[`statistical_mode()`](https://blasbenito.github.io/spatialRF/reference/statistical_mode.md),
[`thinning()`](https://blasbenito.github.io/spatialRF/reference/thinning.md),
[`thinning_til_n()`](https://blasbenito.github.io/spatialRF/reference/thinning_til_n.md)

## Examples

``` r
# Create some objects of different sizes
small_vector <- runif(100)
medium_matrix <- matrix(runif(10000), 100, 100)
large_matrix <- matrix(runif(100000), 1000, 100)

# View the 5 largest objects
objects_size(n = 5)
#>                       Type      Size Length/Rows Columns
#> maxicheck         function 784 bytes          NA      NA
#> load              function 616 bytes          NA      NA
#> plants_xy       data.frame    5.4 Kb         227       3
#> plants_distance     matrix  431.7 Kb         227     227
#> plants_df       data.frame   38.9 Kb         227      21

# Check all objects (up to 10 by default)
objects_size()
#>                         Type      Size Length/Rows Columns
#> maxicheck           function 784 bytes          NA      NA
#> load                function 616 bytes          NA      NA
#> plants_xy         data.frame    5.4 Kb         227       3
#> plants_distance       matrix  431.7 Kb         227     227
#> plants_df         data.frame   38.9 Kb         227      21
#> plants_rf_spatial         rf  176.6 Mb          23      NA
#> plants_rf                 rf  144.7 Mb          22      NA
#> plants_response    character 136 bytes           1      NA
#> plants_predictors  character    1.5 Kb          17      NA
#> minicheck           function    1.3 Kb          NA      NA
```
