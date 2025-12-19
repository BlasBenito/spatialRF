# Multicollinearity reduction via Variance Inflation Factor

Filters predictors using sequential evaluation of variance inflation
factors. Predictors are ranked by user preference (or column order) and
evaluated sequentially. Each candidate is added to the selected pool
only if the maximum VIF of all predictors (candidate plus
already-selected) does not exceed the threshold.

## Usage

``` r
auto_vif(x = NULL, preference.order = NULL, vif.threshold = 5, verbose = TRUE)
```

## Arguments

- x:

  Data frame with predictors, or a `variable_selection` object from
  [`auto_cor()`](https://blasbenito.github.io/spatialRF/reference/auto_cor.md).
  Default: `NULL`.

- preference.order:

  Character vector specifying variable preference order. Does not need
  to include all variables in `x`. If `NULL`, column order is used.
  Default: `NULL`.

- vif.threshold:

  Numeric (recommended: 2.5 to 10). Maximum allowed VIF among selected
  variables. Higher values allow more collinearity. Default: `5`.

- verbose:

  Logical. If `TRUE`, prints messages about operations and removed
  variables. Default: `TRUE`

## Value

List with class `variable_selection` containing:

- `vif`: Data frame with selected variable names and their VIF scores.

- `selected.variables`: Character vector of selected variable names.

- `selected.variables.df`: Data frame containing selected variables.

## Details

The algorithm follows these steps:

1.  Rank predictors by `preference.order` (or use column order if NULL).

2.  Initialize selection pool with first predictor.

3.  For each remaining candidate:

    - Compute VIF for candidate plus all selected predictors.

    - If max VIF equal or lower than `vif.threshold`, add candidate to
      selected pool.

    - Otherwise, skip candidate.

4.  Return selected predictors with their VIF values.

**Data cleaning**: Variables in `preference.order` not found in
`colnames(x)` are silently removed. Non-numeric columns are removed with
a warning. Rows with NA values are removed via
[`na.omit()`](https://rdrr.io/r/stats/na.fail.html). Zero-variance
columns trigger a warning but are not removed.

This function can be chained with
[`auto_cor()`](https://blasbenito.github.io/spatialRF/reference/auto_cor.md)
through pipes (see examples).

## See also

[`auto_cor()`](https://blasbenito.github.io/spatialRF/reference/auto_cor.md)

Other preprocessing:
[`auto_cor()`](https://blasbenito.github.io/spatialRF/reference/auto_cor.md),
[`case_weights()`](https://blasbenito.github.io/spatialRF/reference/case_weights.md),
[`default_distance_thresholds()`](https://blasbenito.github.io/spatialRF/reference/default_distance_thresholds.md),
[`double_center_distance_matrix()`](https://blasbenito.github.io/spatialRF/reference/double_center_distance_matrix.md),
[`is_binary()`](https://blasbenito.github.io/spatialRF/reference/is_binary.md),
[`make_spatial_fold()`](https://blasbenito.github.io/spatialRF/reference/make_spatial_fold.md),
[`make_spatial_folds()`](https://blasbenito.github.io/spatialRF/reference/make_spatial_folds.md),
[`the_feature_engineer()`](https://blasbenito.github.io/spatialRF/reference/the_feature_engineer.md),
[`weights_from_distance_matrix()`](https://blasbenito.github.io/spatialRF/reference/weights_from_distance_matrix.md)

## Examples

``` r
data(
  plants_df,
  plants_predictors
)

y <- auto_vif(
  x = plants_df[, plants_predictors]
)
#> [auto_vif()]: Variables are not collinear.

y$selected.variables
#>  [1] "bias_area_km2"                   "bias_species_per_record"        
#>  [3] "climate_aridity_index_average"   "climate_hypervolume"            
#>  [5] "climate_velocity_lgm_average"    "neighbors_count"                
#>  [7] "neighbors_percent_shared_edge"   "human_population_density"       
#>  [9] "topography_elevation_average"    "landcover_herbs_percent_average"
#> [11] "fragmentation_cohesion"          "fragmentation_division"         
#> [13] "neighbors_area"                  "human_population"               
#> [15] "human_footprint_average"         "climate_bio1_average"           
#> [17] "climate_bio15_minimum"          
y$vif
#>                           variable      vif
#> 1          human_footprint_average 4.701199
#> 2             climate_bio1_average 2.996871
#> 3         human_population_density 2.879088
#> 4                 human_population 2.709401
#> 5     topography_elevation_average 1.975092
#> 6    climate_aridity_index_average 1.938247
#> 7                    bias_area_km2 1.847408
#> 8  landcover_herbs_percent_average 1.742363
#> 9     climate_velocity_lgm_average 1.742296
#> 10   neighbors_percent_shared_edge 1.680098
#> 11          fragmentation_cohesion 1.537666
#> 12         bias_species_per_record 1.508060
#> 13           climate_bio15_minimum 1.483121
#> 14                 neighbors_count 1.408907
#> 15                  neighbors_area 1.391563
#> 16             climate_hypervolume 1.390049
#> 17          fragmentation_division 1.334247
head(y$selected.variables.df)
#>   bias_area_km2 bias_species_per_record climate_aridity_index_average
#> 1     268444.41               0.2699995                   -1.27163039
#> 2      26658.41               0.1456615                    0.01279434
#> 3     140062.77               0.1218554                    0.05070049
#> 4      31915.27               0.1625342                   -0.57904797
#> 5     716681.87               0.2475400                   -0.51033378
#> 6     173261.40               0.4204510                   -0.75537704
#>   climate_hypervolume climate_velocity_lgm_average neighbors_count
#> 1          0.02239650                    7.6806232              10
#> 2          0.52194842                    7.9545641              13
#> 3          0.03088209                    6.4202598               8
#> 4          0.68598777                    0.5746547               4
#> 5          0.05199463                    6.5561073              10
#> 6          0.00147194                    7.6114518               8
#>   neighbors_percent_shared_edge human_population_density
#> 1                     100.00000                7.0675297
#> 2                      56.46095               42.5908861
#> 3                     100.00000                0.8711942
#> 4                      35.48473              218.8202463
#> 5                     100.00000                3.0460006
#> 6                     100.00000                0.7522835
#>   topography_elevation_average landcover_herbs_percent_average
#> 1                    86.807776                        16.22373
#> 2                     6.921187                        53.90207
#> 3                  1130.220919                        42.66630
#> 4                  1872.903223                        54.36617
#> 5                   166.793692                        28.21572
#> 6                    86.900413                        18.40789
#>   fragmentation_cohesion fragmentation_division neighbors_area human_population
#> 1               99.81169              0.9195281      2005112.5        1897238.8
#> 2               92.40274              0.9999953       720242.4        1135405.4
#> 3               99.68820              0.9690934     15661626.9         122021.9
#> 4               99.34487              0.9649031       284154.0        6983707.7
#> 5               99.89918              0.8698819      4646036.6        2183013.4
#> 6               99.73477              0.9294914      2801093.1         130341.7
#>   human_footprint_average climate_bio1_average climate_bio15_minimum
#> 1               0.4305056            251.05816                    13
#> 2               8.3018495            261.08870                    25
#> 3               0.5687886             11.58566                    17
#> 4              11.9806927            173.21428                    16
#> 5               1.9740117            251.82974                    31
#> 6               0.6538992            255.79948                    28
```
