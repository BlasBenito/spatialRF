# Multicollinearity reduction via Pearson correlation

Filters predictors using sequential evaluation of pairwise correlations.
Predictors are ranked by user preference (or column order) and evaluated
sequentially. Each candidate is added to the selected pool only if its
maximum absolute correlation with already-selected predictors does not
exceed the threshold.

## Usage

``` r
auto_cor(
  x = NULL,
  preference.order = NULL,
  cor.threshold = 0.5,
  verbose = TRUE
)
```

## Arguments

- x:

  Data frame with predictors, or a `variable_selection` object from
  [`auto_vif()`](https://blasbenito.github.io/spatialRF/reference/auto_vif.md).
  Default: `NULL`.

- preference.order:

  Character vector specifying variable preference order. Does not need
  to include all variables in `x`. If `NULL`, column order is used.
  Default: `NULL`.

- cor.threshold:

  Numeric between 0 and 1 (recommended: 0.5 to 0.9). Maximum allowed
  absolute Pearson correlation between selected variables. Default:
  `0.50`

- verbose:

  Logical. If `TRUE`, prints messages about operations and removed
  variables. Default: `TRUE`

## Value

List with class `variable_selection` containing:

- `cor`: Correlation matrix of selected variables (only if 2+ variables
  selected).

- `selected.variables`: Character vector of selected variable names.

- `selected.variables.df`: Data frame containing selected variables.

## Details

The algorithm follows these steps:

1.  Rank predictors by `preference.order` (or use column order if NULL).

2.  Initialize selection pool with first predictor.

3.  For each remaining candidate:

    - Compute maximum absolute correlation with selected predictors.

    - If max correlation equal or lower than `cor.threshold`, add to
      selected pool.

    - Otherwise, skip candidate.

4.  Return selected predictors.

**Data cleaning**: Variables in `preference.order` not found in
`colnames(x)` are silently removed. Non-numeric columns are removed with
a warning. Rows with NA values are removed via
[`na.omit()`](https://rdrr.io/r/stats/na.fail.html). Zero-variance
columns trigger a warning but are not removed.

This function can be chained with
[`auto_vif()`](https://blasbenito.github.io/spatialRF/reference/auto_vif.md)
through pipes (see examples).

## See also

[`auto_vif()`](https://blasbenito.github.io/spatialRF/reference/auto_vif.md)

Other preprocessing:
[`auto_vif()`](https://blasbenito.github.io/spatialRF/reference/auto_vif.md),
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

y <- auto_cor(
  x = plants_df[, plants_predictors]
)
#> [auto_cor()]: Removed variables: human_population, human_footprint_average

y$selected.variables
#>  [1] "bias_area_km2"                   "bias_species_per_record"        
#>  [3] "climate_aridity_index_average"   "climate_hypervolume"            
#>  [5] "climate_velocity_lgm_average"    "neighbors_count"                
#>  [7] "neighbors_percent_shared_edge"   "human_population_density"       
#>  [9] "topography_elevation_average"    "landcover_herbs_percent_average"
#> [11] "fragmentation_cohesion"          "fragmentation_division"         
#> [13] "neighbors_area"                  "climate_bio1_average"           
#> [15] "climate_bio15_minimum"          
y$cor
#>                                 bias_area_km2 bias_species_per_record
#> bias_area_km2                            1.00                   -0.21
#> bias_species_per_record                 -0.21                    1.00
#> climate_aridity_index_average            0.05                   -0.19
#> climate_hypervolume                      0.01                   -0.03
#> climate_velocity_lgm_average             0.25                   -0.30
#> neighbors_count                          0.26                   -0.05
#> neighbors_percent_shared_edge            0.05                   -0.04
#> human_population_density                -0.26                   -0.04
#> topography_elevation_average            -0.09                   -0.07
#> landcover_herbs_percent_average         -0.08                   -0.11
#> fragmentation_cohesion                   0.28                   -0.23
#> fragmentation_division                  -0.25                    0.00
#> neighbors_area                           0.16                   -0.07
#> climate_bio1_average                    -0.21                    0.37
#> climate_bio15_minimum                   -0.19                    0.08
#>                                 climate_aridity_index_average
#> bias_area_km2                                            0.05
#> bias_species_per_record                                 -0.19
#> climate_aridity_index_average                            1.00
#> climate_hypervolume                                     -0.16
#> climate_velocity_lgm_average                             0.10
#> neighbors_count                                         -0.16
#> neighbors_percent_shared_edge                            0.29
#> human_population_density                                 0.10
#> topography_elevation_average                             0.28
#> landcover_herbs_percent_average                          0.42
#> fragmentation_cohesion                                   0.11
#> fragmentation_division                                   0.01
#> neighbors_area                                          -0.22
#> climate_bio1_average                                    -0.12
#> climate_bio15_minimum                                    0.32
#>                                 climate_hypervolume
#> bias_area_km2                                  0.01
#> bias_species_per_record                       -0.03
#> climate_aridity_index_average                 -0.16
#> climate_hypervolume                            1.00
#> climate_velocity_lgm_average                  -0.22
#> neighbors_count                                0.13
#> neighbors_percent_shared_edge                 -0.06
#> human_population_density                      -0.05
#> topography_elevation_average                   0.36
#> landcover_herbs_percent_average               -0.19
#> fragmentation_cohesion                        -0.03
#> fragmentation_division                         0.18
#> neighbors_area                                 0.25
#> climate_bio1_average                          -0.09
#> climate_bio15_minimum                         -0.08
#>                                 climate_velocity_lgm_average neighbors_count
#> bias_area_km2                                           0.25            0.26
#> bias_species_per_record                                -0.30           -0.05
#> climate_aridity_index_average                           0.10           -0.16
#> climate_hypervolume                                    -0.22            0.13
#> climate_velocity_lgm_average                            1.00           -0.11
#> neighbors_count                                        -0.11            1.00
#> neighbors_percent_shared_edge                          -0.03            0.16
#> human_population_density                               -0.07           -0.08
#> topography_elevation_average                           -0.27            0.09
#> landcover_herbs_percent_average                         0.19           -0.14
#> fragmentation_cohesion                                  0.17           -0.04
#> fragmentation_division                                 -0.13            0.00
#> neighbors_area                                         -0.07            0.08
#> climate_bio1_average                                   -0.27            0.20
#> climate_bio15_minimum                                  -0.28           -0.08
#>                                 neighbors_percent_shared_edge
#> bias_area_km2                                            0.05
#> bias_species_per_record                                 -0.04
#> climate_aridity_index_average                            0.29
#> climate_hypervolume                                     -0.06
#> climate_velocity_lgm_average                            -0.03
#> neighbors_count                                          0.16
#> neighbors_percent_shared_edge                            1.00
#> human_population_density                                -0.16
#> topography_elevation_average                             0.22
#> landcover_herbs_percent_average                          0.07
#> fragmentation_cohesion                                   0.26
#> fragmentation_division                                  -0.19
#> neighbors_area                                          -0.01
#> climate_bio1_average                                     0.19
#> climate_bio15_minimum                                    0.12
#>                                 human_population_density
#> bias_area_km2                                      -0.26
#> bias_species_per_record                            -0.04
#> climate_aridity_index_average                       0.10
#> climate_hypervolume                                -0.05
#> climate_velocity_lgm_average                       -0.07
#> neighbors_count                                    -0.08
#> neighbors_percent_shared_edge                      -0.16
#> human_population_density                            1.00
#> topography_elevation_average                       -0.04
#> landcover_herbs_percent_average                     0.26
#> fragmentation_cohesion                             -0.29
#> fragmentation_division                              0.14
#> neighbors_area                                     -0.21
#> climate_bio1_average                                0.26
#> climate_bio15_minimum                               0.12
#>                                 topography_elevation_average
#> bias_area_km2                                          -0.09
#> bias_species_per_record                                -0.07
#> climate_aridity_index_average                           0.28
#> climate_hypervolume                                     0.36
#> climate_velocity_lgm_average                           -0.27
#> neighbors_count                                         0.09
#> neighbors_percent_shared_edge                           0.22
#> human_population_density                               -0.04
#> topography_elevation_average                            1.00
#> landcover_herbs_percent_average                        -0.10
#> fragmentation_cohesion                                 -0.02
#> fragmentation_division                                  0.17
#> neighbors_area                                          0.02
#> climate_bio1_average                                   -0.27
#> climate_bio15_minimum                                   0.07
#>                                 landcover_herbs_percent_average
#> bias_area_km2                                             -0.08
#> bias_species_per_record                                   -0.11
#> climate_aridity_index_average                              0.42
#> climate_hypervolume                                       -0.19
#> climate_velocity_lgm_average                               0.19
#> neighbors_count                                           -0.14
#> neighbors_percent_shared_edge                              0.07
#> human_population_density                                   0.26
#> topography_elevation_average                              -0.10
#> landcover_herbs_percent_average                            1.00
#> fragmentation_cohesion                                     0.01
#> fragmentation_division                                     0.00
#> neighbors_area                                            -0.14
#> climate_bio1_average                                       0.02
#> climate_bio15_minimum                                      0.17
#>                                 fragmentation_cohesion fragmentation_division
#> bias_area_km2                                     0.28                  -0.25
#> bias_species_per_record                          -0.23                   0.00
#> climate_aridity_index_average                     0.11                   0.01
#> climate_hypervolume                              -0.03                   0.18
#> climate_velocity_lgm_average                      0.17                  -0.13
#> neighbors_count                                  -0.04                   0.00
#> neighbors_percent_shared_edge                     0.26                  -0.19
#> human_population_density                         -0.29                   0.14
#> topography_elevation_average                     -0.02                   0.17
#> landcover_herbs_percent_average                   0.01                   0.00
#> fragmentation_cohesion                            1.00                  -0.32
#> fragmentation_division                           -0.32                   1.00
#> neighbors_area                                    0.09                   0.06
#> climate_bio1_average                             -0.17                  -0.15
#> climate_bio15_minimum                            -0.02                   0.08
#>                                 neighbors_area climate_bio1_average
#> bias_area_km2                             0.16                -0.21
#> bias_species_per_record                  -0.07                 0.37
#> climate_aridity_index_average            -0.22                -0.12
#> climate_hypervolume                       0.25                -0.09
#> climate_velocity_lgm_average             -0.07                -0.27
#> neighbors_count                           0.08                 0.20
#> neighbors_percent_shared_edge            -0.01                 0.19
#> human_population_density                 -0.21                 0.26
#> topography_elevation_average              0.02                -0.27
#> landcover_herbs_percent_average          -0.14                 0.02
#> fragmentation_cohesion                    0.09                -0.17
#> fragmentation_division                    0.06                -0.15
#> neighbors_area                            1.00                -0.32
#> climate_bio1_average                     -0.32                 1.00
#> climate_bio15_minimum                    -0.19                 0.20
#>                                 climate_bio15_minimum
#> bias_area_km2                                   -0.19
#> bias_species_per_record                          0.08
#> climate_aridity_index_average                    0.32
#> climate_hypervolume                             -0.08
#> climate_velocity_lgm_average                    -0.28
#> neighbors_count                                 -0.08
#> neighbors_percent_shared_edge                    0.12
#> human_population_density                         0.12
#> topography_elevation_average                     0.07
#> landcover_herbs_percent_average                  0.17
#> fragmentation_cohesion                          -0.02
#> fragmentation_division                           0.08
#> neighbors_area                                  -0.19
#> climate_bio1_average                             0.20
#> climate_bio15_minimum                            1.00
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
#>   fragmentation_cohesion fragmentation_division neighbors_area
#> 1               99.81169              0.9195281      2005112.5
#> 2               92.40274              0.9999953       720242.4
#> 3               99.68820              0.9690934     15661626.9
#> 4               99.34487              0.9649031       284154.0
#> 5               99.89918              0.8698819      4646036.6
#> 6               99.73477              0.9294914      2801093.1
#>   climate_bio1_average climate_bio15_minimum
#> 1            251.05816                    13
#> 2            261.08870                    25
#> 3             11.58566                    17
#> 4            173.21428                    16
#> 5            251.82974                    31
#> 6            255.79948                    28
```
