# Extract response curve data for plotting

Extracts data for plotting partial dependence (response) curves showing
how predictions vary with each predictor from models fitted with
[`rf()`](https://blasbenito.github.io/spatialRF/reference/rf.md),
[`rf_repeat()`](https://blasbenito.github.io/spatialRF/reference/rf_repeat.md),
or
[`rf_spatial()`](https://blasbenito.github.io/spatialRF/reference/rf_spatial.md).

## Usage

``` r
get_response_curves(
  model = NULL,
  variables = NULL,
  quantiles = c(0.1, 0.5, 0.9),
  grid.resolution = 200,
  verbose = TRUE
)
```

## Arguments

- model:

  Model object from
  [`rf()`](https://blasbenito.github.io/spatialRF/reference/rf.md),
  [`rf_repeat()`](https://blasbenito.github.io/spatialRF/reference/rf_repeat.md),
  or
  [`rf_spatial()`](https://blasbenito.github.io/spatialRF/reference/rf_spatial.md).

- variables:

  Character vector of predictor names to plot. If `NULL`, automatically
  selects the top 50% most important variables. Default: `NULL`.

- quantiles:

  Numeric vector of quantiles (0 to 1) at which to fix non-plotted
  predictors. Multiple quantiles show response variation under different
  scenarios. Default: `c(0.1, 0.5, 0.9)`.

- grid.resolution:

  Integer (20 to 500) specifying the number of points along the
  predictor axis. Higher values produce smoother curves. Default: `200`.

- verbose:

  Logical. If `TRUE`, prints progress messages. Default: `TRUE`.

## Value

Data frame with the following columns:

- `response`: Predicted response values.

- `predictor`: Predictor values along the gradient.

- `quantile`: Factor indicating which quantile was used to fix other
  predictors.

- `model`: Model index (only for
  [`rf_repeat()`](https://blasbenito.github.io/spatialRF/reference/rf_repeat.md)
  models with multiple repetitions).

- `predictor.name`: Character name of the focal predictor.

- `response.name`: Character name of the response variable.

## Details

Response curves (also called partial dependence plots) show how
predicted values change as a focal predictor varies while holding other
predictors constant at specified quantile values. This reveals the
marginal effect of each predictor.

The function generates curves by:

1.  Creating a grid of values for the focal predictor

2.  Fixing non-plotted predictors at each quantile (e.g., 0.1, 0.5, 0.9)

3.  Predicting responses across the grid

4.  Repeating for each selected predictor and quantile combination

Multiple quantiles reveal whether the effect of a predictor is
consistent across different environmental contexts (parallel curves) or
varies depending on other conditions (non-parallel curves).

## See also

[`rf()`](https://blasbenito.github.io/spatialRF/reference/rf.md),
[`rf_repeat()`](https://blasbenito.github.io/spatialRF/reference/rf_repeat.md),
[`rf_spatial()`](https://blasbenito.github.io/spatialRF/reference/rf_spatial.md),
[`plot_response_curves()`](https://blasbenito.github.io/spatialRF/reference/plot_response_curves.md),
[`get_importance()`](https://blasbenito.github.io/spatialRF/reference/get_importance.md)

Other model_info:
[`get_evaluation()`](https://blasbenito.github.io/spatialRF/reference/get_evaluation.md),
[`get_importance()`](https://blasbenito.github.io/spatialRF/reference/get_importance.md),
[`get_importance_local()`](https://blasbenito.github.io/spatialRF/reference/get_importance_local.md),
[`get_moran()`](https://blasbenito.github.io/spatialRF/reference/get_moran.md),
[`get_performance()`](https://blasbenito.github.io/spatialRF/reference/get_performance.md),
[`get_predictions()`](https://blasbenito.github.io/spatialRF/reference/get_predictions.md),
[`get_residuals()`](https://blasbenito.github.io/spatialRF/reference/get_residuals.md),
[`get_spatial_predictors()`](https://blasbenito.github.io/spatialRF/reference/get_spatial_predictors.md),
[`print.rf()`](https://blasbenito.github.io/spatialRF/reference/print.md),
[`print_evaluation()`](https://blasbenito.github.io/spatialRF/reference/print_evaluation.md),
[`print_importance()`](https://blasbenito.github.io/spatialRF/reference/print_importance.md),
[`print_moran()`](https://blasbenito.github.io/spatialRF/reference/print_moran.md),
[`print_performance()`](https://blasbenito.github.io/spatialRF/reference/print_performance.md)

## Examples

``` r
data(plants_rf)

# Extract response curve data for plotting
curves <- get_response_curves(
  model = plants_rf,
  variables = NULL,  # auto-select important variables
  quantiles = c(0.1, 0.5, 0.9)
)

# View structure
head(curves)
#>   response predictor quantile model       predictor.name
#> 1 1222.094 -183.8091      0.1     1 climate_bio1_average
#> 2 1222.094 -181.5008      0.1     1 climate_bio1_average
#> 3 1222.094 -179.1924      0.1     1 climate_bio1_average
#> 4 1222.094 -176.8841      0.1     1 climate_bio1_average
#> 5 1222.094 -174.5758      0.1     1 climate_bio1_average
#> 6 1222.094 -172.2675      0.1     1 climate_bio1_average
#>               response.name
#> 1 richness_species_vascular
#> 2 richness_species_vascular
#> 3 richness_species_vascular
#> 4 richness_species_vascular
#> 5 richness_species_vascular
#> 6 richness_species_vascular
str(curves)
#> 'data.frame':    4800 obs. of  6 variables:
#>  $ response      : num  1222 1222 1222 1222 1222 ...
#>  $ predictor     : num  -184 -182 -179 -177 -175 ...
#>  $ quantile      : Factor w/ 3 levels "0.1","0.5","0.9": 1 1 1 1 1 1 1 1 1 1 ...
#>  $ model         : int  1 1 1 1 1 1 1 1 1 1 ...
#>  $ predictor.name: chr  "climate_bio1_average" "climate_bio1_average" "climate_bio1_average" "climate_bio1_average" ...
#>  $ response.name : chr  "richness_species_vascular" "richness_species_vascular" "richness_species_vascular" "richness_species_vascular" ...

# Check unique predictors included
unique(curves$predictor.name)
#> [1] "climate_bio1_average"          "human_population"             
#> [3] "climate_hypervolume"           "bias_area_km2"                
#> [5] "human_population_density"      "human_footprint_average"      
#> [7] "climate_aridity_index_average" "neighbors_count"              
```
