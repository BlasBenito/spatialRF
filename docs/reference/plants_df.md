# Plant richness and predictors for American ecoregions

Vascular plant species richness for American ecoregions as defined in
[Ecoregions 2017](https://ecoregions2017.appspot.com/).

## Usage

``` r
data(plants_df)
```

## Format

A data frame with 227 rows and 22 columns:

- `ecoregion_id`: Ecoregion identifier.

- `x`: Longitude in degrees (WGS84).

- `y`: Latitude in degrees (WGS84).

- `richness_species_vascular`: Number of vascular plant species
  (response variable).

- `bias_area_km2`: Ecoregion area in square kilometers.

- `bias_species_per_record`: Species count divided by GBIF spatial
  records (sampling bias metric).

- `climate_aridity_index_average`: Average aridity index.

- `climate_hypervolume`: Climatic envelope volume computed with
  [hypervolume](https://cran.r-project.org/package=hypervolume).

- `climate_velocity_lgm_average`: Average climate velocity since the
  Last Glacial Maximum.

- `neighbors_count`: Number of immediate neighbors (connectivity
  metric).

- `neighbors_percent_shared_edge`: Percentage of shared edge with
  neighbors (connectivity metric).

- `human_population_density`: Human population density.

- `topography_elevation_average`: Average elevation.

- `landcover_herbs_percent_average`: Average herb cover from MODIS
  Vegetation Continuous Fields.

- `fragmentation_cohesion`: Cohesion index computed with
  [landscapemetrics](https://CRAN.R-project.org/package=landscapemetrics).

- `fragmentation_division`: Division index computed with
  [landscapemetrics](https://CRAN.R-project.org/package=landscapemetrics).

- `neighbors_area`: Total area of immediate neighbors.

- `human_population`: Total human population.

- `human_footprint_average`: Average human footprint index.

- `climate_bio1_average`: Average mean annual temperature.

- `climate_bio15_minimum`: Minimum precipitation seasonality.

## See also

Other data:
[`plants_distance`](https://blasbenito.github.io/spatialRF/reference/plants_distance.md),
[`plants_predictors`](https://blasbenito.github.io/spatialRF/reference/plants_predictors.md),
[`plants_response`](https://blasbenito.github.io/spatialRF/reference/plants_response.md),
[`plants_rf`](https://blasbenito.github.io/spatialRF/reference/plants_rf.md),
[`plants_rf_spatial`](https://blasbenito.github.io/spatialRF/reference/plants_rf_spatial.md),
[`plants_xy`](https://blasbenito.github.io/spatialRF/reference/plants_xy.md)
