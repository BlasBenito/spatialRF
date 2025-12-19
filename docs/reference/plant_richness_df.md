# Plant richness and predictors of American ecoregions

Richness of vascular plants of the American ecoregions as defined in
[Ecoregions 2017](https://ecoregions2017.appspot.com/).

## Usage

``` r
data(plant_richness_df)
```

## Format

A data frame with 227 rows and 22 columns:

- `ecoregion_id`: Id of the ecoregion).

- `x`: Longitude in degrees (WGS84).

- `y`: Latitude in degrees (WGS84).

- `richness_species_vascular`: Number of vascular species found in the
  ecoregion. Response variable.

- `bias_area_km2`: Area of the ecoregion in squared kilometers.

- `bias_species_per_record`: Number of species divided by the number of
  spatial GBIF records available in the ecoregion as a measure of
  sampling bias.

- `climate_aridity_index_average`: Average of the ecoregion.

- `climate_hypervolume`: Volume of the climatic envelope of the
  ecoregion, computed with the
  [hypervolume](https://cran.r-project.org/package=hypervolume) package.

- `climate_velocity_lgm_average`: Average climate velocity of the
  ecoregion since the Last Glacial Maximum.

- `neighbors_count`: Number of immediate neighbors of the ecoregion as a
  measure of connectivity/isolation.

- `neighbors_percent_shared_edge`: Percentage of shared edge with the
  neighbors as a measure of connectivity/isolation.

- `human_population_density`: Population density of the ecoregion.

- `topography_elevation_average`: Average elevation of the ecoregion.

- `landcover_herbs_percent_average`: Average cover percentage of herbs
  extracted from MODIS Vegetation Continuous Fields.

- `fragmentation_cohesion`: Geographic fragmentation index of the
  ecoregion as computed with the R package
  [landscapemetrics](https://CRAN.R-project.org/package=landscapemetrics).

- `fragmentation_division`: Another fragmentation index.

- `neighbors_area`: Total area of the ecoregions's immediate neighbors.

- `human_population`: Human population in the ecoregion.

- `human_footprint_average`: Average human footprint in the ecoregion.

- `climate_bio1_average`: Average mean annual temperature.

- `climate_bio15_minimum`: Average precipitation seasonality.

## See also

[distance_matrix](https://blasbenito.github.io/spatialRF/reference/distance_matrix.md)
