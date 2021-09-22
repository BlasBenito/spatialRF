#' @title Plant richness and predictors of American ecoregions
#'
#' @description Richness of vascular plants of the American ecoregions as defined in [Ecoregions 2017](https://ecoregions2017.appspot.com/).
#' @usage data(plant_richness_df)
#' @seealso [distance_matrix]
#' @format A data frame with 227 rows and 22 columns:
#' \itemize{
#'   \item `ecoregion_id`: Id of the ecoregion).
#'   \item `x`: Longitude in degrees (WGS84).
#'   \item `y`: Latitude in degrees (WGS84).
#'   \item `richness_species_vascular`: Number of vascular species found in the ecoregion. Response variable.
#'   \item `bias_area_km2`: Area of the ecoregion in squared kilometers.
#'   \item `bias_species_per_record`: Number of species divided by the number of spatial GBIF records available in the ecoregion as a measure of sampling bias.
#'   \item `climate_aridity_index_average`: Average [aridity index](https://figshare.com/articles/dataset/Global_Aridity_Index_and_Potential_Evapotranspiration_ET0_Climate_Database_v2/7504448) of the ecoregion.
#'   \item `climate_hypervolume`: Volume of the climatic envelope of the ecoregion, computed with the [hypervolume](https://cran.r-project.org/package=hypervolume) package.
#'   \item `climate_velocity_lgm_average`: Average [climate velocity](https://www.science.org/doi/10.1126/science.1210173) of the ecoregion since the Last Glacial Maximum.
#'   \item `neighbors_count`: Number of immediate neighbors of the ecoregion as a measure of connectivity/isolation.
#'   \item `neighbors_percent_shared_edge`: Percentage of shared edge with the neighbors as a measure of connectivity/isolation.
#'   \item `human_population_density`: Population density of the ecoregion.
#'   \item `topography_elevation_average`: Average elevation of the ecoregion.
#'   \item `landcover_herbs_percent_average`: Average cover percentage of herbs extracted from [MODIS Vegetation Continuous Fields](https://modis-land.gsfc.nasa.gov/vcc.html).
#'   \item `fragmentation_cohesion`: Geographic fragmentation index of the ecoregion as computed with the R package [landscapemetrics]( https://CRAN.R-project.org/package=landscapemetrics).
#'   \item `fragmentation_division`: Another fragmentation index.
#'   \item `neighbors_area`: Total area of the ecoregions's immediate neighbors.
#'   \item `human_population`: Human population in the ecoregion.
#'   \item `human_footprint_average`: Average human footprint \doi{10.1641/0006-3568(2002)052[0891:THFATL]2.0.CO;2} in the ecoregion.
#'   \item `climate_bio1_average`: Average mean annual temperature.
#'   \item `climate_bio15_minimum`: Average precipitation seasonality.
#'
#' }
"plant_richness_df"
