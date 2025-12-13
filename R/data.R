#' @title Plant richness and predictors of American ecoregions
#'
#' @description Richness of vascular plants of the American ecoregions as defined in [Ecoregions 2017](https://ecoregions2017.appspot.com/).
#' @usage data(plants_df)
#' @format A data frame with 227 rows and 22 columns:
#' \itemize{
#'   \item `ecoregion_id`: Id of the ecoregion).
#'   \item `x`: Longitude in degrees (WGS84).
#'   \item `y`: Latitude in degrees (WGS84).
#'   \item `richness_species_vascular`: Number of vascular species found in the ecoregion. Response variable.
#'   \item `bias_area_km2`: Area of the ecoregion in squared kilometers.
#'   \item `bias_species_per_record`: Number of species divided by the number of spatial GBIF records available in the ecoregion as a measure of sampling bias.
#'   \item `climate_aridity_index_average`: Average of the ecoregion.
#'   \item `climate_hypervolume`: Volume of the climatic envelope of the ecoregion, computed with the [hypervolume](https://cran.r-project.org/package=hypervolume) package.
#'   \item `climate_velocity_lgm_average`: Average climate velocity of the ecoregion since the Last Glacial Maximum.
#'   \item `neighbors_count`: Number of immediate neighbors of the ecoregion as a measure of connectivity/isolation.
#'   \item `neighbors_percent_shared_edge`: Percentage of shared edge with the neighbors as a measure of connectivity/isolation.
#'   \item `human_population_density`: Population density of the ecoregion.
#'   \item `topography_elevation_average`: Average elevation of the ecoregion.
#'   \item `landcover_herbs_percent_average`: Average cover percentage of herbs extracted from MODIS Vegetation Continuous Fields.
#'   \item `fragmentation_cohesion`: Geographic fragmentation index of the ecoregion as computed with the R package [landscapemetrics](https://CRAN.R-project.org/package=landscapemetrics).
#'   \item `fragmentation_division`: Another fragmentation index.
#'   \item `neighbors_area`: Total area of the ecoregions's immediate neighbors.
#'   \item `human_population`: Human population in the ecoregion.
#'   \item `human_footprint_average`: Average human footprint in the ecoregion.
#'   \item `climate_bio1_average`: Average mean annual temperature.
#'   \item `climate_bio15_minimum`: Average precipitation seasonality.
#'
#' }
"plants_df"

#' Matrix of distances among ecoregion edges.
#'
#' Distance matrix (in km) among the edges of the American ecoregions described in the [plants_df] dataset.
#'
#' @usage data(plants_distance)
#'
#' @format A numeric matrix with 227 rows and columns.
"plants_distance"

#' @title Coordinates of plant richness data
#'
#' @description Data frame containing the x and y coordinates extracted from [plants_df] for use in spatial modeling functions.
#' @usage data(plants_xy)
#' @format A data frame with 227 rows and 2 columns:
#' \itemize{
#'   \item `x`: Longitude in degrees (WGS84).
#'   \item `y`: Latitude in degrees (WGS84).
#' }
"plants_xy"

#' @title Response variable name for plant richness examples
#'
#' @description Character string containing the name of the response variable in [plants_df]: "richness_species_vascular".
#' @usage data(plants_response)
#' @format A character string of length 1.
"plants_response"

#' @title Predictor variable names for plant richness examples
#'
#' @description Character vector containing the names of predictor variables in [plants_df] (columns 5 to 21).
#' @usage data(plants_predictors)
#' @format A character vector of length 17.
"plants_predictors"
