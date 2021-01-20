#' Vascular plant richness of the american ecoregions.
#'
#' Richness of vascular plants the american ecoregions as defined in [Ecoregions 2017](https://ecoregions2017.appspot.com/).
#' plant_richness_sf
#'
#' @format A spatial data frame (sf) file with 227 rows and 22 columns:
#' \describe{
#'   \item{ecoregion_id}{id of the ecoregion).}
#'   \item{x}{longitude in degrees (WGS84).}
#'   \item{y}{latitude in degrees (WGS84).}
#'   \item{richness_species_vascular}{number of vascular species found in the ecoregion.}
#'   \item{bias_area_km2}{area of the ecoregion in squared kilometers.}
#'   \item{bias_species_per_record}{number of species divided by the number of spatial GBIF records available in the ecoregion as a measure of sampling bias.}
#'   \item{climate_aridity_index_average}{average [aridity index](https://figshare.com/articles/dataset/Global_Aridity_Index_and_Potential_Evapotranspiration_ET0_Climate_Database_v2/7504448) of the ecoregion.}
#'   \item{climate_hypervolume}{volume of the climatic envelope of the ecoregion.}
#'   \item{climate_velocity_lgm_average}{average [climate velocity](https://science.sciencemag.org/content/334/6056/660) of the ecoregion since the Last Glacial Maximum.}
#'   \item{neighbors_count}{number of immediate neighbors of the ecoregion as a measure of connectivity/isolation.}
#'   \item{neighbors_percent_shared_edge}{percentage of shared edge with the neighbors as a measure of connectivity/isolation.}
#'   \item{human_population_density}{population density of the ecoregion.}
#'   \item{topography_elevation_average}{average elevation of the ecoregion.}
#'   \item{landcover_herbs_percent_average}{average cover percentage of herbs extracted from [MODIS Vegetation Continuous Fields](https://modis-land.gsfc.nasa.gov/vcc.html).}
#'   \item{fragmentation_cohesion}{geographic fragmentation index of the ecoregion as computed with the R package [landscapemetrics](https://cran.r-project.org/web/packages/landscapemetrics/index.html).}
#'   \item{fragmentation_division}{another fragmentation index.}
#'   \item{neighbors_area}{total area of the ecoregions's immediate neighbors.}
#'   \item{human_population}{human population in the ecoregion.}
#'   \item{human_footprint_average}{average [human footprint](https://sedac.ciesin.columbia.edu/data/set/wildareas-v3-2009-human-footprint) in the ecoregion.}
#'   \item{climate_bio1_average}{average mean annual temperature according to [CHELSA](https://chelsa-climate.org/).}
#'   \item{climate_bio15_minimum}{average precipitation seasonality.}
#'   \item{geom_centroids}{spatial definition of the ecoregions centroids.}
#'
#' }
"plant_richness_sf"

#'
#' Richness of vascular plants the american ecoregions as defined in [Ecoregions 2017](https://ecoregions2017.appspot.com/).
#' plant_richness_df
#'
#' @format A data frame (sf) file with 227 rows and 22 columns:
#' \describe{
#'   \item{ecoregion_id}{id of the ecoregion).}
#'   \item{x}{longitude in degrees (WGS84).}
#'   \item{y}{latitude in degrees (WGS84).}
#'   \item{richness_species_vascular}{number of vascular species found in the ecoregion.}
#'   \item{bias_area_km2}{area of the ecoregion in squared kilometers.}
#'   \item{bias_species_per_record}{number of species divided by the number of spatial GBIF records available in the ecoregion as a measure of sampling bias.}
#'   \item{climate_aridity_index_average}{average [aridity index](https://figshare.com/articles/dataset/Global_Aridity_Index_and_Potential_Evapotranspiration_ET0_Climate_Database_v2/7504448) of the ecoregion.}
#'   \item{climate_hypervolume}{volume of the climatic envelope of the ecoregion.}
#'   \item{climate_velocity_lgm_average}{average [climate velocity](https://science.sciencemag.org/content/334/6056/660) of the ecoregion since the Last Glacial Maximum.}
#'   \item{neighbors_count}{number of immediate neighbors of the ecoregion as a measure of connectivity/isolation.}
#'   \item{neighbors_percent_shared_edge}{percentage of shared edge with the neighbors as a measure of connectivity/isolation.}
#'   \item{human_population_density}{population density of the ecoregion.}
#'   \item{topography_elevation_average}{average elevation of the ecoregion.}
#'   \item{landcover_herbs_percent_average}{average cover percentage of herbs extracted from [MODIS Vegetation Continuous Fields](https://modis-land.gsfc.nasa.gov/vcc.html).}
#'   \item{fragmentation_cohesion}{geographic fragmentation index of the ecoregion as computed with the R package [landscapemetrics](https://cran.r-project.org/web/packages/landscapemetrics/index.html).}
#'   \item{fragmentation_division}{another fragmentation index.}
#'   \item{neighbors_area}{total area of the ecoregions's immediate neighbors.}
#'   \item{human_population}{human population in the ecoregion.}
#'   \item{human_footprint_average}{average [human footprint](https://sedac.ciesin.columbia.edu/data/set/wildareas-v3-2009-human-footprint) in the ecoregion.}
#'   \item{climate_bio1_average}{average mean annual temperature according to [CHELSA](https://chelsa-climate.org/).}
#'   \item{climate_bio15_minimum}{average precipitation seasonality.}
#'
#' }
"plant_richness_df"
