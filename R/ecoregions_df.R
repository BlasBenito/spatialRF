#' @title Plant richness and predictors of American ecoregions
#'
#' @description Richness of vascular plants of the American ecoregions as defined in [Ecoregions 2017](https://ecoregions2017.appspot.com/), and computed from GBIF data, and environmental predictors.
#' @usage data(ecoregions_df)
#' @seealso [ecoregions_polygons], [ecoregions_distance_matrix], [ecoregions_dependent_variable_name], [ecoregions_predictor_variable_names]
#' @format A data frame with 225 rows and 49 columns:
#' \itemize{
#'   \item `ecoregion_id`: Numeric id of the ecoregion.
#'   \item `ecoregion_name`: Name of the ecoregion according to [Ecoregions 2017](https://ecoregions2017.appspot.com/).
#'   \item `x` and `y`: Numeric, coordinates of the ecoregion centroids, in EPSG 4326.
#'   \item `plant_richness`: Number of vascular species found in the ecoregion. Response variable.
#'   \item `plant_richness_binary`: Binary version of `plant_richness`, with value `1` when `plant_richness` is higher than 5000, and `0` otherwise.
#'   \item `ecoregion_area_km2`: Area of the ecoregion in squared kilometers.
#'   \item `sampling_bias`: Indicator of sampling bias, measured as number of vascular plant species found in the ecoregion divided by the number of GBIF records of the ecoregion, divided by the area of the ecoregion.
#'   \item `neighbors_count`, `neighbors_area`, `neighbors_percent_shared_edge`, and `neighbors_average_aridity`: Number, area sum, proportion of shared edge, and average aridity of the direct neighbors of each ecoregion.
#'   \item `climate_aridity_index_average`: Average [aridity index](https://figshare.com/articles/dataset/Global_Aridity_Index_and_Potential_Evapotranspiration_ET0_Climate_Database_v2/7504448) of the ecoregion.
#'   \item `climate_bio1_average` to `climate_bio15_average`: Bioclimatic variables representing average annual temperature (BIO1), temperature_seasonality (BIO4), average and maximum temperature of the warmest month (BIO5), minimum, average and maximum monthly rainfall (BIO12), and precipitation seasonlality (BIO15).
#'   \item `climate_hypervolume`: Volume of the climatic envelope of the ecoregion, computed with the [hypervolume](https://cran.r-project.org/package=hypervolume) package.
#'   \item `landcover_bare_percent_average`, `landcover_herbs_percent_average`, AND `landcover_trees_percent_average`, : Average cover percentage of bare soil, herbs, and trees, extracted from [MODIS Vegetation Continuous Fields](https://modis-land.gsfc.nasa.gov/vcc.html).
#'   \item `topography_elevation_average` and `topography_elevation_range`: Average elevation of the ecoregion and its range.

#'   \item `fragmentation_ai` to `fragmentation_te`: Geographic fragmentation indices of the ecoregion,  computed with the R package [landscapemetrics](https://CRAN.R-project.org/package=landscapemetrics).
#'
#' }
"ecoregions_df"

#' Ecoregion polygons.
#'
#' Simplified ecoregion polygons in sf format.
#'
#' @usage data(ecoregions_polygons)
#' @seealso [ecoregions_df]
#'
#' @format A data frame of the classes "sf" and "data.frame".
"ecoregions_polygons"

#' Matrix of distances among ecoregion edges.
#'
#' Distance matrix (in km) among the edges of the American ecoregions described in the [ecoregions_df] dataset.
#'
#' @usage data(ecoregions_distance_matrix)
#' @seealso [ecoregions_df]
#'
#' @format A numeric matrix with 227 rows and columns.
"ecoregions_distance_matrix"

#' Name of the dependent variable
#'
#' Character string with the name of the dependent variable.
#'
#' @usage data(ecoregions_dependent_variable_name)
#' @seealso [ecoregions_df]
#'
#' @format Character string.
"ecoregions_dependent_variable_name"

#' Names of the predictor variables
#'
#' Character vector with the names of the predictors.
#'
#' @usage data(ecoregions_predictor_variable_names)
#' @seealso [ecoregions_df]
#'
#' @format Character vector with 43 predictor names.
"ecoregions_predictor_variable_names"
