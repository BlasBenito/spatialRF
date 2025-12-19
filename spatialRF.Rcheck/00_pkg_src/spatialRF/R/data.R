#' @title Plant richness and predictors for American ecoregions
#' @description Vascular plant species richness for American ecoregions as defined in [Ecoregions 2017](https://ecoregions2017.appspot.com/).
#' @usage data(plants_df)
#' @format A data frame with 227 rows and 22 columns:
#' \itemize{
#'   \item `ecoregion_id`: Ecoregion identifier.
#'   \item `x`: Longitude in degrees (WGS84).
#'   \item `y`: Latitude in degrees (WGS84).
#'   \item `richness_species_vascular`: Number of vascular plant species (response variable).
#'   \item `bias_area_km2`: Ecoregion area in square kilometers.
#'   \item `bias_species_per_record`: Species count divided by GBIF spatial records (sampling bias metric).
#'   \item `climate_aridity_index_average`: Average aridity index.
#'   \item `climate_hypervolume`: Climatic envelope volume computed with [hypervolume](https://cran.r-project.org/package=hypervolume).
#'   \item `climate_velocity_lgm_average`: Average climate velocity since the Last Glacial Maximum.
#'   \item `neighbors_count`: Number of immediate neighbors (connectivity metric).
#'   \item `neighbors_percent_shared_edge`: Percentage of shared edge with neighbors (connectivity metric).
#'   \item `human_population_density`: Human population density.
#'   \item `topography_elevation_average`: Average elevation.
#'   \item `landcover_herbs_percent_average`: Average herb cover from MODIS Vegetation Continuous Fields.
#'   \item `fragmentation_cohesion`: Cohesion index computed with [landscapemetrics](https://CRAN.R-project.org/package=landscapemetrics).
#'   \item `fragmentation_division`: Division index computed with [landscapemetrics](https://CRAN.R-project.org/package=landscapemetrics).
#'   \item `neighbors_area`: Total area of immediate neighbors.
#'   \item `human_population`: Total human population.
#'   \item `human_footprint_average`: Average human footprint index.
#'   \item `climate_bio1_average`: Average mean annual temperature.
#'   \item `climate_bio15_minimum`: Minimum precipitation seasonality.
#'
#' }
#' @family data
"plants_df"

#' @title Distance matrix between ecoregion edges
#' @description Distance matrix (in km) between the edges of American ecoregions in [plants_df].
#' @usage data(plants_distance)
#' @format Numeric matrix with 227 rows and 227 columns.
#' @family data
"plants_distance"

#' @title Coordinates for plant richness data
#' @description Spatial coordinates (longitude and latitude) extracted from [plants_df] for use in spatial modeling functions.
#' @usage data(plants_xy)
#' @format A data frame with 227 rows and 2 columns:
#' \itemize{
#'   \item `x`: Longitude in degrees (WGS84).
#'   \item `y`: Latitude in degrees (WGS84).
#' }
#' @family data
"plants_xy"

#' @title Response variable name for plant richness examples
#'
#' @description Character string containing the name of the response variable in [plants_df]: "richness_species_vascular".
#' @usage data(plants_response)
#' @format A character string of length 1.
#' @family data
"plants_response"

#' @title Predictor variable names for plant richness examples
#' @description Character vector of predictor variable names from [plants_df] (columns 5 to 21).
#' @usage data(plants_predictors)
#' @format A character vector of length 17.
#' @family data
"plants_predictors"

#' @title Example fitted random forest model
#' @description Fitted random forest model using [plants_df]. Provided for testing and examples without requiring model fitting. Fitted with reduced complexity for faster computation and smaller object size.
#'
#' @usage data(plants_rf)
#'
#' @format An object of class `rf` fitted with the following parameters:
#' \itemize{
#'   \item `data`: [plants_df]
#'   \item `dependent.variable.name`: [plants_response] ("richness_species_vascular")
#'   \item `predictor.variable.names`: [plants_predictors] (17 variables)
#'   \item `distance.matrix`: [plants_distance]
#'   \item `xy`: [plants_xy]
#'   \item `distance.thresholds`: `c(100, 1000, 2000, 4000)`
#'   \item `num.trees`: 50
#'   \item `min.node.size`: 30
#'   \item `n.cores`: 1
#' }
#'
#' @details
#' This model uses reduced complexity (50 trees, min.node.size = 30) to keep object size small for package distribution. For actual analyses, use higher values (e.g., num.trees = 500, min.node.size = 5).
#'
#' @seealso [rf()], [plants_df], [plants_response], [plants_predictors]
#' @family data
"plants_rf"

#' @title Example fitted spatial random forest model
#' @description Fitted spatial random forest model using [plants_df] with spatial predictors from Moran's Eigenvector Maps. Provided for testing and examples without requiring model fitting. Fitted with reduced complexity for faster computation and smaller object size.
#'
#' @usage data(plants_rf_spatial)
#'
#' @format An object of class `rf` fitted with the following parameters:
#' \itemize{
#'   \item `data`: [plants_df]
#'   \item `dependent.variable.name`: [plants_response] ("richness_species_vascular")
#'   \item `predictor.variable.names`: [plants_predictors] (17 variables)
#'   \item `distance.matrix`: [plants_distance]
#'   \item `xy`: [plants_xy]
#'   \item `distance.thresholds`: `c(100, 1000, 2000, 4000)`
#'   \item `method`: "mem.effect.recursive"
#'   \item `num.trees`: 50
#'   \item `min.node.size`: 30
#'   \item `n.cores`: 14
#' }
#'
#' @details
#' This spatial model includes spatial predictors (Moran's Eigenvector Maps) selected using the recursive method to minimize residual spatial autocorrelation. Uses reduced complexity (50 trees, min.node.size = 30) to keep object size small for package distribution. For actual analyses, use higher values (e.g., num.trees = 500, min.node.size = 5).
#'
#' @seealso [rf_spatial()], [rf()], [plants_rf], [plants_df], [plants_response], [plants_predictors]
#' @family data
"plants_rf_spatial"
