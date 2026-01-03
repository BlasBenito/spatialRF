library(spatialRF)

data("plant_richness_df")
data("distance_matrix")

plants_df <- plant_richness_df
plants_distance <- distance_matrix
plants_xy <- plant_richness_df[, c("x", "y")]
plants_response <- "richness_species_vascular"
plants_predictors <- colnames(plant_richness_df)[5:21]


usethis::use_data(plants_df, overwrite = TRUE)
usethis::use_data(plants_distance, overwrite = TRUE)
usethis::use_data(plants_xy, overwrite = TRUE)
usethis::use_data(plants_response, overwrite = TRUE)
usethis::use_data(plants_predictors, overwrite = TRUE)

#from dataframe
plants_distance <- spatialRF::distance_matrix(
  data = plants_df,
  crs = 4326
)

#convert to km and set new units
plants_distance <- plants_distance / 1000
attributes(plants_distance)$units <- "km"

usethis::use_data(plants_distance, overwrite = TRUE)

#from sf
plants_sf <- sf::st_as_sf(
  x = plants_df,
  coords = c("x", "y"),
  crs = 4326
)

plants_distance <- distance_matrix(
  data = plants_sf
)

plants_rf <- rf(
  data = plants_df,
  dependent.variable.name = plants_response,
  predictor.variable.names = plants_predictors,
  distance.matrix = plants_distance,
  xy = plants_xy,
  distance.thresholds = c(100, 1000, 2000, 4000),
  ranger.arguments = list(
    num.trees = 50,
    min.node.size = 30
  ),
  n.cores = 1,
  verbose = FALSE
)

usethis::use_data(plants_rf, overwrite = TRUE, compress = "xz")

plants_rf_spatial <- rf_spatial(
  data = plants_df,
  dependent.variable.name = plants_response,
  predictor.variable.names = plants_predictors,
  distance.matrix = plants_distance,
  xy = plants_xy,
  distance.thresholds = c(100, 1000, 2000, 4000),
  method = "mem.effect.recursive",
  ranger.arguments = list(
    num.trees = 50,
    min.node.size = 30
  ),
  n.cores = 14,
  verbose = FALSE
)

usethis::use_data(plants_rf_spatial, overwrite = TRUE, compress = "xz")

#recompress
tools::resaveRdaFiles("data", compress = "xz")
