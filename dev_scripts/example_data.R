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
