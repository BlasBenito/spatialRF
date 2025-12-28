test_that("`get_spatial_predictors()` works", {
  data("plants_distance", package = "spatialRF")
  data("plants_df", package = "spatialRF")

  plants_distance <- plants_distance[1:100, 1:100]
  plants_df <- plants_df[1:100, ]

  model <- rf_spatial(
    data = plants_df,
    dependent.variable.name = "richness_species_vascular",
    predictor.variable.names = colnames(plants_df)[5:21],
    distance.matrix = plants_distance,
    distance.thresholds = c(0, 500, 1000),
    method = "mem.moran.sequential",
    verbose = FALSE
  )
  spatial.predictors <- get_spatial_predictors(model)
  expect_s3_class(spatial.predictors, "data.frame")
})
