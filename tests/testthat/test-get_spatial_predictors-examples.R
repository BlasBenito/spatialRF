test_that("`get_spatial_predictors()` works", {
  data(distance_matrix)
  data(plant_richness_df)
  model <- rf_spatial(
    data = plant_richness_df,
    dependent.variable.name = "richness_species_vascular",
    predictor.variable.names = colnames(plant_richness_df)[5:21],
    distance.matrix = distance_matrix,
    distance.thresholds = c(0, 500, 1000),
    method = "mem.moran.sequential",
    verbose = FALSE,
    n.cores = 1
  )
  spatial.predictors <- get_spatial_predictors(model)
  expect_s3_class(spatial.predictors, "data.frame")
})
