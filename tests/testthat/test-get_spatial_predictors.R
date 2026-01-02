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


test_that("get_spatial_predictors() validates model class", {
  data(plants_rf)

  # Non-spatial model should error
  expect_error(
    get_spatial_predictors(plants_rf),
    "only works on models fitted with 'rf_spatial'"
  )
})

test_that("get_spatial_predictors() works with spatial models", {
  data(plants_rf_spatial)

  result <- get_spatial_predictors(plants_rf_spatial)

  expect_s3_class(result, "data.frame")
  expect_true(ncol(result) > 0)
  expect_true(nrow(result) > 0)
})

test_that("get_spatial_predictors() returns correct structure", {
  data(plants_rf_spatial)

  result <- get_spatial_predictors(plants_rf_spatial)

  # All columns should be numeric
  expect_true(all(sapply(result, is.numeric)))

  # Column names should start with spatial_predictor
  expect_true(all(grepl("spatial_predictor", colnames(result))))
})
