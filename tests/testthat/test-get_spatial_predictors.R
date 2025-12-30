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
