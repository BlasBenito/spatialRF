test_that("`get_spatial_predictors()` works", {
  data(plants_rf_spatial)

  x <- get_spatial_predictors(plants_rf_spatial)

  expect_s3_class(x, "data.frame")
  expect_true(ncol(x) > 0)
  expect_true(nrow(x) > 0)
})


test_that("get_spatial_predictors() validates model class", {
  data(plants_rf)

  # Non-spatial model should error
  expect_error(
    get_spatial_predictors(plants_rf),
    "only works on models fitted with 'rf_spatial'"
  )
})
