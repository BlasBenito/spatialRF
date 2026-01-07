test_that("get_importance() works with rf models", {
  data(plants_rf)

  x <- get_importance(plants_rf)

  expect_s3_class(x, "data.frame")
  expect_named(x, c("variable", "importance"))
  expect_true(nrow(x) > 0)
  expect_true(is.character(x$variable))
  expect_true(is.numeric(x$importance))
})

test_that("get_importance() works with rf_spatial models", {
  data(plants_rf_spatial)

  x <- get_importance(plants_rf_spatial)

  expect_s3_class(x, "data.frame")
  expect_named(x, c("variable", "importance"))
  expect_true(nrow(x) > 0)
})

test_that("get_importance() works with rf_repeat models", {
  data(plants_rf)

  # Create rf_repeat model
  model_repeat <- rf_repeat(
    model = plants_rf,
    repetitions = 5,
    verbose = FALSE
  )

  x <- get_importance(model_repeat)

  expect_s3_class(x, "data.frame")
  expect_named(x, c("variable", "importance"))
  expect_true(nrow(x) > 0)
})

test_that("get_importance() works with rf_spatial rf_repeat models", {
  data(plants_rf_spatial)

  # Create rf_repeat from spatial model
  model_spatial_repeat <- rf_repeat(
    model = plants_rf_spatial,
    repetitions = 5,
    verbose = FALSE
  )

  x <- get_importance(model_spatial_repeat)

  expect_s3_class(x, "data.frame")
  expect_named(x, c("variable", "importance"))
  expect_true(nrow(x) > 0)
})

test_that("get_importance() sorts by decreasing importance", {
  data(plants_rf)

  x <- get_importance(plants_rf)

  # Check that importance is sorted in decreasing order
  expect_true(all(diff(x$importance) <= 0))
})

test_that("get_importance() returns correct structure", {
  data(plants_rf)

  x <- get_importance(plants_rf)

  # Should have two columns
  expect_equal(ncol(x), 2)

  # Number of rows should match number of predictors
  expect_true(nrow(x) > 0)

  # All importance values should be numeric (can be negative)
  expect_true(all(is.numeric(x$importance)))
})
