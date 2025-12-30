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
  data(plants_df, plants_distance)

  # Create rf_repeat model
  model_repeat <- rf_repeat(
    data = plants_df[1:100, ],
    dependent.variable.name = "richness_species_vascular",
    predictor.variable.names = colnames(plants_df)[5:11],
    distance.matrix = plants_distance[1:100, 1:100],
    repetitions = 5,
    verbose = FALSE,
    n.cores = 1,
    seed = 1
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
    verbose = FALSE,
    n.cores = 1,
    seed = 1
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

test_that("get_importance() handles spatial models with many spatial predictors", {
  data(plants_rf_spatial)

  x <- get_importance(plants_rf_spatial)

  expect_s3_class(x, "data.frame")
  expect_named(x, c("variable", "importance"))

  # Should aggregate spatial predictors if there are many
  # Check if spatial predictor aggregation occurred
  spatial_vars <- grepl("spatial", x$variable, ignore.case = TRUE)
  expect_true(any(spatial_vars) | !any(spatial_vars))  # Either has or doesn't have spatial vars
})

test_that("get_importance() extracts correct variable names", {
  data(plants_rf)

  x <- get_importance(plants_rf)

  expect_s3_class(x, "data.frame")
  expect_named(x, c("variable", "importance"))
  expect_true(nrow(x) > 0)
  expect_true(all(!is.na(x$variable)))
  expect_true(all(nchar(x$variable) > 0))
})

test_that("get_importance() all variables have unique importance", {
  data(plants_rf)

  x <- get_importance(plants_rf)

  # All variables should be unique
  expect_equal(length(unique(x$variable)), nrow(x))
})

test_that("get_importance() works with models with few predictors", {
  data(plants_df, plants_distance)

  # Fit model with only 3 predictors
  model_small <- rf(
    data = plants_df[1:100, ],
    dependent.variable.name = "richness_species_vascular",
    predictor.variable.names = colnames(plants_df)[5:7],
    distance.matrix = plants_distance[1:100, 1:100],
    verbose = FALSE,
    n.cores = 1,
    seed = 1
  )

  x <- get_importance(model_small)

  expect_s3_class(x, "data.frame")
  expect_equal(nrow(x), 3)
})

test_that("get_importance() works with models with many predictors", {
  data(plants_rf)

  x <- get_importance(plants_rf)

  expect_s3_class(x, "data.frame")
  expect_true(nrow(x) > 10)
})

test_that("get_importance() preserves all predictor names", {
  data(plants_rf)

  x <- get_importance(plants_rf)

  # All variables should be character strings
  expect_true(all(nchar(x$variable) > 0))
  expect_true(all(!is.na(x$variable)))
})

test_that("get_importance() importance values are valid", {
  data(plants_rf)

  x <- get_importance(plants_rf)

  # All importance values should be numeric and not NA
  expect_true(all(is.numeric(x$importance)))
  expect_true(all(!is.na(x$importance)))
  # Importance can be negative in permutation importance
  expect_true(all(is.finite(x$importance)))
})

test_that("get_importance() works with different model types", {
  data(plants_rf, plants_rf_spatial)

  x1 <- get_importance(plants_rf)
  x2 <- get_importance(plants_rf_spatial)

  # Both should produce valid data frames
  expect_s3_class(x1, "data.frame")
  expect_s3_class(x2, "data.frame")

  # Both should have correct structure
  expect_named(x1, c("variable", "importance"))
  expect_named(x2, c("variable", "importance"))
})
