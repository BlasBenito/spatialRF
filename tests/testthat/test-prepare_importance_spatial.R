test_that("prepare_importance_spatial() works with basic inputs", {
  data(plants_rf_spatial)

  result <- prepare_importance_spatial(plants_rf_spatial)

  expect_type(result, "list")
  expect_named(
    result,
    c(
      "per.variable",
      "per.variable.plot",
      "spatial.predictors",
      "spatial.predictors.plot",
      "spatial.predictors.stats",
      "spatial.predictors.stats.plot"
    )
  )

  # Data frames
  expect_s3_class(result$per.variable, "data.frame")
  expect_s3_class(result$spatial.predictors, "data.frame")
  expect_s3_class(result$spatial.predictors.stats, "data.frame")

  # Plots
  expect_s3_class(result$per.variable.plot, "ggplot")
  expect_s3_class(result$spatial.predictors.plot, "ggplot")
  expect_s3_class(result$spatial.predictors.stats.plot, "ggplot")
})

test_that("prepare_importance_spatial() validates input model class", {
  data(plants_rf)

  # Non-spatial model should fail
  expect_error(
    prepare_importance_spatial(plants_rf),
    "rf_spatial"
  )
})


test_that("prepare_importance_spatial() per.variable matches model", {
  data(plants_rf_spatial)

  result <- prepare_importance_spatial(plants_rf_spatial)

  # per.variable should match the model's importance
  expect_equal(
    result$per.variable,
    plants_rf_spatial$importance$per.variable
  )
})

test_that("prepare_importance_spatial() groups spatial predictors", {
  data(plants_rf_spatial)

  result <- prepare_importance_spatial(plants_rf_spatial)

  # spatial.predictors should have spatial predictors grouped
  spatial_rows <- grepl(
    "spatial_predictor",
    result$spatial.predictors$variable
  )

  # All spatial predictors should be labeled as "spatial_predictors"
  expect_true(
    all(
      result$spatial.predictors$variable[spatial_rows] == "spatial_predictors"
    )
  )
})

test_that("prepare_importance_spatial() computes summary statistics", {
  data(plants_rf_spatial)

  result <- prepare_importance_spatial(plants_rf_spatial)

  # spatial.predictors.stats should have summary rows
  expected_vars <- c(
    "spatial_predictors (max)",
    "spatial_predictors (min)",
    "spatial_predictors (median)",
    "spatial_predictors (quantile 0.25)",
    "spatial_predictors (quantile 0.75)"
  )

  stat_vars <- result$spatial.predictors.stats$variable[
    grepl("spatial_predictors", result$spatial.predictors.stats$variable)
  ]

  expect_true(all(expected_vars %in% stat_vars))
})
