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
})

test_that("prepare_importance_spatial() validates input model class", {
  data(plants_rf)

  # Non-spatial model should fail
  expect_error(
    prepare_importance_spatial(plants_rf),
    "rf_spatial"
  )
})

test_that("prepare_importance_spatial() output has correct structure", {
  data(plants_rf_spatial)

  result <- prepare_importance_spatial(plants_rf_spatial)

  # Data frames
  expect_s3_class(result$per.variable, "data.frame")
  expect_s3_class(result$spatial.predictors, "data.frame")
  expect_s3_class(result$spatial.predictors.stats, "data.frame")

  # Plots
  expect_s3_class(result$per.variable.plot, "ggplot")
  expect_s3_class(result$spatial.predictors.plot, "ggplot")
  expect_s3_class(result$spatial.predictors.stats.plot, "ggplot")
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
    all(result$spatial.predictors$variable[spatial_rows] == "spatial_predictors")
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

test_that("prepare_importance_spatial() preserves non-spatial predictors", {
  data(plants_rf_spatial)

  result <- prepare_importance_spatial(plants_rf_spatial)

  # Get non-spatial predictors from original model
  original_non_spatial <- plants_rf_spatial$importance$per.variable[
    !grepl(
      "spatial_predictor",
      plants_rf_spatial$importance$per.variable$variable
    ),
  ]

  # Check they're in spatial.predictors
  non_spatial_in_result <- result$spatial.predictors[
    !grepl("spatial_predictor", result$spatial.predictors$variable),
  ]

  expect_equal(nrow(non_spatial_in_result), nrow(original_non_spatial))
})

test_that("prepare_importance_spatial() all data frames have required columns", {
  data(plants_rf_spatial)

  result <- prepare_importance_spatial(plants_rf_spatial)

  # All importance data frames should have variable and importance columns
  expect_true("variable" %in% colnames(result$per.variable))
  expect_true("importance" %in% colnames(result$per.variable))

  expect_true("variable" %in% colnames(result$spatial.predictors))
  expect_true("importance" %in% colnames(result$spatial.predictors))

  expect_true("variable" %in% colnames(result$spatial.predictors.stats))
  expect_true("importance" %in% colnames(result$spatial.predictors.stats))
})

test_that("prepare_importance_spatial() importance values are numeric", {
  data(plants_rf_spatial)

  result <- prepare_importance_spatial(plants_rf_spatial)

  expect_type(result$per.variable$importance, "double")
  expect_type(result$spatial.predictors$importance, "double")
  expect_type(result$spatial.predictors.stats$importance, "double")
})

test_that("prepare_importance_spatial() produces valid plots", {
  data(plants_rf_spatial)

  result <- prepare_importance_spatial(plants_rf_spatial)

  # Each plot should have data
  expect_true(nrow(result$per.variable.plot$data) > 0)
  expect_true(nrow(result$spatial.predictors.plot$data) > 0)
  expect_true(nrow(result$spatial.predictors.stats.plot$data) > 0)
})

test_that("prepare_importance_spatial() spatial.predictors.stats has correct number of rows", {
  data(plants_rf_spatial)

  result <- prepare_importance_spatial(plants_rf_spatial)

  # Get number of non-spatial predictors
  n_non_spatial <- sum(
    !grepl(
      "spatial_predictor",
      plants_rf_spatial$importance$per.variable$variable
    )
  )

  # Should have: non-spatial predictors + 5 summary stats
  expect_equal(nrow(result$spatial.predictors.stats), n_non_spatial + 5)
})

test_that("prepare_importance_spatial() spatial.predictors has all rows", {
  data(plants_rf_spatial)

  result <- prepare_importance_spatial(plants_rf_spatial)

  # Should have same number of rows as original importance
  expect_equal(
    nrow(result$spatial.predictors),
    nrow(plants_rf_spatial$importance$per.variable)
  )
})

test_that("prepare_importance_spatial() handles models with many spatial predictors", {
  data(plants_rf_spatial)

  # plants_rf_spatial should have multiple spatial predictors
  n_spatial <- sum(
    grepl(
      "spatial_predictor",
      plants_rf_spatial$importance$per.variable$variable
    )
  )

  expect_true(n_spatial > 1)

  result <- prepare_importance_spatial(plants_rf_spatial)

  # All 6 components should be present
  expect_length(result, 6)
})

test_that("prepare_importance_spatial() stats are ordered by importance", {
  data(plants_rf_spatial)

  result <- prepare_importance_spatial(plants_rf_spatial)

  # spatial.predictors.stats should be ordered by importance (decreasing)
  expect_true(
    all(
      diff(result$spatial.predictors.stats$importance) <= 0
    )
  )
})
