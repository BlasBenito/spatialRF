test_that("rf_compare() works with basic inputs", {
  data(plants_rf, plants_rf_spatial, plants_xy)

  comparison <- rf_compare(
    models = list(
      `Non spatial` = plants_rf,
      Spatial = plants_rf_spatial
    ),
    xy = plants_xy,
    repetitions = 5,
    metrics = "rmse",
    verbose = FALSE
  )

  expect_type(comparison, "list")
  expect_named(comparison, c("comparison.df", "spatial.folds", "plot"))
  expect_s3_class(comparison$comparison.df, "data.frame")
  expect_s3_class(comparison$plot, "ggplot")
  expect_true(nrow(comparison$comparison.df) > 0)
  expect_equal(length(comparison$spatial.folds), 5)
})

test_that("rf_compare() handles multiple metrics", {
  data(plants_rf, plants_rf_spatial, plants_xy)

  comparison <- rf_compare(
    models = list(
      a = plants_rf,
      b = plants_rf_spatial
    ),
    xy = plants_xy,
    repetitions = 5,
    metrics = c("rmse", "r.squared"),
    verbose = FALSE
  )

  expect_true(all(
    c("rmse", "r.squared") %in% unique(comparison$comparison.df$metric)
  ))
  expect_equal(length(unique(comparison$comparison.df$metric)), 2)
})

test_that("rf_compare() works with more than two models", {
  data(plants_rf, plants_rf_spatial, plants_xy)

  # Create a third model (simple variant)
  plants_rf_subset <- plants_rf
  plants_rf_subset$ranger.arguments$num.trees <- 50

  comparison <- rf_compare(
    models = list(
      model1 = plants_rf,
      model2 = plants_rf_spatial,
      model3 = plants_rf_subset
    ),
    xy = plants_xy,
    repetitions = 5,
    metrics = "rmse",
    verbose = FALSE
  )

  expect_equal(length(unique(comparison$comparison.df$model)), 3)
  expect_true(all(
    c("model1", "model2", "model3") %in% unique(comparison$comparison.df$model)
  ))
})

test_that("rf_compare() comparison.df has correct structure", {
  data(plants_rf, plants_rf_spatial, plants_xy)

  comparison <- rf_compare(
    models = list(
      a = plants_rf,
      b = plants_rf_spatial
    ),
    xy = plants_xy,
    repetitions = 5,
    metrics = "rmse",
    verbose = FALSE
  )

  expect_named(comparison$comparison.df, c("metric", "value", "model"))
  expect_type(comparison$comparison.df$metric, "character")
  expect_type(comparison$comparison.df$value, "double")
  expect_type(comparison$comparison.df$model, "character")
  expect_true(all(!is.na(comparison$comparison.df$value)))
})

test_that("rf_compare() respects seed parameter", {
  data(plants_rf, plants_rf_spatial, plants_xy)

  comparison1 <- rf_compare(
    models = list(a = plants_rf, b = plants_rf_spatial),
    xy = plants_xy,
    repetitions = 5,
    metrics = "rmse",
    verbose = FALSE
  )

  comparison2 <- rf_compare(
    models = list(a = plants_rf, b = plants_rf_spatial),
    xy = plants_xy,
    repetitions = 5,
    metrics = "rmse",
    verbose = FALSE
  )

  expect_equal(
    comparison1$comparison.df$value,
    comparison2$comparison.df$value
  )
})

test_that("rf_compare() respects repetitions parameter", {
  data(plants_rf, plants_rf_spatial, plants_xy)

  comparison <- rf_compare(
    models = list(a = plants_rf, b = plants_rf_spatial),
    xy = plants_xy,
    repetitions = 5,
    metrics = "rmse",
    verbose = FALSE
  )

  # Each model gets evaluated on 'repetitions' folds
  # So total rows = repetitions * number of models
  expect_equal(nrow(comparison$comparison.df), 5 * 2)
  expect_equal(length(comparison$spatial.folds), 5)
})

test_that("rf_compare() validates models input", {
  data(plants_xy)

  expect_error(
    rf_compare(
      models = NULL,
      xy = plants_xy,
      verbose = FALSE
    ),
    "no models to compare"
  )
})

test_that("rf_compare() validates xy input", {
  data(plants_rf)

  expect_error(
    rf_compare(
      models = list(a = plants_rf),
      xy = NULL,
      verbose = FALSE
    ),
    "coordinates 'xy' is missing"
  )
})

test_that("rf_compare() validates metrics parameter", {
  data(plants_rf, plants_rf_spatial, plants_xy)

  expect_error(
    rf_compare(
      models = list(a = plants_rf, b = plants_rf_spatial),
      xy = plants_xy,
      metrics = "invalid_metric",
      verbose = FALSE
    ),
    "arg"
  )
})

test_that("rf_compare() plot uses correct metric labels", {
  data(plants_rf, plants_rf_spatial, plants_xy)

  comparison <- rf_compare(
    models = list(a = plants_rf, b = plants_rf_spatial),
    xy = plants_xy,
    repetitions = 5,
    metrics = c("r.squared", "rmse"),
    verbose = FALSE
  )

  # Plot data should have pretty labels
  plot_data <- comparison$plot$data
  expect_true(any(grepl("R squared|RMSE", plot_data$metric)))
})

test_that("rf_compare() works with all metrics", {
  data(plants_rf, plants_rf_spatial, plants_xy)

  comparison <- rf_compare(
    models = list(a = plants_rf, b = plants_rf_spatial),
    xy = plants_xy,
    repetitions = 5,
    metrics = c("r.squared", "pseudo.r.squared", "rmse", "nrmse"),
    verbose = FALSE
  )

  expect_equal(
    length(unique(comparison$comparison.df$metric)),
    4
  )
  expect_true(
    all(
      c("r.squared", "pseudo.r.squared", "rmse", "nrmse") %in%
        unique(comparison$comparison.df$metric)
    )
  )
})

test_that("rf_compare() spatial.folds has correct structure", {
  data(plants_rf, plants_rf_spatial, plants_xy)

  comparison <- rf_compare(
    models = list(a = plants_rf, b = plants_rf_spatial),
    xy = plants_xy,
    repetitions = 5,
    metrics = "rmse",
    verbose = FALSE
  )

  expect_s3_class(comparison$spatial.folds, "data.frame")
  expect_equal(ncol(comparison$spatial.folds), 5)
  expect_equal(nrow(comparison$spatial.folds), nrow(plants_df))

  # Each fold should be a logical vector with training and testing records
  for (i in seq_len(ncol(comparison$spatial.folds))) {
    expect_type(comparison$spatial.folds[[i]], "logical")
    expect_true(sum(comparison$spatial.folds[[i]]) > 0) # Has training records
    expect_true(sum(!comparison$spatial.folds[[i]]) > 0) # Has testing records
  }
})
