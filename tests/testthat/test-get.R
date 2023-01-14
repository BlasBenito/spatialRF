testthat::test_that("`get_xxx()` works", {

  library(spatialRF)
  library(magrittr)

  #loading example data
  data(
    ecoregions_df,
    ecoregions_distance_matrix,
    ecoregions_numeric_predictors,
    ecoregions_continuous_response
    )

  ecoregions_df <- tibble::as_tibble(ecoregions_df)

  distance.thresholds <- c(0, 100, 1000)

   #fitting random forest model
   model <- rf(
     data = ecoregions_df,
     dependent.variable.name = ecoregions_continuous_response,
     predictor.variable.names = ecoregions_numeric_predictors,
     distance.matrix = ecoregions_distance_matrix,
     distance.thresholds = distance.thresholds,
     xy = ecoregions_df[, c("x", "y")],
     n.cores = 1,
     verbose = FALSE
   )

  #evaluating the model with spatial cross-validation
   model <- rf_evaluate(
    model = model,
    xy = ecoregions_df[, c("x", "y")],
    n.cores = 1,
    verbose = FALSE
  )

  #testing get_evaluation_aggregated
  ##################################
  x <- get_evaluation_aggregated(model = model)

  testthat::expect_equal(
    object = class(x),
    expected = "data.frame"
  )

  testthat::expect_equal(
    object = names(x),
    expected = c(
      "model",
      "metric",
      "median",
      "median_absolute_deviation",
      "q1",
      "q3",
      "mean",
      "se",
      "sd",
      "min",
      "max"
    )
  )

  #testing get_evaluation_per_fold
  ##################################
  x <- get_evaluation_per_fold(model = model)

  testthat::expect_equal(
    object = class(x),
    expected = "data.frame"
  )

  testthat::expect_equal(
    object = names(x),
    expected = c(
      "fold.id",
      "fold.center.x",
      "fold.center.y",
      "training.records",
      "testing.records",
      "training.r.squared",
      "testing.r.squared",
      "training.rmse",
      "testing.rmse",
      "training.nrmse",
      "testing.nrmse"
    )
  )


  #testing get_evaluation_folds
  ##################################
  x <- get_evaluation_folds(model = model)

  testthat::expect_equal(
    object = class(x),
    expected = "list"
  )

  #testing get_importance
  ##################################
  x <- get_importance(model = model)

  testthat::expect_equal(
    object = class(x),
    expected = "data.frame"
  )

  testthat::expect_equal(
    object = nrow(x),
    expected = length(ecoregions_numeric_predictors)
  )

  #testing get_importance_local
  ##################################
  x <- get_importance_local(model = model)

  testthat::expect_equal(
    object = inherits(x = x, what = "matrix"),
    expected = TRUE
  )

  testthat::expect_equal(
    object = nrow(x),
    expected = nrow(ecoregions_df)
  )

  #testing get_moran
  ##################################
  x <- get_moran(model = model)

  testthat::expect_equal(
    object = class(x),
    expected = "data.frame"
  )

  testthat::expect_equal(
    object = nrow(x),
    expected = length(distance.thresholds)
  )

  #testing get_residuals
  ##################################
  x <- get_residuals(model = model)

  testthat::expect_equal(
    object = class(x),
    expected = "numeric"
  )

  testthat::expect_equal(
    object = length(x),
    expected = nrow(ecoregions_df)
  )

  #testing get_performance
  ##################################
  x <- get_performance(model = model)

  testthat::expect_equal(
    object = class(x),
    expected = "data.frame"
  )

  testthat::expect_equal(
    object = nrow(x),
    expected = 5
  )

  #testing get_predictions
  ##################################
  x <- get_predictions(model = model)

  testthat::expect_equal(
    object = class(x),
    expected = "numeric"
  )

  testthat::expect_equal(
    object = length(x),
    expected = nrow(ecoregions_df)
  )

  #of rf repeat model
  model.repeat <- rf_repeat(
    model = model,
    repetitions = 5,
    verbose = FALSE
  )

  x <- get_predictions(model = model.repeat)

  testthat::expect_equal(
    object = ncol(x),
    expected = 5
  )

  testthat::expect_equal(
    object = nrow(x),
    expected = nrow(ecoregions_df)
  )

  #testing get_spatial_predictors
  ##################################
  model.spatial <- rf_spatial(
    model = model,
    verbose = FALSE
    )

  x <- get_spatial_predictors(model = model.spatial)

  testthat::expect_equal(
    object = class(x),
    expected = "data.frame"
  )

  testthat::expect_equal(
    object = nrow(x),
    expected = nrow(ecoregions_df)
  )

  #testing get_jackknife
  ###############################
  model <- rf(
    model = model,
    verbose = FALSE
  )

  model <- rf_jackknife(
    model = model,
    verbose = FALSE
    )

  x <- get_jackknife(model = model)

  testthat::expect_equal(
    object = class(x),
    expected = "data.frame"
  )

  #testing get_select
  ###############################
  model <- rf_select(
    model = model,
    verbose = FALSE,
    cluster = start_cluster()
  )

  stop_cluster()

  x <- get_select(model = model)

  testthat::expect_equal(
    object = class(x),
    expected = "data.frame"
  )

})
