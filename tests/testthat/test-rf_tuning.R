test_that("`rf_tuning()` works", {
  data(plants_rf)
  data(plants_xy)

  out <- rf_tuning(
    model = plants_rf,
    xy = plants_xy,
    num.trees = c(100),
    mtry = c(2),
    min.node.size = c(5, 10),
    verbose = FALSE
  )

  expect_s3_class(out$tuning$tuning.df, "data.frame")
  expect_type(out$tuning, "list")
})


test_that("rf_tuning() validates model parameter", {
  # NULL model
  expect_error(
    rf_tuning(model = NULL, xy = data.frame(x = 1, y = 1)),
    "argument 'model' is empty"
  )
})

test_that("rf_tuning() validates xy column names", {
  data(plants_rf, plants_xy)

  # Wrong column names
  xy_wrong <- plants_xy
  names(xy_wrong) <- c("longitude", "latitude")

  expect_error(
    rf_tuning(
      model = plants_rf,
      xy = xy_wrong,
      verbose = FALSE
    ),
    "column names of 'xy' must be 'x' and 'y'"
  )
})

test_that("rf_tuning() validates xy row count", {
  data(plants_rf, plants_xy)

  # Different number of rows
  xy_subset <- plants_xy[1:100, ]

  expect_error(
    rf_tuning(
      model = plants_rf,
      xy = xy_subset,
      verbose = FALSE
    ),
    "nrow\\(xy\\) and nrow\\(data\\)"
  )
})


test_that("rf_tuning() includes spatial predictors in tuned rf_spatial models", {
  data(plants_rf_spatial, plants_xy)

  # Tune the spatial model with simpler parameter grid
  result <- rf_tuning(
    model = plants_rf_spatial,
    xy = plants_xy,
    num.trees = 50,
    mtry = c(2, 3),
    min.node.size = 5,
    repetitions = 5, # Minimum required by rf_evaluate
    verbose = FALSE
  )

  # CRITICAL TEST: Verify spatial predictors are included
  # Check that spatial predictor names from original model are in tuned model
  original_spatial_names <- plants_rf_spatial$spatial$names
  tuned_predictor_names <- result$ranger.arguments$predictor.variable.names

  # Test that ALL original spatial predictors are in the tuned model
  spatial_predictors_included <- all(
    original_spatial_names %in% tuned_predictor_names
  )

  expect_true(
    spatial_predictors_included,
    info = paste(
      "Spatial predictors from original model missing in tuned model.",
      "Original spatial predictors:",
      paste(original_spatial_names, collapse = ", "),
      "Tuned model predictors:",
      paste(tuned_predictor_names, collapse = ", ")
    )
  )

  # Also verify the tuned model has the spatial slot
  expect_true(inherits(result, "rf_spatial"))
  expect_true("spatial" %in% names(result))
  expect_equal(result$spatial$names, original_spatial_names)
})
