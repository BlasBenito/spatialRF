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

test_that("rf_tuning() works with basic parameters", {
  data(plants_df, plants_xy)

  model <- rf(
    data = plants_df[1:50, ],
    dependent.variable.name = "richness_species_vascular",
    predictor.variable.names = colnames(plants_df)[5:8],
    verbose = FALSE,
    n.cores = 1,
    seed = 1
  )

  result <- rf_tuning(
    model = model,
    xy = plants_xy[1:50, ],
    num.trees = 50,
    mtry = 2,
    min.node.size = 5,
    repetitions = 5,
    verbose = FALSE,
    n.cores = 1,
    seed = 1
  )

  expect_true("tuning" %in% names(result))
  expect_s3_class(result$tuning$tuning.df, "data.frame")
  expect_true("num.trees" %in% colnames(result$tuning$tuning.df))
  expect_true("mtry" %in% colnames(result$tuning$tuning.df))
  expect_true("min.node.size" %in% colnames(result$tuning$tuning.df))
})

test_that("rf_tuning() includes spatial predictors in tuned rf_spatial models", {
  # Temporarily increase future.globals.maxSize for this test
  old_size <- getOption("future.globals.maxSize")
  options(future.globals.maxSize = 1000 * 1024^2)  # 1000 MiB
  on.exit(options(future.globals.maxSize = old_size))

  data(plants_df, plants_distance, plants_xy)

  # Use small subset for speed
  idx <- 1:50
  plants_df_sub <- plants_df[idx, ]
  plants_distance_sub <- plants_distance[idx, idx]
  plants_xy_sub <- plants_xy[idx, ]

  # Fit non-spatial model first
  model_nonspatial <- rf(
    data = plants_df_sub,
    dependent.variable.name = "richness_species_vascular",
    predictor.variable.names = colnames(plants_df)[5:8],
    distance.matrix = plants_distance_sub,
    distance.thresholds = c(0, 1000),
    xy = plants_xy_sub,
    verbose = FALSE,
    n.cores = 1,
    seed = 1
  )

  # Convert to spatial model
  model_spatial <- rf_spatial(
    model = model_nonspatial,
    method = "mem.moran.sequential",
    verbose = FALSE,
    n.cores = 1
  )

  # Tune the spatial model with simpler parameter grid
  result <- rf_tuning(
    model = model_spatial,
    xy = plants_xy_sub,
    num.trees = 50,
    mtry = c(2, 3),
    min.node.size = 5,
    repetitions = 5,  # Minimum required by rf_evaluate
    verbose = FALSE,
    n.cores = 1,
    seed = 1
  )

  # CRITICAL TEST: Verify spatial predictors are included
  # Check that spatial predictor names from original model are in tuned model
  original_spatial_names <- model_spatial$spatial$names
  tuned_predictor_names <- result$ranger.arguments$predictor.variable.names

  # Test that ALL original spatial predictors are in the tuned model
  spatial_predictors_included <- all(original_spatial_names %in% tuned_predictor_names)

  expect_true(
    spatial_predictors_included,
    info = paste(
      "Spatial predictors from original model missing in tuned model.",
      "Original spatial predictors:", paste(original_spatial_names, collapse = ", "),
      "Tuned model predictors:", paste(tuned_predictor_names, collapse = ", ")
    )
  )

  # Also verify the tuned model has the spatial slot
  expect_true(inherits(result, "rf_spatial"))
  expect_true("spatial" %in% names(result))
  expect_equal(result$spatial$names, original_spatial_names)
})

test_that("rf_tuning() mtry calculation accounts for spatial predictors", {
  # Temporarily increase future.globals.maxSize for this test
  old_size <- getOption("future.globals.maxSize")
  options(future.globals.maxSize = 1000 * 1024^2)  # 1000 MiB
  on.exit(options(future.globals.maxSize = old_size))

  data(plants_df, plants_distance, plants_xy)

  # Use larger subset to ensure spatial predictors can be generated
  idx <- 1:100
  plants_df_sub <- plants_df[idx, ]
  plants_distance_sub <- plants_distance[idx, idx]
  plants_xy_sub <- plants_xy[idx, ]

  # Fit spatial model with known number of spatial predictors
  model_nonspatial <- rf(
    data = plants_df_sub,
    dependent.variable.name = "richness_species_vascular",
    predictor.variable.names = colnames(plants_df)[5:10],  # 6 predictors
    distance.matrix = plants_distance_sub,
    distance.thresholds = c(0),
    xy = plants_xy_sub,
    verbose = FALSE,
    n.cores = 1,
    seed = 1
  )

  model_spatial <- rf_spatial(
    model = model_nonspatial,
    method = "mem.moran.sequential",
    max.spatial.predictors = 4,  # Force exactly 4 spatial predictors
    verbose = FALSE,
    n.cores = 1
  )

  # Count total predictors: should be 4 original + up to 3 spatial = 7 max
  num_original <- length(model_nonspatial$ranger.arguments$predictor.variable.names)
  num_spatial <- length(model_spatial$spatial$names)
  total_predictors <- num_original + num_spatial

  # Tune with mtry = NULL (should auto-generate based on total predictors)
  result <- rf_tuning(
    model = model_spatial,
    xy = plants_xy_sub,
    num.trees = 50,
    mtry = NULL,  # Should auto-detect including spatial predictors
    min.node.size = 5,
    repetitions = 5,  # Minimum required by rf_evaluate
    verbose = FALSE,
    n.cores = 1,
    seed = 1
  )

  # The maximum mtry value should equal total_predictors (not just original predictors)
  max_mtry_tested <- max(result$tuning$tuning.df$mtry)

  expect_equal(
    max_mtry_tested,
    total_predictors,
    info = paste(
      "mtry max should equal total predictors (original + spatial).",
      "Expected:", total_predictors,
      "Got:", max_mtry_tested,
      "Original predictors:", num_original,
      "Spatial predictors:", num_spatial
    )
  )
})
