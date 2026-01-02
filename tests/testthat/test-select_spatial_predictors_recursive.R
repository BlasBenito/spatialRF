test_that("`select_spatial_predictors_recursive()` works", {
  data("plants_distance")
  data("plants_df")

  # Use smaller subset for faster testing of this expensive recursive
  sample_idx <- 1:50
  data <- plants_df[sample_idx, ]
  distance.matrix <- plants_distance[sample_idx, sample_idx]

  # Use fewer predictors (6 instead of 17) for faster testing
  dependent.variable.name <- "richness_species_vascular"
  predictor.variable.names <- colnames(plants_df)[5:10]

  # Use single distance threshold for speed (matches function documentation example)
  distance.thresholds <- 0

  model <- rf(
    data = data,
    dependent.variable.name = dependent.variable.name,
    predictor.variable.names = predictor.variable.names,
    distance.matrix = distance.matrix,
    distance.thresholds = distance.thresholds,
    verbose = FALSE
  )
  spatial.predictors <- pca_multithreshold(
    distance.matrix,
    distance.thresholds = distance.thresholds
  )

  spatial.predictors.ranking <- rank_spatial_predictors(
    data = data,
    dependent.variable.name = dependent.variable.name,
    predictor.variable.names = predictor.variable.names,
    distance.matrix = distance.matrix,
    distance.thresholds = distance.thresholds,
    spatial.predictors.df = spatial.predictors,
    ranking.method = "effect",
    reference.moran.i = model$spatial.correlation.residuals$max.moran
  )

  selection <- select_spatial_predictors_recursive(
    data = data,
    dependent.variable.name = dependent.variable.name,
    predictor.variable.names = predictor.variable.names,
    distance.matrix = distance.matrix,
    distance.thresholds = distance.thresholds,
    spatial.predictors.df = spatial.predictors,
    spatial.predictors.ranking = spatial.predictors.ranking
  )

  expect_type(selection, "list")
  expect_s3_class(selection$optimization, "data.frame")
  expect_type(selection$best.spatial.predictors, "character")
  expect_length(selection, 2)
  expect_named(selection, c("optimization", "best.spatial.predictors"))
  expect_named(
    selection$optimization,
    c(
      "spatial.predictor.name",
      "spatial.predictor.index",
      "moran.i",
      "p.value",
      "p.value.binary",
      "r.squared",
      "penalization.per.variable",
      "optimization",
      "selected"
    )
  )
})

test_that("select_spatial_predictors_recursive() evaluates all spatial predictors", {
  data("plants_distance")
  data("plants_df")

  # Use very small subset to force limited spatial predictors
  sample_idx <- 1:40
  data <- plants_df[sample_idx, ]
  distance.matrix <- plants_distance[sample_idx, sample_idx]

  dependent.variable.name <- "richness_species_vascular"
  predictor.variable.names <- colnames(plants_df)[5:7] # Only 3 predictors
  distance.thresholds <- 0

  model <- rf(
    data = data,
    dependent.variable.name = dependent.variable.name,
    predictor.variable.names = predictor.variable.names,
    distance.matrix = distance.matrix,
    distance.thresholds = distance.thresholds,
    verbose = FALSE
  )

  # Generate limited spatial predictors
  spatial.predictors <- pca_multithreshold(
    distance.matrix,
    distance.thresholds = distance.thresholds,
    max.spatial.predictors = 3 # Force small number to test loop termination
  )

  spatial.predictors.ranking <- rank_spatial_predictors(
    data = data,
    dependent.variable.name = dependent.variable.name,
    predictor.variable.names = predictor.variable.names,
    distance.matrix = distance.matrix,
    distance.thresholds = distance.thresholds,
    spatial.predictors.df = spatial.predictors,
    ranking.method = "effect",
    reference.moran.i = model$spatial.correlation.residuals$max.moran
  )

  # CRITICAL TEST: Verify loop doesn't exit early
  # The bug was loop termination at > 1 instead of > 0
  # This meant the last spatial predictor was never evaluated
  selection <- select_spatial_predictors_recursive(
    data = data,
    dependent.variable.name = dependent.variable.name,
    predictor.variable.names = predictor.variable.names,
    distance.matrix = distance.matrix,
    distance.thresholds = distance.thresholds,
    spatial.predictors.df = spatial.predictors,
    spatial.predictors.ranking = spatial.predictors.ranking
  )

  # Count how many spatial predictors were available
  num_available <- ncol(spatial.predictors)

  # Verify that the optimization data frame has entries for ALL available predictors
  # Not just num_available - 1 (which would happen with the > 1 bug)
  expect_s3_class(selection$optimization, "data.frame")
  expect_true(
    nrow(selection$optimization) >= num_available,
    info = paste(
      "Recursive selection should evaluate all spatial predictors.",
      "Available predictors:",
      num_available,
      "Evaluated predictors:",
      nrow(selection$optimization)
    )
  )

  # Verify that at least some spatial predictors were selected
  expect_true(length(selection$best.spatial.predictors) > 0)
})
