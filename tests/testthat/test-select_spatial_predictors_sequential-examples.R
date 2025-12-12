test_that("`select_spatial_predictors_sequential()` works", {
  data("distance_matrix")
  data("plant_richness_df")

  # Use smaller subset for faster testing
  # Sample 70 rows to keep test fast while maintaining validity
  sample_idx <- 1:100
  data <- plant_richness_df[sample_idx, ]
  distance.matrix <- distance_matrix[sample_idx, sample_idx]

  # Use fewer predictors (7 instead of 17) for faster testing
  dependent.variable.name <- "richness_species_vascular"
  predictor.variable.names <- colnames(plant_richness_df)[5:11]

  # Use single distance threshold for speed
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

  spatial.predictors.ranked <- rank_spatial_predictors(
    distance.matrix = distance.matrix,
    distance.thresholds = distance.thresholds,
    spatial.predictors.df = spatial.predictors,
    ranking.method = "moran",
    reference.moran.i = model$spatial.correlation.residuals$max.moran
  )

  selection <- select_spatial_predictors_sequential(
    data = data,
    dependent.variable.name = dependent.variable.name,
    predictor.variable.names = predictor.variable.names,
    distance.matrix = distance.matrix,
    distance.thresholds = distance.thresholds,
    spatial.predictors.df = spatial.predictors,
    spatial.predictors.ranking = spatial.predictors.ranked
  )

  expect_type(selection, "list")
  expect_length(selection, 2)
  expect_named(selection, c("optimization", "best.spatial.predictors"))
  expect_equal(
    sum(selection$optimization$selected),
    length(selection$best.spatial.predictors)
  )
})
