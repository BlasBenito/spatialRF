test_that("`select_spatial_predictors_sequential()` works", {
  data("distance_matrix")
  data("plant_richness_df")

  data <- plant_richness_df
  dependent.variable.name <- "richness_species_vascular"
  predictor.variable.names <- colnames(plant_richness_df)[5:21]
  distance.matrix <- distance_matrix
  distance.thresholds <- c(0, 100, 1000)

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
    reference.moran.i = model$spatial.correlation.residuals$max.moran,
    n.cores = 1
  )

  selection <- select_spatial_predictors_sequential(
    data = data,
    dependent.variable.name = dependent.variable.name,
    predictor.variable.names = predictor.variable.names,
    distance.matrix = distance.matrix,
    distance.thresholds = distance.thresholds,
    spatial.predictors.df = spatial.predictors,
    spatial.predictors.ranking = spatial.predictors.ranked,
    n.cores = 1
  )

  expect_type(selection, "list")
  expect_length(selection, 2)
  expect_named(selection, c("optimization", "best.spatial.predictors"))
  expect_equal(sum(selection$optimization$selected), length(selection$best.spatial.predictors))
})
