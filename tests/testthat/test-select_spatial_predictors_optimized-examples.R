test_that("`select_spatial_predictors_optimized()` works", {
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
    distance.thresholds = distance.thresholds
  )
  spatial.predictors <- pca_multithreshold(
    x = distance.matrix,
    distance.thresholds = distance.thresholds
  )
  spatial.predictors.ranking <- rank_spatial_predictors(
    data = data,
    dependent.variable.name = dependent.variable.name,
    predictor.variable.names = predictor.variable.names,
    spatial.predictors.df = spatial.predictors,
    ranking.method = "moran.i.reduction",
    reference.moran.i = model$spatial.correlation.residuals$max.moran,
    distance.matrix = distance.matrix,
    distance.thresholds = distance.thresholds,
    n.cores = 1,
    multicollinearity.filter = "none"
  )
  selection <- select_spatial_predictors_optimized(
    data = data,
    dependent.variable.name = dependent.variable.name,
    predictor.variable.names = predictor.variable.names,
    distance.matrix = distance.matrix,
    distance.thresholds = distance.thresholds,
    spatial.predictors.df = spatial.predictors,
    spatial.predictors.ranking = spatial.predictors.ranking,
    n.cores = 1
  )
  expect_type(selection, "list")
  expect_s3_class(selection$optimization, "data.frame")
  expect_type(selection$best.spatial.predictors, "character")
  expect_length(selection, 2)
  expect_named(selection, c("optimization", "best.spatial.predictors"))
  expect_named(selection$optimization, c("spatial.predictors.index", "spatial.predictors.name", "moran.i", "r.squared", "sum"))
})
