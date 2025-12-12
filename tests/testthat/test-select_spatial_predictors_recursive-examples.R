test_that("`select_spatial_predictors_recursive()` works", {
  data("distance_matrix")
  data("plant_richness_df")

  # Use smaller subset for faster testing of this expensive recursive
  sample_idx <- 1:50
  data <- plant_richness_df[sample_idx, ]
  distance.matrix <- distance_matrix[sample_idx, sample_idx]

  # Use fewer predictors (6 instead of 17) for faster testing
  dependent.variable.name <- "richness_species_vascular"
  predictor.variable.names <- colnames(plant_richness_df)[5:10]

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

  library(tictoc)

  #no cluster
  tic()
  spatial.predictors.ranking <- rank_spatial_predictors(
    data = data,
    dependent.variable.name = dependent.variable.name,
    predictor.variable.names = predictor.variable.names,
    distance.matrix = distance.matrix,
    distance.thresholds = distance.thresholds,
    spatial.predictors.df = spatial.predictors,
    ranking.method = "effect",
    reference.moran.i = model$spatial.correlation.residuals$max.moran,
    n.cores = 1,
    cluster = NULL
  )
  toc()

  #internal cluster
  tic()
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
  toc()

  #external cluster
  cluster <- parallel::makeCluster(
    parallel::detectCores() - 1,
    type = "PSOCK"
  )

  t
  spatial.predictors.ranking <- rank_spatial_predictors(
    data = data,
    dependent.variable.name = dependent.variable.name,
    predictor.variable.names = predictor.variable.names,
    distance.matrix = distance.matrix,
    distance.thresholds = distance.thresholds,
    spatial.predictors.df = spatial.predictors,
    ranking.method = "effect",
    reference.moran.i = model$spatial.correlation.residuals$max.moran,
    cluster = cluster
  )

  selection <- select_spatial_predictors_recursive(
    data = data,
    dependent.variable.name = dependent.variable.name,
    predictor.variable.names = predictor.variable.names,
    distance.matrix = distance.matrix,
    distance.thresholds = distance.thresholds,
    spatial.predictors.df = spatial.predictors,
    spatial.predictors.ranking = spatial.predictors.ranking,
    cluster = cluster
  )

  foreach::registerDoSEQ()
  parallel::stopCluster(cluster)
  invisible(gc())

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
