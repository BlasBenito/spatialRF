testthat::test_that("`select_spatial_predictors_recursive()` works", {

  library(spatialRF)
  library(magrittr)

  #loading data
  data(
    ecoregions_df,
    ecoregions_distance_matrix,
    ecoregions_predictor_variable_names,
    ecoregions_dependent_variable_name
  )

  distance.thresholds = c(0, 100)

  ecoregions_df <- tibble::as_tibble(ecoregions_df)

  cluster <- start_cluster()

  model <- rf(
    data = ecoregions_df,
    dependent.variable.name = ecoregions_dependent_variable_name,
    predictor.variable.names = ecoregions_predictor_variable_names,
    distance.matrix = ecoregions_distance_matrix,
    xy = ecoregions_df[, c("x", "y")],
    distance.thresholds = distance.thresholds,
    scaled.importance = FALSE,
    verbose = FALSE
  )

  spatial.predictors <- mem_multithreshold(
    distance.matrix = ecoregions_distance_matrix,
    distance.thresholds = distance.thresholds
  )

  spatial.predictors.ranking <- rank_spatial_predictors(
    data = ecoregions_df,
    dependent.variable.name = ecoregions_dependent_variable_name,
    predictor.variable.names = ecoregions_predictor_variable_names,
    distance.matrix = ecoregions_distance_matrix,
    distance.thresholds = distance.thresholds,
    spatial.predictors.df = spatial.predictors,
    ranking.method = "moran",
    reference.moran.i = model$spatial.correlation.residuals$max.moran,
    cluster = cluster
  )

  selection <- select_spatial_predictors_sequential(
    data = ecoregions_df,
    dependent.variable.name = ecoregions_dependent_variable_name,
    predictor.variable.names = ecoregions_predictor_variable_names,
    distance.matrix = ecoregions_distance_matrix,
    distance.thresholds = distance.thresholds,
    spatial.predictors.df = spatial.predictors,
    spatial.predictors.ranking = spatial.predictors.ranking,
    cluster = cluster
  )

  stop_cluster(cluster = cluster)

  testthat::expect_type(selection, "list")
  testthat::expect_s3_class(selection$optimization, "data.frame")
  testthat::expect_type(selection$best.spatial.predictors, "character")
  testthat::expect_length(selection, 2)
  testthat::expect_named(selection, c("optimization", "best.spatial.predictors"))
  testthat::expect_named(selection$optimization, c("spatial.predictor.name", "spatial.predictor.index", "moran.i", "p.value", "p.value.binary", "r.squared", "penalization.per.variable", "optimization", "selected"))

})

