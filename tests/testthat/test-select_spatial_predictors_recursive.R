test_that("`select_spatial_predictors_recursive()` works", {
  data(plants_rf, plants_distance, plants_df, plants_predictors)

  distance.thresholds <- 1000

  spatial.predictors <- pca_multithreshold(
    distance.matrix = plants_distance,
    distance.thresholds = distance.thresholds
  )

  spatial.predictors <- spatial.predictors[, 1:10]

  spatial.predictors.ranking <- rank_spatial_predictors(
    data = plants_df,
    dependent.variable.name = "richness_species_vascular",
    predictor.variable.names = plants_rf$ranger.arguments$predictor.variable.names,
    distance.matrix = plants_distance,
    distance.thresholds = distance.thresholds,
    spatial.predictors.df = spatial.predictors,
    ranking.method = "effect",
    reference.moran.i = plants_df$spatial.correlation.residuals$max.moran
  )

  selection <- select_spatial_predictors_recursive(
    data = plants_df,
    dependent.variable.name = "richness_species_vascular",
    predictor.variable.names = plants_rf$ranger.arguments$predictor.variable.names,
    distance.matrix = plants_distance,
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
