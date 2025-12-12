test_that("`rf_evaluate()` works", {
  data(plant_richness_df)
  data(distance_matrix)

  cluster <- parallel::makeCluster(
    parallel::detectCores() - 1,
    type = "PSOCK"
  )

  rf.model <- rf(
    data = plant_richness_df,
    dependent.variable.name = "richness_species_vascular",
    predictor.variable.names = colnames(plant_richness_df)[5:21],
    distance.matrix = distance_matrix,
    distance.thresholds = c(
      0,
      1000,
      2000
    ),
    verbose = FALSE
  )

  rf.model <- rf_evaluate(
    model = rf.model,
    xy = plant_richness_df[, c("x", "y")],
    verbose = FALSE,
    cluster = cluster
  )

  parallel::stopCluster(cluster)

  expect_s3_class(rf.model, "rf_evaluate")
  expect_type(rf.model$evaluation, "list")
  expect_s3_class(rf.model$evaluation$per.fold, "data.frame")
  expect_s3_class(rf.model$evaluation$per.model, "data.frame")
  expect_s3_class(rf.model$evaluation$aggregated, "data.frame")
})
