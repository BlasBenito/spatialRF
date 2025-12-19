test_that("`rf_evaluate()` works", {
  data(plants_df)
  data(plants_distance)

  cluster <- parallel::makeCluster(
    parallel::detectCores() - 1,
    type = "PSOCK"
  )

  rf.model <- rf(
    data = plants_df,
    dependent.variable.name = "richness_species_vascular",
    predictor.variable.names = colnames(plants_df)[5:21],
    distance.matrix = plants_distance,
    distance.thresholds = c(
      0,
      1000,
      2000
    ),
    verbose = FALSE
  )

  rf.model <- rf_evaluate(
    model = rf.model,
    xy = plants_df[, c("x", "y")],
    verbose = FALSE,
    cluster = cluster
  )

  foreach::registerDoSEQ()
  parallel::stopCluster(cluster)
  invisible(gc())

  expect_s3_class(rf.model, "rf_evaluate")
  expect_type(rf.model$evaluation, "list")
  expect_s3_class(rf.model$evaluation$per.fold, "data.frame")
  expect_s3_class(rf.model$evaluation$per.model, "data.frame")
  expect_s3_class(rf.model$evaluation$aggregated, "data.frame")
  
})
