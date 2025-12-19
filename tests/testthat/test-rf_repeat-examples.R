test_that("`rf_repeat()` works", {
  data("plants_df")
  data("plants_distance")

  cluster <- parallel::makeCluster(
    parallel::detectCores() - 1,
    type = "PSOCK"
  )

  out <- rf_repeat(
    data = plants_df,
    dependent.variable.name = "richness_species_vascular",
    predictor.variable.names = colnames(plants_df)[5:21],
    distance.matrix = plants_distance,
    distance.thresholds = c(0, 100, 1000),
    repetitions = 5,
    verbose = FALSE,
    cluster = cluster
  )

  foreach::registerDoSEQ()
  parallel::stopCluster(cluster)
  invisible(gc())

  expect_s3_class(out, "rf_repeat")
  expect_s3_class(out$importance$per.variable, "data.frame")
  expect_s3_class(out$residuals$autocorrelation$per.distance, "data.frame")
  expect_named(
    out$residuals$autocorrelation$per.distance,
    c("distance.threshold", "moran.i", "p.value", "interpretation")
  )
  
})
