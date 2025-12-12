test_that("`rf_tuning()` works", {
  data(plant_richness_df)

  out <- rf(
    data = plant_richness_df,
    dependent.variable.name = "richness_species_vascular",
    predictor.variable.names = colnames(plant_richness_df)[5:21],
    verbose = FALSE
  )

  cluster <- parallel::makeCluster(
    parallel::detectCores() - 1,
    type = "PSOCK"
  )

  out <- rf_tuning(
    model = out,
    xy = plant_richness_df[, c("x", "y")],
    num.trees = c(100, 200),
    mtry = c(2, 4),
    min.node.size = c(5, 10),
    verbose = FALSE,
    cluster = cluster
  )

  parallel::stopCluster(cluster)

  expect_s3_class(out$tuning$tuning.df, "data.frame")
  expect_type(out$tuning, "list")
})
