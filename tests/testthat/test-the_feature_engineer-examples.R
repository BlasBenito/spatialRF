test_that("`the_feature_engineer()` works", {
  data(plants_df)

  cluster <- parallel::makeCluster(
    parallel::detectCores() - 1,
    type = "PSOCK"
  )

  interactions <- the_feature_engineer(
    data = plants_df,
    dependent.variable.name = "richness_species_vascular",
    predictor.variable.names = colnames(plants_df)[5:21],
    xy = plants_df[, c("x", "y")],
    verbose = FALSE,
    seed = 100,
    cluster = cluster
  )

  foreach::registerDoSEQ()
  parallel::stopCluster(cluster)
  invisible(gc())

  expect_s3_class(interactions$screening, "data.frame")
  expect_s3_class(interactions$selected, "data.frame")
  
})
