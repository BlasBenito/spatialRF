test_that("`the_feature_engineer()` works", {
  data(plants_df)

  # Increase future.globals.maxSize for this test
  old_limit <- getOption("future.globals.maxSize")
  options(future.globals.maxSize = 1024^3) # 1 GB
  on.exit(options(future.globals.maxSize = old_limit), add = TRUE)

  interactions <- the_feature_engineer(
    data = plants_df,
    dependent.variable.name = "richness_species_vascular",
    predictor.variable.names = colnames(plants_df)[5:21],
    xy = plants_df[, c("x", "y")],
    verbose = FALSE,
    seed = 100
  )

  invisible(gc())

  expect_s3_class(interactions$screening, "data.frame")
  expect_s3_class(interactions$selected, "data.frame")
})
