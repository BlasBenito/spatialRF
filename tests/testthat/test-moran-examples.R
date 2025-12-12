test_that("`moran()` works", {
  data(distance_matrix)
  data(plant_richness_df)
  out <- moran(
    plant_richness_df$richness_species_vascular,
    distance.matrix = distance_matrix,
    verbose = FALSE
  )
  expect_type(out, "list")
  expect_s3_class(out$test, "data.frame")
  expect_s3_class(out$plot.df, "data.frame")
  expect_length(out, 3)
  expect_named(out, c("test", "plot", "plot.df"))
})
