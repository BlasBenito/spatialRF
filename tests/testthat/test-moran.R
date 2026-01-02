test_that("`moran()` works", {
  data(plants_distance)
  data(plants_df)
  out <- moran(
    plants_df$richness_species_vascular,
    distance.matrix = plants_distance,
    verbose = FALSE
  )
  expect_type(out, "list")
  expect_s3_class(out$test, "data.frame")
  expect_s3_class(out$plot.df, "data.frame")
  expect_length(out, 3)
  expect_named(out, c("test", "plot", "plot.df"))
})
