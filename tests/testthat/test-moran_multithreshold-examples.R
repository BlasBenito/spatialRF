test_that("`moran_multithreshold()` works", {
  data(distance_matrix)
  data(plant_richness_df)
  moran.out <- moran_multithreshold(
    plant_richness_df$richness_species_vascular,
    distance.matrix = distance_matrix,
    distance.thresholds = c(0, 100, 1000),
    verbose = FALSE
  )
  expect_type(moran.out, "list")
  expect_s3_class(moran.out$per.distance, "data.frame")
  expect_named(moran.out$per.distance, c("distance.threshold", "moran.i", "moran.i.null", "p.value", "interpretation"))
  expect_length(moran.out, 4)
})
