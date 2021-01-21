test_that("`thinning()` works", {
  data(plant_richness)
  plant_richness.thin <- thinning(x = plant_richness_df, minimum.distance = 5)
  expect_s3_class(plant_richness.thin, "data.frame")
  expect_equal(colnames(plant_richness.thin), colnames(plant_richness.thin))
})
