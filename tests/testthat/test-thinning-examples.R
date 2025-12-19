test_that("`thinning()` works", {
  data(plants_df)
  plant_richness.thin <- thinning(x = plants_df, minimum.distance = 5)
  expect_s3_class(plant_richness.thin, "data.frame")
  expect_equal(colnames(plant_richness.thin), colnames(plant_richness.thin))
})
