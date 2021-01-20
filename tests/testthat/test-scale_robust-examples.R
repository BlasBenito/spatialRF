test_that("`scale_robust()` works", {
  data(plant_richness_df)
  plant_richness_df.scaled <- scale_robust(x = plant_richness_df[,5:10])
  expect_equal(ncol(plant_richness_df[,5:10]), ncol(plant_richness_df.scaled))
  expect_equal(nrow(plant_richness_df[,5:10]), nrow(plant_richness_df.scaled))
})
