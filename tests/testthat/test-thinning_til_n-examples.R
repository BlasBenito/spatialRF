test_that("`thinning_til_n()` works", {
  data(plants_df)
  plant_richness.thin <- thinning_til_n(
    x = plants_df,
    n = 100
  )
  expect_s3_class(plant_richness.thin, "data.frame")
  expect_equal(colnames(plant_richness.thin), colnames(plant_richness.thin))
})
