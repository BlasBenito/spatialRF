testthat::test_that("`thinning()` works", {

  data(ecoregions_df)

  ecoregions_df <- tibble::as_tibble(ecoregions_df)

  plant_richness.thin <- thinning(x = ecoregions_df, minimum.distance = 5)

  testthat::expect_s3_class(plant_richness.thin, "data.frame")
  testthat::expect_equal(colnames(plant_richness.thin), colnames(plant_richness.thin))

})
