testthat::test_that("`thinning_til_n()` works", {

  data(ecoregions_df)

  ecoregions_df <- tibble::as_tibble(ecoregions_df)

  plant_richness.thin <- thinning_til_n(
    x = ecoregions_df,
    n = 100
  )

  testthat::expect_s3_class(plant_richness.thin, "data.frame")

  testthat::expect_equal(colnames(plant_richness.thin), colnames(plant_richness.thin))
})
