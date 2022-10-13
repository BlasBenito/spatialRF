testthat::test_that("`default_distance_thresholds()` works", {
  data(ecoregions_distance_matrix)

  x <- default_distance_thresholds(ecoregions_distance_matrix)

  testthat::expect_length(x, 4)
  testthat::expect_type(x, "double")
})
