testthat::test_that("`mem()` works", {

  library(spatialRF)
  library(magrittr)

  data(ecoregions_distance_matrix)

  x <- mem(ecoregions_distance_matrix)

  testthat::expect_s3_class(x, "data.frame")
  testthat::expect_equal("spatial_predictor__1", colnames(x)[1])
  testthat::expect_equal(nrow(ecoregions_distance_matrix), nrow(x))

  x <- mem_multithreshold(
    ecoregions_distance_matrix,
    distance.thresholds = c(0, 1000, 2000)
  )

  testthat::expect_s3_class(x, "data.frame")
  testthat::expect_equal(nrow(x), nrow(ecoregions_distance_matrix))

})
