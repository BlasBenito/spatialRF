testthat::test_that("`weights_from_distance_matrix()` works", {

  data(ecoregions_distance_matrix)

  distance.matrix.weights <- weights_from_distance_matrix(ecoregions_distance_matrix)

  testthat::expect_equal(ncol(ecoregions_distance_matrix), ncol(distance.matrix.weights))
  testthat::expect_equal(nrow(ecoregions_distance_matrix), nrow(distance.matrix.weights))
  testthat::expect_equal(is.matrix(distance.matrix.weights), TRUE)
  testthat::expect_type(distance.matrix.weights, "double")
})
