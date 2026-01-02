test_that("`weights_from_distance_matrix()` works", {
  data(plants_distance)
  distance.matrix.weights <- weights_from_distance_matrix(plants_distance)
  expect_equal(ncol(plants_distance), ncol(distance.matrix.weights))
  expect_equal(nrow(plants_distance), nrow(distance.matrix.weights))
  expect_equal(is.matrix(distance.matrix.weights), TRUE)
  expect_type(distance.matrix.weights, "double")
})
