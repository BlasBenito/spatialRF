test_that("`weights_from_distance_matrix()` works", {
  data(distance_matrix)
  distance.matrix.weights <- weights_from_distance_matrix(x = distance_matrix)
  expect_equal(ncol(distance_matrix), ncol(distance.matrix.weights))
  expect_equal(nrow(distance_matrix), nrow(distance.matrix.weights))
  expect_equal(is.matrix(distance.matrix.weights), TRUE)
  expect_type(distance.matrix.weights, "double")
})
