test_that("`double_center_distance_matrix()` works", {
  data(distance_matrix)
  x <- double_center_distance_matrix(x = distance_matrix)
  expect_equal(ncol(distance_matrix), ncol(x))
  expect_equal(nrow(distance_matrix), nrow(x))
  expect_equal(is.matrix(x), TRUE)
  expect_type(x, "double")
})
