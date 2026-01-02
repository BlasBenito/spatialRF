test_that("`double_center_distance_matrix()` works", {
  data(plants_distance)
  x <- double_center_distance_matrix(plants_distance)
  expect_equal(ncol(plants_distance), ncol(x))
  expect_equal(nrow(plants_distance), nrow(x))
  expect_equal(is.matrix(x), TRUE)
  expect_type(x, "double")
})
