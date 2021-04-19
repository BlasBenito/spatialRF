test_that("`default_distance_thresholds()` works", {
  data(distance_matrix)
  x <- default_distance_thresholds(distance_matrix)
  expect_length(x, 4)
  expect_type(x, "double")
})
