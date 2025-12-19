test_that("`default_distance_thresholds()` works", {
  data(plants_distance)
  x <- default_distance_thresholds(plants_distance)
  expect_length(x, 4)
  expect_type(x, "double")
})
