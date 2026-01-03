test_that("`default_distance_thresholds()` works", {
  data(plants_distance)
  x <- default_distance_thresholds(distance.matrix = plants_distance)
  expect_true(length(x) >= 1)
  expect_true(length(x) <= 4)
  expect_type(x, "double")
  expect_true(all(x > 0))
})

test_that("`default_distance_thresholds()` validates user-provided thresholds", {
  data(plants_distance)

  # Valid custom thresholds
  result <- default_distance_thresholds(
    distance.thresholds = c(1000, 5000),
    distance.matrix = plants_distance
  )
  expect_equal(result, c(1000, 5000))

  # Invalid: negative
  expect_error(
    default_distance_thresholds(c(-100, 2000), plants_distance),
    "must be greater than 0"
  )

  # Invalid: exceeds max
  expect_error(
    default_distance_thresholds(c(999999), plants_distance),
    "must be <= max"
  )

  # Invalid: zero
  expect_error(
    default_distance_thresholds(c(0, 1000), plants_distance),
    "must be greater than 0"
  )

  # Invalid: non-numeric
  expect_error(
    default_distance_thresholds("1000", plants_distance),
    "must be numeric"
  )

  # Invalid: matrix instead of vector
  expect_error(
    default_distance_thresholds(matrix(c(1000, 2000), ncol = 2), plants_distance),
    "must be a vector"
  )
})
