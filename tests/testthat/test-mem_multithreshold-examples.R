test_that("`mem_multithreshold()` works", {
  data(distance_matrix)
  x <- mem_multithreshold(
    distance_matrix,
    distance.thresholds = c(0, 1000, 2000)
  )
  expect_s3_class(x, "data.frame")
  expect_equal(nrow(x), nrow(distance_matrix))
})
