test_that("`mem_multithreshold()` works", {
  data(plants_distance)
  x <- mem_multithreshold(
    plants_distance,
    distance.thresholds = c(0, 1000, 2000)
  )
  expect_s3_class(x, "data.frame")
  expect_equal(nrow(x), nrow(plants_distance))
})
