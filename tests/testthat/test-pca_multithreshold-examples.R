test_that("`pca_multithreshold()` works", {
  data(plants_distance)
  out <- pca_multithreshold(
    plants_distance,
    distance.thresholds = c(0, 1000)
  )
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), nrow(plants_distance))
})
