test_that("`pca_multithreshold()` works", {
  data(distance_matrix)
  out <- pca_multithreshold(
    distance_matrix,
    distance.thresholds = c(0, 1000)
    )
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), nrow(distance_matrix))
})
