test_that("`pca_distance_matrix()` works", {
  data(distance_matrix)
  out <- pca_distance_matrix(
    x = distance_matrix,
    distance.thresholds = c(0, 1000)
    )
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), nrow(distance_matrix))
})
