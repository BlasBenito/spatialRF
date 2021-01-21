test_that("`pca()` works", {
  data(distance_matrix)
  out <- pca(x = distance_matrix)
  expect_s3_class(out, "data.frame")
  expect_equal(ncol(out), ncol(distance_matrix))
  expect_equal(nrow(out), nrow(distance_matrix))
})
