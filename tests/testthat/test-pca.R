test_that("`pca()` works", {
  data(plants_distance)
  out <- pca(x = plants_distance)
  expect_s3_class(out, "data.frame")
  expect_equal(ncol(out), ncol(plants_distance))
  expect_equal(nrow(out), nrow(plants_distance))
})
