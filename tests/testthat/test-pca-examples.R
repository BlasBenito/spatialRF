testthat::test_that("`pca()` works", {

  data(ecoregions_distance_matrix)

  out <- pca(x = ecoregions_distance_matrix)

  testthat::expect_s3_class(out, "data.frame")

  testthat::expect_equal(
    ncol(out),
    ncol(ecoregions_distance_matrix)
    )

  testthat::expect_equal(
    nrow(out),
    nrow(ecoregions_distance_matrix)
    )

})
