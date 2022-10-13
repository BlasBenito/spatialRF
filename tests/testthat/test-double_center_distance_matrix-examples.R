testthat::test_that("`double_center_distance_matrix()` works", {

  data(ecoregions_distance_matrix)

  x <- double_center_distance_matrix(ecoregions_distance_matrix)

  testthat::expect_equal(
    ncol(ecoregions_distance_matrix),
    ncol(x)
    )

  testthat::expect_equal(
    nrow(ecoregions_distance_matrix),
    nrow(x)
    )

  testthat::expect_equal(
    is.matrix(x),
    TRUE
    )

  testthat::expect_type(
    x,
    "double"
    )

})
