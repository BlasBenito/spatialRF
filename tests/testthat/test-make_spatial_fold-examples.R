testthat::test_that("`make_spatial_fold()` works", {

  data(ecoregions_df)

  ecoregions_df <- tibble::as_tibble(ecoregions_df)

  xy <- ecoregions_df[, c("ecoregion_id", "x", "y")]

  colnames(xy) <- c("id", "x", "y")

  out <- make_spatial_fold(
    xy.i = xy[1, ],
    xy = xy,
    training.fraction = 0.6
  )

  testthat::expect_type(out, "list")
  testthat::expect_length(out, 2)
  testthat::expect_named(out, c("training", "testing"))
  testthat::expect_type(out$training, "integer")
  testthat::expect_type(out$testing, "integer")
})
