testthat::test_that("`make_spatial_folds()` works", {
  data(ecoregions_df)

  ecoregions_df <- tibble::as_tibble(ecoregions_df)

  xy <- ecoregions_df[, c("ecoregion_id", "x", "y")]

  colnames(xy) <- c("id", "x", "y")

  xy.selected <- thinning_til_n(
    xy = xy,
    n = 20
    )

  out <- make_spatial_folds(
    xy.selected = xy.selected,
    xy = xy,
    training.fraction = 0.6,
    n.cores = 1
  )

  testthat::expect_type(out, "list")
  testthat::expect_length(out, nrow(xy.selected))
  testthat::expect_named(out[[1]], c("training", "testing"))
  testthat::expect_type(out[[1]]$training, "integer")
  testthat::expect_type(out[[1]]$testing, "integer")
})
