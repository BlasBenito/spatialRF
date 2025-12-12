test_that("`make_spatial_folds()` works", {
  data(plant_richness_df)
  xy <- plant_richness_df[, 1:3]
  colnames(xy) <- c("id", "x", "y")
  xy.selected <- thinning_til_n(
    xy = xy,
    n = 20
  )
  out <- make_spatial_folds(
    xy.selected = xy.selected,
    xy = xy,
    training.fraction = 0.6
  )
  expect_type(out, "list")
  expect_length(out, nrow(xy.selected))
  expect_named(out[[1]], c("training", "testing"))
  expect_type(out[[1]]$training, "integer")
  expect_type(out[[1]]$testing, "integer")
})
