test_that("`make_spatial_fold()` works", {
  data(plant_richness_df)
  xy <- plant_richness_df[, 1:3]
  colnames(xy) <- c("id", "x", "y")
  out <- make_spatial_fold(
    xy.i = xy[1, ], xy = xy, distance.step = 0.05,
    training.fraction = 0.6
  )
  expect_type(out, "list")
  expect_length(out, 2)
  expect_named(out, c("training", "testing"))
  expect_type(out$training, "integer")
  expect_type(out$testing, "integer")
})
