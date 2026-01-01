test_that("`make_spatial_fold()` works", {
  data(plants_df)
  xy <- plants_df[, 1:3]
  colnames(xy) <- c("id", "x", "y")
  out <- make_spatial_fold(
    xy.i = xy[1, ],
    xy = xy,
    training.fraction = 0.6
  )
  expect_type(out, "logical")
  expect_length(out, nrow(xy))
  expect_true(sum(out) > 0)
  expect_true(sum(!out) > 0)
})
