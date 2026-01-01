test_that("`make_spatial_folds()` works", {
  data(plants_df)
  xy <- plants_df[, 1:3]
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

  # Output should be a data frame
  expect_type(out, "list")  # data.frame is a list in R
  expect_s3_class(out, "data.frame")

  # Should have one column per fold
  expect_equal(ncol(out), nrow(xy.selected))
  expect_equal(nrow(out), nrow(xy))

  # Column names should be fold_1, fold_2, etc.
  expect_equal(
    colnames(out),
    paste0("fold_", seq_len(nrow(xy.selected)))
  )

  # Each column should be logical (TRUE = training, FALSE = testing)
  for (i in seq_len(ncol(out))) {
    expect_type(out[[i]], "logical")
    expect_length(out[[i]], nrow(xy))
    # Should have both training and testing records
    expect_true(sum(out[[i]]) > 0)
    expect_true(sum(!out[[i]]) > 0)
  }

  # Verify no connections left open
})
