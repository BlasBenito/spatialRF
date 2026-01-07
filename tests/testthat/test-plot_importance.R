test_that("`plot_importance()` works", {
  data(plants_rf)

  p <- plot_importance(plants_rf, verbose = FALSE)
  expect_equal(inherits(p, "ggplot"), TRUE)
  expect_true(!is.null(p$data))
  expect_true(nrow(p$data) > 0)
})


test_that("plot_importance() works with rf_spatial models", {
  data(plants_rf_spatial)

  p <- plot_importance(plants_rf_spatial, verbose = FALSE)

  expect_s3_class(p, "ggplot")
  expect_true(!is.null(p$data))
  expect_true(nrow(p$data) > 0)
})

test_that("plot_importance() works with data frame input", {
  data(plants_rf)

  # Extract importance data frame
  importance_df <- plants_rf$importance$per.variable

  p <- plot_importance(importance_df, verbose = FALSE)

  expect_s3_class(p, "ggplot")
  expect_true(!is.null(p$data))
})
