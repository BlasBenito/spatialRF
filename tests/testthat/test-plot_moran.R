test_that("`plot_moran()` works", {
  data(plants_rf_spatial)

  p <- plot_moran(plants_rf_spatial, verbose = FALSE)
  expect_equal(inherits(p, "ggplot"), TRUE)
  expect_true(!is.null(p$data))
  expect_true(nrow(p$data) > 0)
})


test_that("plot_moran() works with rf models - option 2", {
  data(plants_rf_spatial)

  p <- plot_moran(plants_rf_spatial, option = 2, verbose = FALSE)

  expect_equal(inherits(p, "ggplot"), TRUE)
  expect_true(!is.null(p$data))
  expect_true(nrow(p$data) > 0)
})

test_that("plot_moran() works with rf_spatial models - option 1", {
  data(plants_rf_spatial)

  p <- plot_moran(plants_rf_spatial, verbose = FALSE)

  expect_s3_class(p, "ggplot")
  expect_true(!is.null(p$data))
  expect_true(nrow(p$data) > 0)
})

test_that("plot_moran() works with rf_spatial models - option 2", {
  data(plants_rf_spatial)

  p <- plot_moran(plants_rf_spatial, option = 2, verbose = FALSE)

  expect_s3_class(p, "ggplot")
  expect_true(!is.null(p$data))
  expect_true(nrow(p$data) > 0)
})

test_that("plot_moran() works with rf_repeat models - option 1", {
  data(plants_rf_spatial)

  # Create rf_repeat model
  model_repeat <- rf_repeat(
    model = plants_rf_spatial,
    repetitions = 5,
    verbose = FALSE
  )

  p <- plot_moran(model_repeat, verbose = FALSE)

  expect_s3_class(p, "ggplot")
  expect_true(!is.null(p$data))
  expect_true(nrow(p$data) > 0)

  # Should have "repetition" in the data
  expect_true("repetition" %in% colnames(p$data))
})

test_that("plot_moran() works with data frame input from moran()", {
  data(plants_rf_spatial, plants_distance)

  residuals <- get_residuals(plants_rf_spatial)

  # Run moran test
  moran_results <- moran_multithreshold(
    x = residuals,
    distance.matrix = plants_distance,
    distance.thresholds = c(100, 1000, 2000, 4000)
  )

  # Pass the per.distance data frame
  p <- plot_moran(moran_results$per.distance, verbose = FALSE)

  expect_s3_class(p, "ggplot")
  expect_true(!is.null(p$data))
})
