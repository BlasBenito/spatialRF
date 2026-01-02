test_that("`plot_moran()` works", {
  data(plants_rf)

  p <- plot_moran(plants_rf, verbose = FALSE)
  expect_equal(inherits(p, "ggplot"), TRUE)
})

test_that("plot_moran() works with rf models - option 1", {
  data(plants_rf)

  p <- plot_moran(plants_rf, verbose = FALSE)

  expect_s3_class(p, "ggplot")
  expect_true(!is.null(p$data))
  expect_true(nrow(p$data) > 0)
})

test_that("plot_moran() works with rf models - option 2", {
  data(plants_rf)

  p <- plot_moran(plants_rf, option = 2, verbose = FALSE)

  expect_s3_class(p, "ggplot")
  expect_true(!is.null(p))
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
  expect_true(!is.null(p))
})

test_that("plot_moran() works with rf_repeat models - option 1", {
  data(plants_df, plants_distance)

  # Create rf_repeat model
  model_repeat <- rf_repeat(
    data = plants_df[1:100, ],
    dependent.variable.name = "richness_species_vascular",
    predictor.variable.names = colnames(plants_df)[5:11],
    distance.matrix = plants_distance[1:100, 1:100],
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
  data(plants_df, plants_distance)

  # Get residuals from a model
  model <- rf(
    data = plants_df[1:100, ],
    dependent.variable.name = "richness_species_vascular",
    predictor.variable.names = colnames(plants_df)[5:11],
    distance.matrix = plants_distance[1:100, 1:100],
    verbose = FALSE
  )

  residuals <- get_residuals(model)

  # Run moran test
  moran_results <- moran_multithreshold(
    x = residuals,
    distance.matrix = plants_distance[1:100, 1:100],
    distance.thresholds = c(0, 1000, 2000)
  )

  # Pass the per.distance data frame
  p <- plot_moran(moran_results$per.distance, verbose = FALSE)

  expect_s3_class(p, "ggplot")
  expect_true(!is.null(p$data))
})

test_that("plot_moran() respects point.color parameter", {
  data(plants_rf)

  # Custom colors
  p <- plot_moran(
    plants_rf,
    point.color = c("#440154FF", "#21908CFF", "#FDE725FF"),
    verbose = FALSE
  )

  expect_s3_class(p, "ggplot")
  expect_true(!is.null(p$data))
})

test_that("plot_moran() respects line.color parameter", {
  data(plants_rf)

  p <- plot_moran(
    plants_rf,
    line.color = "red",
    verbose = FALSE
  )

  expect_s3_class(p, "ggplot")
  expect_true(!is.null(p$data))
})

test_that("plot_moran() respects ncol parameter for option 2", {
  data(plants_rf)

  p <- plot_moran(
    plants_rf,
    option = 2,
    ncol = 2,
    verbose = FALSE
  )

  expect_s3_class(p, "ggplot")
  expect_true(!is.null(p))
})

test_that("plot_moran() handles invalid option gracefully", {
  data(plants_rf)

  # Invalid option should default to 1
  p <- plot_moran(
    plants_rf,
    option = 99,
    verbose = FALSE
  )

  expect_s3_class(p, "ggplot")
  expect_true(!is.null(p$data))
})

test_that("plot_moran() data frame input forces option 1", {
  data(plants_df, plants_distance)

  # Get residuals from a model
  model <- rf(
    data = plants_df[1:100, ],
    dependent.variable.name = "richness_species_vascular",
    predictor.variable.names = colnames(plants_df)[5:11],
    distance.matrix = plants_distance[1:100, 1:100],
    verbose = FALSE
  )

  residuals <- get_residuals(model)

  # Run moran test
  moran_results <- moran_multithreshold(
    x = residuals,
    distance.matrix = plants_distance[1:100, 1:100],
    distance.thresholds = c(0, 1000)
  )

  # Even with option = 2, should produce option 1 plot for data frame
  p <- plot_moran(moran_results$per.distance, option = 2, verbose = FALSE)

  expect_s3_class(p, "ggplot")
  expect_true(!is.null(p$data))
})

test_that("plot_moran() output has correct structure - option 1", {
  data(plants_rf)

  p <- plot_moran(plants_rf, verbose = FALSE)

  # Should have required columns
  expect_true("distance.threshold" %in% colnames(p$data))
  expect_true("moran.i" %in% colnames(p$data))
  expect_true("p.value.binary" %in% colnames(p$data))
})

test_that("plot_moran() output has correct structure - option 2", {
  data(plants_rf)

  p <- plot_moran(plants_rf, option = 2, verbose = FALSE)

  # Should be a ggplot object
  expect_s3_class(p, "ggplot")
})

test_that("plot_moran() works with different color palettes", {
  data(plants_rf)

  # Function-based palette
  p1 <- plot_moran(
    plants_rf,
    point.color = grDevices::hcl.colors(100, palette = "Blues"),
    verbose = FALSE
  )

  # Vector of colors
  p2 <- plot_moran(
    plants_rf,
    point.color = c("blue", "green", "red"),
    verbose = FALSE
  )

  expect_s3_class(p1, "ggplot")
  expect_s3_class(p2, "ggplot")
})

test_that("plot_moran() p.value.binary is correctly set", {
  data(plants_rf)

  p <- plot_moran(plants_rf, verbose = FALSE)

  # p.value.binary should be a factor with two levels
  expect_true(is.factor(p$data$p.value.binary))
  expect_equal(levels(p$data$p.value.binary), c("< 0.05", ">= 0.05"))
})

test_that("plot_moran() handles models with single distance threshold", {
  data(plants_df, plants_distance)

  # Create model with single distance threshold
  model <- rf(
    data = plants_df[1:100, ],
    dependent.variable.name = "richness_species_vascular",
    predictor.variable.names = colnames(plants_df)[5:11],
    distance.matrix = plants_distance[1:100, 1:100],
    distance.thresholds = 1000, # Single threshold
    verbose = FALSE
  )

  # Option 1 should work
  p1 <- plot_moran(model, verbose = FALSE)
  expect_s3_class(p1, "ggplot")

  # Option 2 should work
  p2 <- plot_moran(model, option = 2, verbose = FALSE)
  expect_s3_class(p2, "ggplot")
})

test_that("plot_moran() verbose parameter controls printing", {
  data(plants_rf)

  # verbose = FALSE should not print
  expect_silent(p1 <- plot_moran(plants_rf, verbose = FALSE))

  # Should still return a plot
  expect_s3_class(p1, "ggplot")
})
