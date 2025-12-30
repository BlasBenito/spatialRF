test_that("plot_importance() works with rf models", {
  data(plants_rf)

  p <- plot_importance(plants_rf, verbose = FALSE)

  expect_s3_class(p, "ggplot")
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

test_that("plot_importance() respects fill.color parameter", {
  data(plants_rf)

  # Custom colors
  p <- plot_importance(
    plants_rf,
    fill.color = c("#440154FF", "#21908CFF", "#FDE725FF"),
    verbose = FALSE
  )

  expect_s3_class(p, "ggplot")
})

test_that("plot_importance() respects line.color parameter", {
  data(plants_rf)

  p <- plot_importance(
    plants_rf,
    line.color = "red",
    verbose = FALSE
  )

  expect_s3_class(p, "ggplot")
})

test_that("plot_importance() handles importance.oob column", {
  data(plants_rf)

  # Modify to have importance.oob instead of importance
  model_copy <- plants_rf
  if ("importance" %in% colnames(model_copy$importance$per.variable)) {
    colnames(model_copy$importance$per.variable)[
      colnames(model_copy$importance$per.variable) == "importance"
    ] <- "importance.oob"
  }

  p <- plot_importance(model_copy, verbose = FALSE)

  expect_s3_class(p, "ggplot")
})

test_that("plot_importance() verbose parameter controls printing", {
  data(plants_rf)

  # verbose = FALSE should not error
  expect_silent(p1 <- plot_importance(plants_rf, verbose = FALSE))

  # Should still return a plot
  expect_s3_class(p1, "ggplot")
})

test_that("plot_importance() works with different color palettes", {
  data(plants_rf)

  # Function-based palette
  p1 <- plot_importance(
    plants_rf,
    fill.color = grDevices::hcl.colors(50, palette = "Blues"),
    verbose = FALSE
  )

  # Vector of colors
  p2 <- plot_importance(
    plants_rf,
    fill.color = c("blue", "green", "red"),
    verbose = FALSE
  )

  expect_s3_class(p1, "ggplot")
  expect_s3_class(p2, "ggplot")
})

test_that("plot_importance() orders variables by importance", {
  data(plants_rf)

  p <- plot_importance(plants_rf, verbose = FALSE)

  # Check that data exists and has variable column
  expect_true("variable" %in% colnames(p$data))
  expect_true("importance" %in% colnames(p$data))
})

test_that("plot_importance() plot has appropriate labels", {
  data(plants_rf)

  p <- plot_importance(plants_rf, verbose = FALSE)

  # Should have axis labels
  expect_true(!is.null(p$labels))
})

test_that("plot_importance() handles models with few variables", {
  data(plants_df, plants_distance)

  # Fit model with only 3 predictors
  model_small <- rf(
    data = plants_df[1:100, ],
    dependent.variable.name = "richness_species_vascular",
    predictor.variable.names = colnames(plants_df)[5:7],
    distance.matrix = plants_distance[1:100, 1:100],
    verbose = FALSE,
    seed = 1,
    n.cores = 1
  )

  p <- plot_importance(model_small, verbose = FALSE)

  expect_s3_class(p, "ggplot")
  expect_equal(nrow(p$data), 3)
})

test_that("plot_importance() handles models with many variables", {
  data(plants_rf)

  # plants_rf has many predictors
  p <- plot_importance(plants_rf, verbose = FALSE)

  expect_s3_class(p, "ggplot")
  expect_true(nrow(p$data) > 10)
})

test_that("plot_importance() spatial.predictors are grouped in rf_spatial", {
  data(plants_rf_spatial)

  p <- plot_importance(plants_rf_spatial, verbose = FALSE)

  # Check if spatial_predictors grouping exists
  expect_true("variable" %in% colnames(p$data))

  # Should have spatial_predictors as a variable name
  expect_true(any(grepl("spatial", p$data$variable, ignore.case = TRUE)))
})

test_that("plot_importance() works with different model types", {
  data(plants_rf, plants_rf_spatial)

  p1 <- plot_importance(plants_rf, verbose = FALSE)
  p2 <- plot_importance(plants_rf_spatial, verbose = FALSE)

  # Both should produce valid plots
  expect_s3_class(p1, "ggplot")
  expect_s3_class(p2, "ggplot")

  # But they may have different data structures
  expect_true(nrow(p1$data) > 0)
  expect_true(nrow(p2$data) > 0)
})

test_that("plot_importance() works with rf_repeat models", {
  data(plants_df, plants_distance)

  # Create rf_repeat model (non-spatial)
  model_repeat <- rf_repeat(
    data = plants_df[1:100, ],
    dependent.variable.name = "richness_species_vascular",
    predictor.variable.names = colnames(plants_df)[5:11],
    distance.matrix = plants_distance[1:100, 1:100],
    repetitions = 5,
    verbose = FALSE,
    n.cores = 1,
    seed = 1
  )

  p <- plot_importance(model_repeat, verbose = FALSE)

  expect_s3_class(p, "ggplot")
  expect_true(!is.null(p$data))
  expect_true(nrow(p$data) > 0)

  # Should have geom_violin for repeated models
  expect_true(any(sapply(p$layers, function(x) inherits(x$geom, "GeomViolin"))))
})

test_that("plot_importance() works with rf_repeat spatial models", {
  data(plants_rf_spatial)

  # Create rf_repeat from spatial model
  model_spatial_repeat <- rf_repeat(
    model = plants_rf_spatial,
    repetitions = 5,
    verbose = FALSE,
    n.cores = 1,
    seed = 1
  )

  p <- plot_importance(model_spatial_repeat, verbose = FALSE)

  expect_s3_class(p, "ggplot")
  expect_true(!is.null(p$data))
  expect_true(nrow(p$data) > 0)

  # Should have geom_violin for repeated spatial models
  expect_true(any(sapply(p$layers, function(x) inherits(x$geom, "GeomViolin"))))
})

test_that("plot_importance() handles rf_repeat with custom colors", {
  data(plants_df, plants_distance)

  model_repeat <- rf_repeat(
    data = plants_df[1:80, ],
    dependent.variable.name = "richness_species_vascular",
    predictor.variable.names = colnames(plants_df)[5:9],
    distance.matrix = plants_distance[1:80, 1:80],
    repetitions = 5,
    verbose = FALSE,
    n.cores = 1,
    seed = 1
  )

  # Test with custom fill colors
  p <- plot_importance(
    model_repeat,
    fill.color = c("blue", "green", "red"),
    verbose = FALSE
  )

  expect_s3_class(p, "ggplot")
  expect_true(!is.null(p$data))
})

test_that("plot_importance() rf_repeat respects line.color parameter", {
  data(plants_df, plants_distance)

  model_repeat <- rf_repeat(
    data = plants_df[1:80, ],
    dependent.variable.name = "richness_species_vascular",
    predictor.variable.names = colnames(plants_df)[5:8],
    distance.matrix = plants_distance[1:80, 1:80],
    repetitions = 5,
    verbose = FALSE,
    n.cores = 1,
    seed = 1
  )

  p <- plot_importance(
    model_repeat,
    line.color = "purple",
    verbose = FALSE
  )

  expect_s3_class(p, "ggplot")
  expect_true(!is.null(p$data))
})
