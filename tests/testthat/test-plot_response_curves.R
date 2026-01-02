test_that("`plot_response_surfaces()` works", {
  data(plants_rf)
  p <- plot_response_curves(plants_rf)
  testthat::expect_true(
    inherits(p, "patchwork")
  )
})


test_that("plot_response_curves() works with rf models", {
  data(plants_rf)

  p <- plot_response_curves(plants_rf, verbose = FALSE)

  expect_s3_class(p, "ggplot")
  expect_true(!is.null(p))
})

test_that("plot_response_curves() works with specific variables", {
  data(plants_rf)

  p <- plot_response_curves(
    plants_rf,
    variables = "climate_bio1_average",
    # Returns ggplot for single variable
    verbose = FALSE
  )

  expect_s3_class(p, "ggplot")
  expect_true(!is.null(p))
})

test_that("plot_response_curves() works with multiple variables", {
  data(plants_rf)

  p <- plot_response_curves(
    plants_rf,
    variables = c("climate_bio1_average", "climate_aridity_index_average"),
    verbose = FALSE
  )

  expect_s3_class(p, "patchwork")
  expect_true(!is.null(p))
})

test_that("plot_response_curves() works with show.data = TRUE", {
  data(plants_rf)

  p <- plot_response_curves(
    plants_rf,
    variables = "climate_bio1_average",
    # Returns ggplot for single variable
    show.data = TRUE,
    verbose = FALSE
  )

  expect_s3_class(p, "ggplot")
  expect_true(!is.null(p))
})

test_that("plot_response_curves() works with custom quantiles", {
  data(plants_rf)

  p <- plot_response_curves(
    plants_rf,
    variables = "climate_bio1_average",
    # Returns ggplot for single variable
    quantiles = c(0.25, 0.75),
    verbose = FALSE
  )

  expect_s3_class(p, "ggplot")
  expect_true(!is.null(p))
})

test_that("plot_response_curves() works with custom grid.resolution", {
  data(plants_rf)

  p <- plot_response_curves(
    plants_rf,
    variables = "climate_bio1_average",
    # Returns ggplot for single variable
    grid.resolution = 50,
    verbose = FALSE
  )

  expect_s3_class(p, "ggplot")
  expect_true(!is.null(p))
})

test_that("plot_response_curves() works with custom line.color", {
  data(plants_rf)

  p <- plot_response_curves(
    plants_rf,
    variables = "climate_bio1_average",
    # Returns ggplot for single variable
    line.color = c("blue", "green", "red"),
    verbose = FALSE
  )

  expect_s3_class(p, "ggplot")
  expect_true(!is.null(p))
})

test_that("plot_response_curves() works with custom ncol", {
  data(plants_rf)

  p <- plot_response_curves(
    plants_rf,
    variables = c("climate_bio1_average", "climate_aridity_index_average"),
    ncol = 1,
    verbose = FALSE
  )

  expect_s3_class(p, "ggplot")
  expect_true(!is.null(p))
})

test_that("plot_response_curves() works with rf_spatial models", {
  data(plants_rf_spatial)

  p <- plot_response_curves(
    plants_rf_spatial,
    verbose = FALSE
  )

  expect_s3_class(p, "ggplot")
  expect_true(!is.null(p))
})

test_that("plot_response_curves() handles insufficient line colors", {
  data(plants_rf)

  # Provide fewer colors than quantiles
  expect_message(
    p <- plot_response_curves(
      plants_rf,
      variables = "climate_bio1_average",
      # Returns ggplot for single variable
      quantiles = c(0.1, 0.5, 0.9),
      line.color = c("blue"),
      verbose = TRUE
    ),
    "Insufficient colors"
  )

  expect_s3_class(p, "ggplot")
})

test_that("plot_response_curves() handles excess line colors", {
  data(plants_rf)

  # Provide more colors than quantiles
  p <- plot_response_curves(
    plants_rf,
    variables = "climate_bio1_average",
    # Returns ggplot for single variable
    quantiles = c(0.5),
    line.color = c("blue", "green", "red", "yellow"),
    verbose = FALSE
  )

  expect_s3_class(p, "ggplot")
  expect_true(!is.null(p))
})

test_that("plot_response_curves() works with single quantile", {
  data(plants_rf)

  p <- plot_response_curves(
    plants_rf,
    variables = "climate_bio1_average",
    # Returns ggplot for single variable
    quantiles = 0.5,
    verbose = FALSE
  )

  expect_s3_class(p, "ggplot")
  expect_true(!is.null(p))
})

test_that("plot_response_curves() verbose parameter controls printing", {
  data(plants_rf)

  # verbose = FALSE should not print
  expect_silent(
    p <- plot_response_curves(
      plants_rf,
      variables = "climate_bio1_average",
      # Returns ggplot for single variable
      verbose = FALSE
    )
  )

  expect_s3_class(p, "ggplot")
})

test_that("plot_response_curves() works with different color palettes", {
  data(plants_rf)

  # Function-based palette
  p <- plot_response_curves(
    plants_rf,
    variables = "climate_bio1_average",
    # Returns ggplot for single variable
    line.color = grDevices::hcl.colors(3, palette = "Blues"),
    verbose = FALSE
  )

  expect_s3_class(p, "ggplot")
  expect_true(!is.null(p))
})

test_that("plot_response_curves() handles NULL variables (auto-select)", {
  data(plants_rf)

  # NULL variables should auto-select important ones
  p <- plot_response_curves(
    plants_rf,
    variables = NULL,
    verbose = FALSE
  )

  expect_s3_class(p, "ggplot")
  expect_true(!is.null(p))
})

test_that("plot_response_curves() works with varying grid resolutions", {
  data(plants_rf)

  # Low resolution
  p1 <- plot_response_curves(
    plants_rf,
    variables = "climate_bio1_average",
    # Returns ggplot for single variable
    grid.resolution = 20,
    verbose = FALSE
  )

  # High resolution
  p2 <- plot_response_curves(
    plants_rf,
    variables = "climate_bio1_average",
    # Returns ggplot for single variable
    grid.resolution = 500,
    verbose = FALSE
  )

  expect_s3_class(p1, "ggplot")
  expect_s3_class(p2, "ggplot")
})

test_that("plot_response_curves() combines show.data with custom parameters", {
  data(plants_rf)

  p <- plot_response_curves(
    plants_rf,
    variables = "climate_bio1_average",
    # Returns ggplot for single variable
    quantiles = c(0.25, 0.75),
    show.data = TRUE,
    line.color = c("red", "blue"),
    verbose = FALSE
  )

  expect_s3_class(p, "ggplot")
  expect_true(!is.null(p))
})
