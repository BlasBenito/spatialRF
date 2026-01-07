test_that("`plot_response_surfaces()` works", {
  data(plants_rf)
  p <- plot_response_curves(plants_rf)
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
