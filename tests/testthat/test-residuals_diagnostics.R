test_that("residuals_diagnostics() works with model residuals", {
  data(plants_rf)

  residuals <- get_residuals(plants_rf)
  predictions <- get_predictions(plants_rf)

  result <- residuals_diagnostics(
    residuals = residuals,
    predictions = predictions
  )

  expect_type(result, "list")
  expect_named(result, c("shapiro.w", "p.value", "interpretation", "plot"))
})

test_that("residuals_diagnostics() output has correct structure", {
  data(plants_rf)

  result <- residuals_diagnostics(
    residuals = get_residuals(plants_rf),
    predictions = get_predictions(plants_rf)
  )

  expect_type(result$shapiro.w, "double")
  expect_type(result$p.value, "double")
  expect_type(result$interpretation, "character")
  expect_s3_class(result$plot, "patchwork")
})

test_that("residuals_diagnostics() shapiro.w is between 0 and 1", {
  data(plants_rf)

  result <- residuals_diagnostics(
    residuals = get_residuals(plants_rf),
    predictions = get_predictions(plants_rf)
  )

  expect_true(result$shapiro.w >= 0 & result$shapiro.w <= 1)
})

test_that("residuals_diagnostics() p.value is between 0 and 1", {
  data(plants_rf)

  result <- residuals_diagnostics(
    residuals = get_residuals(plants_rf),
    predictions = get_predictions(plants_rf)
  )

  expect_true(result$p.value >= 0 & result$p.value <= 1)
})

test_that("residuals_diagnostics() interpretation is correct", {
  data(plants_rf)

  result <- residuals_diagnostics(
    residuals = get_residuals(plants_rf),
    predictions = get_predictions(plants_rf)
  )

  if (result$p.value > 0.05) {
    expect_equal(result$interpretation, "Residuals are normal")
  } else {
    expect_equal(result$interpretation, "Residuals are not normal")
  }
})

test_that("residuals_diagnostics() works with normal residuals", {
  # Generate normal residuals
  set.seed(123)
  residuals <- rnorm(200, mean = 0, sd = 1)
  predictions <- rnorm(200, mean = 10, sd = 2)

  result <- residuals_diagnostics(
    residuals = residuals,
    predictions = predictions
  )

  expect_type(result, "list")
  # Normal residuals should likely have high p-value
  expect_true(result$p.value > 0.01)
})

test_that("residuals_diagnostics() works with non-normal residuals", {
  # Generate non-normal residuals (exponential distribution)
  set.seed(123)
  residuals <- rexp(200, rate = 1)
  predictions <- rnorm(200, mean = 10, sd = 2)

  result <- residuals_diagnostics(
    residuals = residuals,
    predictions = predictions
  )

  expect_type(result, "list")
  # Exponential residuals should likely fail normality test
  expect_true(result$p.value < 0.10)
})

test_that("residuals_diagnostics() handles large vectors", {
  # Test with > 5000 cases (triggers sampling)
  set.seed(123)
  residuals <- rnorm(6000, mean = 0, sd = 1)
  predictions <- rnorm(6000, mean = 10, sd = 2)

  result <- residuals_diagnostics(
    residuals = residuals,
    predictions = predictions
  )

  expect_type(result, "list")
  expect_named(result, c("shapiro.w", "p.value", "interpretation", "plot"))
  expect_true(result$shapiro.w >= 0 & result$shapiro.w <= 1)
})

test_that("residuals_diagnostics() handles small vectors", {
  # Minimum for Shapiro test is 3
  residuals <- c(-1, 0, 1)
  predictions <- c(5, 10, 15)

  result <- residuals_diagnostics(
    residuals = residuals,
    predictions = predictions
  )

  expect_type(result, "list")
  expect_named(result, c("shapiro.w", "p.value", "interpretation", "plot"))
})

test_that("residuals_diagnostics() plot is created", {
  data(plants_rf)

  result <- residuals_diagnostics(
    residuals = get_residuals(plants_rf),
    predictions = get_predictions(plants_rf)
  )

  # Plot should be patchwork
  expect_s3_class(result$plot, "patchwork")

  # Plot should have title
  expect_true(!is.null(result$plot))
})

test_that("residuals_diagnostics() handles different residual distributions", {
  set.seed(123)

  # Normal
  res_normal <- rnorm(100)
  pred_normal <- rnorm(100, 10, 2)

  # Uniform
  res_uniform <- runif(100, -2, 2)
  pred_uniform <- rnorm(100, 10, 2)

  # Bimodal
  res_bimodal <- c(rnorm(50, -2, 0.5), rnorm(50, 2, 0.5))
  pred_bimodal <- rnorm(100, 10, 2)

  r1 <- residuals_diagnostics(res_normal, pred_normal)
  r2 <- residuals_diagnostics(res_uniform, pred_uniform)
  r3 <- residuals_diagnostics(res_bimodal, pred_bimodal)

  # All should produce valid output
  expect_type(r1, "list")
  expect_type(r2, "list")
  expect_type(r3, "list")

  # All should have valid test statistics
  expect_true(r1$shapiro.w >= 0 & r1$shapiro.w <= 1)
  expect_true(r2$shapiro.w >= 0 & r2$shapiro.w <= 1)
  expect_true(r3$shapiro.w >= 0 & r3$shapiro.w <= 1)
})

test_that("residuals_diagnostics() interpretation matches p-value threshold", {
  set.seed(123)
  residuals <- rnorm(100)
  predictions <- rnorm(100, 10, 2)

  result <- residuals_diagnostics(
    residuals = residuals,
    predictions = predictions
  )

  # Verify interpretation logic
  if (result$p.value > 0.05) {
    expect_match(result$interpretation, "normal")
    expect_match(result$interpretation, "are", ignore.case = FALSE)
  } else {
    expect_match(result$interpretation, "not normal")
  }
})

test_that("residuals_diagnostics() works with spatial model residuals", {
  data(plants_rf_spatial)

  result <- residuals_diagnostics(
    residuals = get_residuals(plants_rf_spatial),
    predictions = get_predictions(plants_rf_spatial)
  )

  expect_type(result, "list")
  expect_named(result, c("shapiro.w", "p.value", "interpretation", "plot"))
  expect_true(result$shapiro.w >= 0 & result$shapiro.w <= 1)
})

test_that("residuals_diagnostics() handles negative and positive residuals", {
  set.seed(123)
  residuals <- rnorm(100, mean = 0, sd = 2)
  predictions <- rnorm(100, mean = 10, sd = 3)

  result <- residuals_diagnostics(
    residuals = residuals,
    predictions = predictions
  )

  # Should work regardless of residual signs
  expect_type(result, "list")
  expect_true(any(residuals < 0))
  expect_true(any(residuals > 0))
})

test_that("residuals_diagnostics() shapiro.w has no names", {
  data(plants_rf)

  result <- residuals_diagnostics(
    residuals = get_residuals(plants_rf),
    predictions = get_predictions(plants_rf)
  )

  # names should be removed (line 47 in source)
  expect_null(names(result$shapiro.w))
})
