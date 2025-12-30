test_that("residuals_test() works with basic inputs", {
  residuals <- rnorm(100, mean = 0, sd = 1)

  result <- residuals_test(residuals = residuals)

  expect_type(result, "list")
  expect_named(result, c("shapiro.w", "p.value", "interpretation"))
})

test_that("residuals_test() output has correct structure", {
  residuals <- rnorm(100)

  result <- residuals_test(residuals = residuals)

  expect_type(result$shapiro.w, "double")
  expect_type(result$p.value, "double")
  expect_type(result$interpretation, "character")
})

test_that("residuals_test() shapiro.w is between 0 and 1", {
  residuals <- rnorm(100)

  result <- residuals_test(residuals = residuals)

  expect_true(result$shapiro.w >= 0 & result$shapiro.w <= 1)
})

test_that("residuals_test() p.value is between 0 and 1", {
  residuals <- rnorm(100)

  result <- residuals_test(residuals = residuals)

  expect_true(result$p.value >= 0 & result$p.value <= 1)
})

test_that("residuals_test() interpretation is correct for normal residuals", {
  set.seed(123)
  residuals <- rnorm(100, mean = 0, sd = 1)

  result <- residuals_test(residuals = residuals)

  if (result$p.value > 0.05) {
    expect_equal(result$interpretation, "Residuals are normal")
  } else {
    expect_equal(result$interpretation, "Residuals are not normal")
  }
})

test_that("residuals_test() works with normal residuals", {
  set.seed(123)
  residuals <- rnorm(200, mean = 0, sd = 1)

  result <- residuals_test(residuals = residuals)

  expect_type(result, "list")
  # Normal residuals should likely have high p-value
  expect_true(result$p.value > 0.01)
})

test_that("residuals_test() works with non-normal residuals", {
  set.seed(123)
  # Exponential distribution (clearly non-normal)
  residuals <- rexp(200, rate = 1)

  result <- residuals_test(residuals = residuals)

  expect_type(result, "list")
  # Exponential residuals should likely fail normality test
  expect_true(result$p.value < 0.10)
})

test_that("residuals_test() handles small vectors", {
  # Minimum for Shapiro test is 3
  residuals <- c(-1, 0, 1)

  result <- residuals_test(residuals = residuals)

  expect_type(result, "list")
  expect_named(result, c("shapiro.w", "p.value", "interpretation"))
})

test_that("residuals_test() interpretation matches p-value threshold", {
  set.seed(123)
  residuals <- rnorm(100)

  result <- residuals_test(residuals = residuals)

  # Verify interpretation logic
  if (result$p.value > 0.05) {
    expect_match(result$interpretation, "normal")
    expect_match(result$interpretation, "are", ignore.case = FALSE)
  } else {
    expect_match(result$interpretation, "not normal")
  }
})

test_that("residuals_test() handles different distributions", {
  set.seed(123)

  # Normal
  res_normal <- rnorm(100)

  # Uniform
  res_uniform <- runif(100, -2, 2)

  # Bimodal
  res_bimodal <- c(rnorm(50, -2, 0.5), rnorm(50, 2, 0.5))

  r1 <- residuals_test(res_normal)
  r2 <- residuals_test(res_uniform)
  r3 <- residuals_test(res_bimodal)

  # All should produce valid output
  expect_type(r1, "list")
  expect_type(r2, "list")
  expect_type(r3, "list")

  # All should have valid test statistics
  expect_true(r1$shapiro.w >= 0 & r1$shapiro.w <= 1)
  expect_true(r2$shapiro.w >= 0 & r2$shapiro.w <= 1)
  expect_true(r3$shapiro.w >= 0 & r3$shapiro.w <= 1)
})

test_that("residuals_test() shapiro.w has no names", {
  residuals <- rnorm(100)

  result <- residuals_test(residuals = residuals)

  # names should be removed (line 29 in source)
  expect_null(names(result$shapiro.w))
})

test_that("residuals_test() works with model residuals", {
  data(plants_rf)

  residuals <- get_residuals(plants_rf)

  result <- residuals_test(residuals = residuals)

  expect_type(result, "list")
  expect_named(result, c("shapiro.w", "p.value", "interpretation"))
  expect_true(result$shapiro.w >= 0 & result$shapiro.w <= 1)
})

test_that("residuals_test() works with spatial model residuals", {
  data(plants_rf_spatial)

  residuals <- get_residuals(plants_rf_spatial)

  result <- residuals_test(residuals = residuals)

  expect_type(result, "list")
  expect_named(result, c("shapiro.w", "p.value", "interpretation"))
  expect_true(result$shapiro.w >= 0 & result$shapiro.w <= 1)
})

test_that("residuals_test() handles negative and positive residuals", {
  set.seed(123)
  residuals <- rnorm(100, mean = 0, sd = 2)

  result <- residuals_test(residuals = residuals)

  # Should work regardless of residual signs
  expect_type(result, "list")
  expect_true(any(residuals < 0))
  expect_true(any(residuals > 0))
})

test_that("residuals_test() handles large vectors", {
  set.seed(123)
  # Note: unlike residuals_diagnostics, this doesn't sample for large vectors
  residuals <- rnorm(1000, mean = 0, sd = 1)

  result <- residuals_test(residuals = residuals)

  expect_type(result, "list")
  expect_named(result, c("shapiro.w", "p.value", "interpretation"))
  expect_true(result$shapiro.w >= 0 & result$shapiro.w <= 1)
})

test_that("residuals_test() example from documentation works", {
  # From @examples in source
  result <- residuals_test(residuals = runif(100))

  expect_type(result, "list")
  expect_named(result, c("shapiro.w", "p.value", "interpretation"))
})
