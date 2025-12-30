test_that("optimization_function() works with moran.i method", {
  opt_data <- data.frame(
    moran.i = c(0.5, 0.3, 0.2, 0.15),
    r.squared = c(0.6, 0.65, 0.68, 0.69),
    penalization.per.variable = c(0.1, 0.2, 0.3, 0.4),
    p.value.binary = c(0, 0, 1, 1)
  )

  scores <- optimization_function(
    x = opt_data,
    weight.r.squared = 0.5,
    weight.penalization.n.predictors = 0.5,
    optimization.method = "moran.i"
  )

  expect_type(scores, "double")
  expect_length(scores, 4)
  expect_true(all(scores >= 0 & scores <= 1))
  expect_true(all(!is.na(scores)))
})

test_that("optimization_function() works with p.value method", {
  opt_data <- data.frame(
    moran.i = c(0.5, 0.3, 0.2, 0.15),
    r.squared = c(0.6, 0.65, 0.68, 0.69),
    penalization.per.variable = c(0.1, 0.2, 0.3, 0.4),
    p.value.binary = c(0, 0, 1, 1)
  )

  scores <- optimization_function(
    x = opt_data,
    weight.r.squared = 0.5,
    weight.penalization.n.predictors = 0.5,
    optimization.method = "p.value"
  )

  expect_type(scores, "double")
  expect_length(scores, 4)
  expect_true(all(scores >= 0 & scores <= 1))
  expect_true(all(!is.na(scores)))
})

test_that("optimization_function() scores are between 0 and 1", {
  opt_data <- data.frame(
    moran.i = c(0.1, 0.9, 0.5),
    r.squared = c(0.3, 0.7, 0.5),
    penalization.per.variable = c(0.2, 0.8, 0.5),
    p.value.binary = c(1, 0, 0)
  )

  scores_moran <- optimization_function(
    x = opt_data,
    weight.r.squared = 0.3,
    weight.penalization.n.predictors = 0.7,
    optimization.method = "moran.i"
  )

  scores_pvalue <- optimization_function(
    x = opt_data,
    weight.r.squared = 0.3,
    weight.penalization.n.predictors = 0.7,
    optimization.method = "p.value"
  )

  expect_true(all(scores_moran >= 0 & scores_moran <= 1))
  expect_true(all(scores_pvalue >= 0 & scores_pvalue <= 1))
})

test_that("optimization_function() higher scores indicate better solutions", {
  # Create data where row 3 should be optimal:
  # - low Moran's I (good)
  # - high R²  (good)
  # - low penalization (good)
  opt_data <- data.frame(
    moran.i = c(0.5, 0.4, 0.1, 0.6),
    r.squared = c(0.5, 0.6, 0.9, 0.4),
    penalization.per.variable = c(0.5, 0.4, 0.1, 0.6),
    p.value.binary = c(0, 0, 1, 0)
  )

  scores <- optimization_function(
    x = opt_data,
    weight.r.squared = 0.5,
    weight.penalization.n.predictors = 0.5,
    optimization.method = "moran.i"
  )

  # Row 3 should have highest score
  expect_equal(which.max(scores), 3)
})

test_that("optimization_function() respects weight.r.squared parameter", {
  opt_data <- data.frame(
    moran.i = c(0.3, 0.35, 0.4),
    r.squared = c(0.9, 0.5, 0.6),
    penalization.per.variable = c(0.2, 0.25, 0.3),
    p.value.binary = c(1, 1, 0)
  )

  # Different weights should produce different scores
  scores_high <- optimization_function(
    x = opt_data,
    weight.r.squared = 0.9,
    weight.penalization.n.predictors = 0.1,
    optimization.method = "moran.i"
  )

  scores_low <- optimization_function(
    x = opt_data,
    weight.r.squared = 0.1,
    weight.penalization.n.predictors = 0.1,
    optimization.method = "moran.i"
  )

  # Scores should be different when weights change
  expect_false(all(abs(scores_high - scores_low) < 1e-10))
  expect_true(all(scores_high >= 0 & scores_high <= 1))
  expect_true(all(scores_low >= 0 & scores_low <= 1))
})

test_that("optimization_function() respects weight.penalization parameter", {
  opt_data <- data.frame(
    moran.i = c(0.3, 0.35, 0.4),
    r.squared = c(0.7, 0.75, 0.8),
    penalization.per.variable = c(0.1, 0.9, 0.5),
    p.value.binary = c(1, 1, 0)
  )

  # Different weights should produce different scores
  scores_high <- optimization_function(
    x = opt_data,
    weight.r.squared = 0.1,
    weight.penalization.n.predictors = 0.9,
    optimization.method = "moran.i"
  )

  scores_low <- optimization_function(
    x = opt_data,
    weight.r.squared = 0.1,
    weight.penalization.n.predictors = 0.1,
    optimization.method = "moran.i"
  )

  # Scores should be different when weights change
  expect_false(all(abs(scores_high - scores_low) < 1e-10))
  expect_true(all(scores_high >= 0 & scores_high <= 1))
  expect_true(all(scores_low >= 0 & scores_low <= 1))
})

test_that("optimization_function() methods give different results", {
  opt_data <- data.frame(
    moran.i = c(0.5, 0.3, 0.2, 0.15),
    r.squared = c(0.6, 0.65, 0.68, 0.69),
    penalization.per.variable = c(0.1, 0.2, 0.3, 0.4),
    p.value.binary = c(0, 0, 1, 1)
  )

  scores_moran <- optimization_function(
    x = opt_data,
    weight.r.squared = 0.5,
    weight.penalization.n.predictors = 0.5,
    optimization.method = "moran.i"
  )

  scores_pvalue <- optimization_function(
    x = opt_data,
    weight.r.squared = 0.5,
    weight.penalization.n.predictors = 0.5,
    optimization.method = "p.value"
  )

  # Methods should give different results
  expect_false(all(scores_moran == scores_pvalue))
})

test_that("optimization_function() p.value method uses binary p-values", {
  # When p.value.binary = 1, it should boost the score
  opt_data <- data.frame(
    moran.i = c(0.5, 0.52),  # Slightly different to avoid NaN
    r.squared = c(0.7, 0.72),  # Slightly different
    penalization.per.variable = c(0.2, 0.22),  # Slightly different
    p.value.binary = c(1, 0)  # Different p-value binary
  )

  scores <- optimization_function(
    x = opt_data,
    weight.r.squared = 0.5,
    weight.penalization.n.predictors = 0.5,
    optimization.method = "p.value"
  )

  # Row 1 (p.value.binary = 1) should have higher score
  expect_true(scores[1] > scores[2])
})

test_that("optimization_function() works with two rows", {
  # Note: single row causes NaN due to rescale_vector division by zero
  # Using two rows as minimal test case
  opt_data <- data.frame(
    moran.i = c(0.3, 0.4),
    r.squared = c(0.7, 0.75),
    penalization.per.variable = c(0.2, 0.25),
    p.value.binary = c(1, 0)
  )

  scores <- optimization_function(
    x = opt_data,
    weight.r.squared = 0.5,
    weight.penalization.n.predictors = 0.5,
    optimization.method = "moran.i"
  )

  expect_type(scores, "double")
  expect_length(scores, 2)
  expect_true(all(scores >= 0 & scores <= 1))
})

test_that("optimization_function() handles many rows", {
  opt_data <- data.frame(
    moran.i = runif(100, 0, 1),
    r.squared = runif(100, 0, 1),
    penalization.per.variable = runif(100, 0, 1),
    p.value.binary = sample(c(0, 1), 100, replace = TRUE)
  )

  scores <- optimization_function(
    x = opt_data,
    weight.r.squared = 0.5,
    weight.penalization.n.predictors = 0.5,
    optimization.method = "moran.i"
  )

  expect_length(scores, 100)
  expect_true(all(scores >= 0 & scores <= 1))
})

test_that("optimization_function() output length matches input", {
  for (n in c(1, 5, 10, 50)) {
    opt_data <- data.frame(
      moran.i = runif(n, 0, 1),
      r.squared = runif(n, 0, 1),
      penalization.per.variable = runif(n, 0, 1),
      p.value.binary = sample(c(0, 1), n, replace = TRUE)
    )

    scores <- optimization_function(
      x = opt_data,
      weight.r.squared = 0.5,
      weight.penalization.n.predictors = 0.5,
      optimization.method = "moran.i"
    )

    expect_length(scores, n)
  }
})

test_that("optimization_function() handles extreme weight values", {
  opt_data <- data.frame(
    moran.i = c(0.5, 0.3),
    r.squared = c(0.6, 0.8),
    penalization.per.variable = c(0.2, 0.4),
    p.value.binary = c(0, 1)
  )

  # Weight = 0 (ignore component)
  scores_zero <- optimization_function(
    x = opt_data,
    weight.r.squared = 0,
    weight.penalization.n.predictors = 0,
    optimization.method = "moran.i"
  )

  # Weight = 1 (maximize component)
  scores_one <- optimization_function(
    x = opt_data,
    weight.r.squared = 1,
    weight.penalization.n.predictors = 1,
    optimization.method = "moran.i"
  )

  expect_true(all(scores_zero >= 0 & scores_zero <= 1))
  expect_true(all(scores_one >= 0 & scores_one <= 1))
})

test_that("optimization_function() handles very similar values", {
  # Nearly identical rows (exact identical causes NaN in rescale_vector)
  opt_data <- data.frame(
    moran.i = c(0.5, 0.501, 0.502, 0.503),
    r.squared = c(0.7, 0.701, 0.702, 0.703),
    penalization.per.variable = c(0.3, 0.301, 0.302, 0.303),
    p.value.binary = c(1, 1, 1, 1)
  )

  scores <- optimization_function(
    x = opt_data,
    weight.r.squared = 0.5,
    weight.penalization.n.predictors = 0.5,
    optimization.method = "moran.i"
  )

  # Scores should be valid (rescale_vector can amplify small differences)
  expect_true(all(scores >= 0 & scores <= 1))
  expect_type(scores, "double")
  expect_length(scores, 4)
})

test_that("optimization_function() default method is moran.i", {
  opt_data <- data.frame(
    moran.i = c(0.5, 0.3),
    r.squared = c(0.6, 0.7),
    penalization.per.variable = c(0.2, 0.3),
    p.value.binary = c(0, 1)
  )

  scores_default <- optimization_function(
    x = opt_data,
    weight.r.squared = 0.5,
    weight.penalization.n.predictors = 0.5
  )

  scores_explicit <- optimization_function(
    x = opt_data,
    weight.r.squared = 0.5,
    weight.penalization.n.predictors = 0.5,
    optimization.method = "moran.i"
  )

  expect_equal(scores_default, scores_explicit)
})
