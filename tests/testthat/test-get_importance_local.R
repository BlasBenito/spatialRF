test_that("`get_importance_local()` works", {
  data(plants_df)
  data(plants_distance)
  rf.model <- rf(
    data = plants_df,
    dependent.variable.name = "richness_species_vascular",
    predictor.variable.names = colnames(plants_df)[5:21],
    distance.matrix = plants_distance,
    distance.thresholds = c(100, 1000, 2000),
    verbose = FALSE
  )
  x <- get_importance_local(rf.model)
  expect_true(is.matrix(x) || is.data.frame(x))
  expect_equal(nrow(x), nrow(plants_df))
  expect_true(all(colnames(plants_df)[5:21] %in% colnames(x)))
  # All values should be numeric
  if (is.data.frame(x)) {
    expect_true(all(sapply(x, is.numeric)))
  } else {
    expect_true(is.numeric(x))
  }
})

test_that("get_importance_local() works with rf models", {
  data(plants_rf)
  data(plants_df)

  x <- get_importance_local(plants_rf)

  expect_true(is.matrix(x) || is.data.frame(x))
  expect_equal(nrow(x), nrow(plants_df))
  expect_true(all(colnames(plants_df)[5:21] %in% colnames(x)))
  # All values should be numeric
  if (is.data.frame(x)) {
    expect_true(all(sapply(x, is.numeric)))
  } else {
    expect_true(is.numeric(x))
  }
})

test_that("get_importance_local() works with rf_spatial models", {
  data(plants_rf_spatial)
  data(plants_df)

  x <- get_importance_local(plants_rf_spatial)

  expect_true(is.matrix(x) || is.data.frame(x))
  expect_equal(nrow(x), nrow(plants_df))
  expect_true(all(colnames(plants_df)[5:21] %in% colnames(x)))
  # All values should be numeric
  if (is.data.frame(x)) {
    expect_true(all(sapply(x, is.numeric)))
  } else {
    expect_true(is.numeric(x))
  }
})

test_that("get_importance_local() works with rf_repeat models", {
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

  x <- get_importance_local(model_repeat)

  expect_true(is.matrix(x) || is.data.frame(x))
  expect_equal(nrow(x), 100)
  expect_true(all(colnames(plants_df)[5:11] %in% colnames(x)))
  # All columns should be numeric
  expect_true(all(sapply(x, is.numeric)))
})

test_that("get_importance_local() returns correct structure", {
  data(plants_rf)
  data(plants_df)

  x <- get_importance_local(plants_rf)

  # Should have one row per observation
  expect_equal(nrow(x), nrow(plants_df))

  # Should have one column per predictor variable
  expect_equal(ncol(x), 17)

  # All importance values should be numeric
  expect_true(all(is.numeric(x)))
})

test_that("get_importance_local() handles spatial models with spatial predictors", {
  data(plants_rf_spatial)
  data(plants_df)

  x <- get_importance_local(plants_rf_spatial)

  expect_true(is.matrix(x) || is.data.frame(x))
  expect_equal(nrow(x), nrow(plants_df))

  # Should include original predictors
  expect_true(all(colnames(plants_df)[5:21] %in% colnames(x)))
  # All values should be numeric
  if (is.data.frame(x)) {
    expect_true(all(sapply(x, is.numeric)))
  } else {
    expect_true(is.numeric(x))
  }
})

test_that("get_importance_local() extracts correct variable names", {
  data(plants_rf)

  x <- get_importance_local(plants_rf)

  # Column names should be predictor variable names
  expect_true(all(!is.na(colnames(x))))
  expect_true(all(nchar(colnames(x)) > 0))
  expect_equal(length(unique(colnames(x))), ncol(x))
})

test_that("get_importance_local() works with models with few predictors", {
  data(plants_df, plants_distance)

  # Fit model with only 3 predictors
  model_small <- rf(
    data = plants_df[1:100, ],
    dependent.variable.name = "richness_species_vascular",
    predictor.variable.names = colnames(plants_df)[5:7],
    distance.matrix = plants_distance[1:100, 1:100],
    verbose = FALSE
  )

  x <- get_importance_local(model_small)

  expect_true(is.matrix(x) || is.data.frame(x))
  expect_equal(nrow(x), 100)
  expect_equal(ncol(x), 3)
  # All values should be numeric
  if (is.data.frame(x)) {
    expect_true(all(sapply(x, is.numeric)))
  } else {
    expect_true(is.numeric(x))
  }
})

test_that("get_importance_local() works with models with many predictors", {
  data(plants_rf)

  x <- get_importance_local(plants_rf)

  expect_true(is.matrix(x) || is.data.frame(x))
  expect_true(ncol(x) > 10)
  # All values should be numeric
  if (is.data.frame(x)) {
    expect_true(all(sapply(x, is.numeric)))
  } else {
    expect_true(is.numeric(x))
  }
})

test_that("get_importance_local() preserves all predictor names", {
  data(plants_rf)
  data(plants_df)

  x <- get_importance_local(plants_rf)

  # All predictor variable names should be preserved
  expected_names <- colnames(plants_df)[5:21]
  expect_true(all(expected_names %in% colnames(x)))
})

test_that("get_importance_local() importance values are valid", {
  data(plants_rf)

  x <- get_importance_local(plants_rf)

  # All importance values should be numeric and not NA/NaN
  if (is.data.frame(x)) {
    expect_true(all(sapply(x, function(col) all(!is.na(col)))))
    expect_true(all(sapply(x, function(col) all(is.finite(col)))))
  } else {
    expect_true(all(!is.na(x)))
    expect_true(all(is.finite(x)))
  }
})

test_that("get_importance_local() works with different model types", {
  data(plants_rf, plants_rf_spatial)
  data(plants_df)

  x1 <- get_importance_local(plants_rf)
  x2 <- get_importance_local(plants_rf_spatial)

  # Both should produce valid matrices/data frames
  expect_true(is.matrix(x1) || is.data.frame(x1))
  expect_true(is.matrix(x2) || is.data.frame(x2))
  expect_equal(nrow(x1), nrow(plants_df))
  expect_equal(nrow(x2), nrow(plants_df))
  # All values should be numeric
  if (is.data.frame(x1)) {
    expect_true(all(sapply(x1, is.numeric)))
  } else {
    expect_true(is.numeric(x1))
  }
  if (is.data.frame(x2)) {
    expect_true(all(sapply(x2, is.numeric)))
  } else {
    expect_true(is.numeric(x2))
  }
})
