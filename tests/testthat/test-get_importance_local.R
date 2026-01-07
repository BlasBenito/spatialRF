test_that("get_importance_local() works with rf models", {
  data(plants_rf)
  data(plants_df)
  data(plants_predictors)

  x <- get_importance_local(plants_rf)

  expect_true(is.matrix(x) || is.data.frame(x))
  expect_equal(nrow(x), nrow(plants_df))
  expect_true(all(colnames(x) %in% plants_predictors))
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
  data(plants_predictors)

  x <- get_importance_local(plants_rf_spatial)

  expect_true(is.matrix(x) || is.data.frame(x))
  expect_equal(nrow(x), nrow(plants_df))
  # All values should be numeric
  if (is.data.frame(x)) {
    expect_true(all(sapply(x, is.numeric)))
  } else {
    expect_true(is.numeric(x))
  }
})

test_that("get_importance_local() works with rf_repeat models", {
  data(plants_df, plants_rf)

  # Create rf_repeat model
  model_repeat <- rf_repeat(
    model = plants_rf,
    repetitions = 5,
    verbose = FALSE
  )

  x <- get_importance_local(model_repeat)

  expect_true(is.matrix(x) || is.data.frame(x))
  expect_equal(nrow(x), nrow(plants_df))
  # All columns should be numeric
  expect_true(all(sapply(x, is.numeric)))
})


test_that("get_importance_local() handles spatial models with spatial predictors", {
  data(plants_rf_spatial)
  data(plants_df)
  data(plants_predictors)

  x <- get_importance_local(plants_rf_spatial)

  expect_true(is.matrix(x) || is.data.frame(x))
  expect_equal(nrow(x), nrow(plants_df))

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
