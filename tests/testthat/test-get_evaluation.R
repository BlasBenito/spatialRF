test_that("get_evaluation() validates input model class", {
  data(plants_rf)

  # Non-evaluated model should error
  expect_error(
    get_evaluation(plants_rf),
    "does not have an 'evaluation' slot"
  )
})

test_that("get_evaluation() works with evaluated models", {
  data(plants_rf, plants_xy)

  # Evaluate model
  model_evaluated <- rf_evaluate(
    model = plants_rf,
    xy = plants_xy,
    repetitions = 5,
    verbose = FALSE,
    n.cores = 1,
    seed = 1
  )

  result <- get_evaluation(model_evaluated)

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true("model" %in% colnames(result))
  expect_true("metric" %in% colnames(result))
  expect_true("mean" %in% colnames(result))
})

test_that("get_evaluation() returns correct structure", {
  data(plants_rf, plants_xy)

  model_evaluated <- rf_evaluate(
    model = plants_rf,
    xy = plants_xy,
    repetitions = 5,
    verbose = FALSE,
    n.cores = 1,
    seed = 1
  )

  result <- get_evaluation(model_evaluated)

  # Should have summary statistics columns
  expect_true(all(c("mean", "sd", "min", "max") %in% colnames(result)))

  # Should have numeric values
  expect_true(is.numeric(result$mean))
  expect_true(is.numeric(result$sd))
})
