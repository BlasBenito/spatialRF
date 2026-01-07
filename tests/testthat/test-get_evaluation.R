test_that("`get_evaluation()` works", {
  data(plants_rf)
  data(plants_xy)

  rf.model <- rf_evaluate(
    model = plants_rf,
    xy = plants_xy,
    repetitions = 5,
    verbose = FALSE
  )
  x <- get_evaluation(rf.model)
  expect_s3_class(x, "data.frame")
  expect_named(
    x,
    c(
      "model",
      "metric",
      "median",
      "median_absolute_deviation",
      "q1",
      "q3",
      "mean",
      "se",
      "sd",
      "min",
      "max"
    )
  )

  expect_true("model" %in% colnames(x))
  expect_true("metric" %in% colnames(x))
  expect_true("mean" %in% colnames(x))

  # Should have summary statistics columns
  expect_true(all(c("mean", "sd", "min", "max") %in% colnames(x)))

  # Should have numeric values
  expect_true(is.numeric(x$mean))
  expect_true(is.numeric(x$sd))
})


test_that("get_evaluation() validates input model class", {
  data(plants_rf)

  # Non-evaluated model should error
  expect_error(
    get_evaluation(plants_rf),
    "does not have an 'evaluation' slot"
  )
})
