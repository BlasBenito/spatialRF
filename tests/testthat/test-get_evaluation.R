test_that("`get_evaluation()` works", {
  data(plants_df)
  data(plants_distance)
  rf.model <- rf(
    data = plants_df,
    dependent.variable.name = "richness_species_vascular",
    predictor.variable.names = colnames(plants_df)[5:21],
    distance.matrix = plants_distance,
    distance.thresholds = c(
      0,
      1000,
      2000
    ),
    verbose = FALSE
  )
  rf.model <- rf_evaluate(
    model = rf.model,
    xy = plants_df[,
      c("x", "y")
    ],
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
})


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
    verbose = FALSE
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
    verbose = FALSE
  )

  result <- get_evaluation(model_evaluated)

  # Should have summary statistics columns
  expect_true(all(c("mean", "sd", "min", "max") %in% colnames(result)))

  # Should have numeric values
  expect_true(is.numeric(result$mean))
  expect_true(is.numeric(result$sd))
})
