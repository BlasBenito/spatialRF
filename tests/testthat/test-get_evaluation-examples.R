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
