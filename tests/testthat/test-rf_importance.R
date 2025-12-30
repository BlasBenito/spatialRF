test_that("rf_importance() works with basic inputs", {
  data(plants_df, plants_xy, plants_distance)

  # Fit simple model with few predictors for speed
  model <- rf(
    data = plants_df[1:100, ],
    dependent.variable.name = "richness_species_vascular",
    predictor.variable.names = colnames(plants_df)[5:9],  # Only 5 predictors
    distance.matrix = plants_distance[1:100, 1:100],
    verbose = FALSE,
    seed = 1,
    n.cores = 1
  )

  model_with_importance <- rf_importance(
    model = model,
    xy = plants_xy[1:100, ],
    repetitions = 5,
    metric = "rmse",
    verbose = FALSE,
    seed = 1,
    n.cores = 1
  )

  expect_s3_class(model_with_importance, "rf")
  expect_s3_class(model_with_importance$importance$per.variable, "data.frame")
  expect_true("importance.cv" %in% colnames(model_with_importance$importance$per.variable))
  expect_true("importance.cv.mad" %in% colnames(model_with_importance$importance$per.variable))
  expect_true("importance.cv.percent" %in% colnames(model_with_importance$importance$per.variable))
  expect_true("importance.cv.percent.mad" %in% colnames(model_with_importance$importance$per.variable))
})

test_that("rf_importance() creates correct plot objects", {
  data(plants_df, plants_xy, plants_distance)

  model <- rf(
    data = plants_df[1:100, ],
    dependent.variable.name = "richness_species_vascular",
    predictor.variable.names = colnames(plants_df)[5:9],
    distance.matrix = plants_distance[1:100, 1:100],
    verbose = FALSE,
    seed = 1,
    n.cores = 1
  )

  model_with_importance <- rf_importance(
    model = model,
    xy = plants_xy[1:100, ],
    repetitions = 5,
    metric = "rmse",
    verbose = FALSE,
    seed = 1,
    n.cores = 1
  )

  expect_s3_class(model_with_importance$importance$cv.per.variable.plot, "ggplot")
  expect_s3_class(model_with_importance$importance$oob.per.variable.plot, "ggplot")
})

test_that("rf_importance() works with different metrics", {
  data(plants_df, plants_xy, plants_distance)

  model <- rf(
    data = plants_df[1:100, ],
    dependent.variable.name = "richness_species_vascular",
    predictor.variable.names = colnames(plants_df)[5:8],
    distance.matrix = plants_distance[1:100, 1:100],
    verbose = FALSE,
    seed = 1,
    n.cores = 1
  )

  model_r2 <- rf_importance(
    model = model,
    xy = plants_xy[1:100, ],
    repetitions = 5,
    metric = "r.squared",
    verbose = FALSE,
    seed = 1,
    n.cores = 1
  )

  expect_true("importance.cv" %in% colnames(model_r2$importance$per.variable))
  expect_equal(nrow(model_r2$importance$per.variable), 4)
})

test_that("rf_importance() respects seed parameter", {
  data(plants_df, plants_xy, plants_distance)

  model <- rf(
    data = plants_df[1:100, ],
    dependent.variable.name = "richness_species_vascular",
    predictor.variable.names = colnames(plants_df)[5:7],
    distance.matrix = plants_distance[1:100, 1:100],
    verbose = FALSE,
    seed = 1,
    n.cores = 1
  )

  model1 <- rf_importance(
    model = model,
    xy = plants_xy[1:100, ],
    repetitions = 5,
    metric = "rmse",
    verbose = FALSE,
    seed = 123,
    n.cores = 1
  )

  # Create fresh model for second run
  model2 <- rf(
    data = plants_df[1:100, ],
    dependent.variable.name = "richness_species_vascular",
    predictor.variable.names = colnames(plants_df)[5:7],
    distance.matrix = plants_distance[1:100, 1:100],
    verbose = FALSE,
    seed = 1,
    n.cores = 1
  )

  model2 <- rf_importance(
    model = model2,
    xy = plants_xy[1:100, ],
    repetitions = 5,
    metric = "rmse",
    verbose = FALSE,
    seed = 123,
    n.cores = 1
  )

  expect_equal(
    model1$importance$per.variable$importance.cv,
    model2$importance$per.variable$importance.cv
  )
})

test_that("rf_importance() validates xy input", {
  data(plants_df, plants_distance)

  model <- rf(
    data = plants_df[1:100, ],
    dependent.variable.name = "richness_species_vascular",
    predictor.variable.names = colnames(plants_df)[5:7],
    distance.matrix = plants_distance[1:100, 1:100],
    verbose = FALSE,
    seed = 1,
    n.cores = 1
  )

  expect_error(
    rf_importance(
      model = model,
      xy = NULL,
      verbose = FALSE
    ),
    "xy.*required"
  )
})

test_that("rf_importance() validates xy column names", {
  data(plants_df, plants_distance)

  model <- rf(
    data = plants_df[1:100, ],
    dependent.variable.name = "richness_species_vascular",
    predictor.variable.names = colnames(plants_df)[5:7],
    distance.matrix = plants_distance[1:100, 1:100],
    verbose = FALSE,
    seed = 1,
    n.cores = 1
  )

  xy_bad <- data.frame(a = 1:100, b = 1:100)

  expect_error(
    rf_importance(
      model = model,
      xy = xy_bad,
      repetitions = 5,
      verbose = FALSE
    ),
    "column names.*'x'.*'y'"
  )
})

test_that("rf_importance() validates xy and data dimensions", {
  data(plants_df, plants_xy, plants_distance)

  model <- rf(
    data = plants_df[1:100, ],
    dependent.variable.name = "richness_species_vascular",
    predictor.variable.names = colnames(plants_df)[5:7],
    distance.matrix = plants_distance[1:100, 1:100],
    verbose = FALSE,
    seed = 1,
    n.cores = 1
  )

  xy_wrong_size <- plants_xy[1:50, ]

  expect_error(
    rf_importance(
      model = model,
      xy = xy_wrong_size,
      repetitions = 5,
      verbose = FALSE
    ),
    "nrow\\(xy\\).*nrow\\(data\\)"
  )
})

test_that("rf_importance() computes all importance columns", {
  data(plants_df, plants_xy, plants_distance)

  model <- rf(
    data = plants_df[1:100, ],
    dependent.variable.name = "richness_species_vascular",
    predictor.variable.names = colnames(plants_df)[5:7],
    distance.matrix = plants_distance[1:100, 1:100],
    verbose = FALSE,
    seed = 1,
    n.cores = 1
  )

  model_with_importance <- rf_importance(
    model = model,
    xy = plants_xy[1:100, ],
    repetitions = 5,
    metric = "r.squared",
    verbose = FALSE,
    seed = 1,
    n.cores = 1
  )

  imp_df <- model_with_importance$importance$per.variable

  expect_true(all(!is.na(imp_df$importance.cv)))
  expect_true(all(!is.na(imp_df$importance.cv.mad)))
  expect_true(all(!is.na(imp_df$importance.cv.percent)))
  expect_true(all(!is.na(imp_df$importance.cv.percent.mad)))
  expect_type(imp_df$importance.cv, "double")
  expect_type(imp_df$importance.cv.mad, "double")
  expect_type(imp_df$importance.cv.percent, "double")
  expect_type(imp_df$importance.cv.percent.mad, "double")
})

test_that("rf_importance() preserves original model structure", {
  data(plants_df, plants_xy, plants_distance)

  model <- rf(
    data = plants_df[1:100, ],
    dependent.variable.name = "richness_species_vascular",
    predictor.variable.names = colnames(plants_df)[5:7],
    distance.matrix = plants_distance[1:100, 1:100],
    verbose = FALSE,
    seed = 1,
    n.cores = 1
  )

  num_trees_before <- model$num.trees
  response_before <- model$ranger.arguments$dependent.variable.name

  model_with_importance <- rf_importance(
    model = model,
    xy = plants_xy[1:100, ],
    repetitions = 5,
    metric = "rmse",
    verbose = FALSE,
    seed = 1,
    n.cores = 1
  )

  expect_equal(model_with_importance$num.trees, num_trees_before)
  expect_equal(
    model_with_importance$ranger.arguments$dependent.variable.name,
    response_before
  )
})

test_that("rf_importance() validates metric parameter", {
  data(plants_df, plants_xy, plants_distance)

  model <- rf(
    data = plants_df[1:100, ],
    dependent.variable.name = "richness_species_vascular",
    predictor.variable.names = colnames(plants_df)[5:7],
    distance.matrix = plants_distance[1:100, 1:100],
    verbose = FALSE,
    seed = 1,
    n.cores = 1
  )

  expect_error(
    rf_importance(
      model = model,
      xy = plants_xy[1:100, ],
      repetitions = 5,
      metric = "invalid_metric",
      verbose = FALSE
    ),
    "arg"
  )
})

test_that("rf_importance() uses stored xy when not provided", {
  data(plants_df, plants_xy, plants_distance)

  # Fit model with xy stored
  model <- rf(
    data = plants_df[1:100, ],
    dependent.variable.name = "richness_species_vascular",
    predictor.variable.names = colnames(plants_df)[5:7],
    distance.matrix = plants_distance[1:100, 1:100],
    verbose = FALSE,
    seed = 1,
    n.cores = 1
  )

  # Store xy in model
  model$ranger.arguments$xy <- plants_xy[1:100, ]

  # Call without xy parameter - should use stored xy
  model_with_importance <- rf_importance(
    model = model,
    repetitions = 5,
    metric = "rmse",
    verbose = FALSE,
    seed = 1,
    n.cores = 1
  )

  expect_s3_class(model_with_importance, "rf")
  expect_true("importance.cv" %in% colnames(model_with_importance$importance$per.variable))
})
