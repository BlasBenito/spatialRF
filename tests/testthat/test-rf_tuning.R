test_that("rf_tuning() validates model parameter", {
  # NULL model
  expect_error(
    rf_tuning(model = NULL, xy = data.frame(x = 1, y = 1)),
    "argument 'model' is empty"
  )
})

test_that("rf_tuning() validates xy column names", {
  data(plants_rf, plants_xy)

  # Wrong column names
  xy_wrong <- plants_xy
  names(xy_wrong) <- c("longitude", "latitude")

  expect_error(
    rf_tuning(
      model = plants_rf,
      xy = xy_wrong,
      verbose = FALSE
    ),
    "column names of 'xy' must be 'x' and 'y'"
  )
})

test_that("rf_tuning() validates xy row count", {
  data(plants_rf, plants_xy)

  # Different number of rows
  xy_subset <- plants_xy[1:100, ]

  expect_error(
    rf_tuning(
      model = plants_rf,
      xy = xy_subset,
      verbose = FALSE
    ),
    "nrow\\(xy\\) and nrow\\(data\\)"
  )
})

test_that("rf_tuning() works with basic parameters", {
  data(plants_df, plants_xy)

  model <- rf(
    data = plants_df[1:50, ],
    dependent.variable.name = "richness_species_vascular",
    predictor.variable.names = colnames(plants_df)[5:8],
    verbose = FALSE,
    n.cores = 1,
    seed = 1
  )

  result <- rf_tuning(
    model = model,
    xy = plants_xy[1:50, ],
    num.trees = 50,
    mtry = 2,
    min.node.size = 5,
    repetitions = 5,
    verbose = FALSE,
    n.cores = 1,
    seed = 1
  )

  expect_true("tuning" %in% names(result))
  expect_s3_class(result$tuning$tuning.df, "data.frame")
  expect_true("num.trees" %in% colnames(result$tuning$tuning.df))
  expect_true("mtry" %in% colnames(result$tuning$tuning.df))
  expect_true("min.node.size" %in% colnames(result$tuning$tuning.df))
})
