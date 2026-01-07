test_that("rf_compare() works with basic inputs", {
  data(plants_rf, plants_rf_spatial, plants_xy)

  comparison <- rf_compare(
    models = list(
      model1 = plants_rf,
      model2 = plants_rf_spatial,
      model3 = plants_rf_spatial
    ),
    xy = plants_xy,
    repetitions = 5,
    metrics = c("rmse", "r.squared"),
    verbose = FALSE
  )

  expect_type(comparison, "list")
  expect_named(comparison, c("comparison.df", "spatial.folds", "plot"))
  expect_s3_class(comparison$comparison.df, "data.frame")
  expect_s3_class(comparison$plot, "ggplot")
  expect_true(nrow(comparison$comparison.df) > 0)
  expect_equal(length(comparison$spatial.folds), 5)
  expect_true(all(
    c("rmse", "r.squared") %in% unique(comparison$comparison.df$metric)
  ))
  expect_equal(length(unique(comparison$comparison.df$metric)), 2)
  expect_equal(length(unique(comparison$comparison.df$model)), 3)
  expect_true(all(
    c("model1", "model2", "model3") %in% unique(comparison$comparison.df$model)
  ))

  expect_named(comparison$comparison.df, c("metric", "value", "model"))
  expect_type(comparison$comparison.df$metric, "character")
  expect_type(comparison$comparison.df$value, "double")
  expect_type(comparison$comparison.df$model, "character")
  expect_true(all(!is.na(comparison$comparison.df$value)))
})
