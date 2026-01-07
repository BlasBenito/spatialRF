test_that("rf_importance() works with basic inputs", {
  data(plants_rf)

  x <- rf_importance(
    model = plants_rf,
    repetitions = 5,
    metric = "rmse",
    verbose = FALSE
  )

  expect_s3_class(x, "rf")
  expect_s3_class(x$importance$per.variable, "data.frame")
  expect_true(
    "importance.cv" %in% colnames(x$importance$per.variable)
  )
  expect_true(
    "importance.cv.mad" %in%
      colnames(x$importance$per.variable)
  )
  expect_true(
    "importance.cv.percent" %in%
      colnames(x$importance$per.variable)
  )
  expect_true(
    "importance.cv.percent.mad" %in%
      colnames(x$importance$per.variable)
  )

  expect_s3_class(
    x$importance$cv.per.variable.plot,
    "ggplot"
  )
  expect_s3_class(
    x$importance$oob.per.variable.plot,
    "ggplot"
  )
})
