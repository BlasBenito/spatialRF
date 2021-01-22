test_that("`objects_size()` works", {
  x <- matrix(runif(100), 10, 10)
  y <- matrix(runif(10000), 100, 100)
  out <- objects_size()
  expect_s3_class(out, "data.frame")
  expect_named(out, c("Type", "Size", "Length/Rows", "Columns"))
})
