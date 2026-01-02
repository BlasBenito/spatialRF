test_that("`is_binary()` works", {
  data <- data.frame(
    binary.response = c(0, 0, 0, 1, 1),
    numeric.response = 1:5
  )
  x.1 <- is_binary(
    data = data,
    dependent.variable.name = "binary.response"
  )
  expect_equal(x.1, TRUE)
  x.2 <- is_binary(
    data = data,
    dependent.variable.name = "numeric.response"
  )
  expect_equal(x.2, FALSE)
})
