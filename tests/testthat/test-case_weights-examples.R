test_that("`case_weights()` works", {
  data <- data.frame(
    response = c(0, 0, 0, 1, 1)
  )

  w <- case_weights(
    data = data,
    dependent.variable.name = "response"
    )
  expect_length(w, nrow(data))
  expect_type(w, "double")
  expect_equal(sum(w[data$response == 1]), sum(w[data$response == 0]))
})
