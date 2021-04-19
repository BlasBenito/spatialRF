test_that("`auc()` works", {
    out <- auc(
      o = sample(x = c(1, 0), size = 10, replace = TRUE),
      p = runif(10)
      )
    expect_length(out, 1)
    expect_type(out, "double")
})
