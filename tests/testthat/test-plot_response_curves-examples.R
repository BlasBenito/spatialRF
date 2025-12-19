test_that("`plot_response_surfaces()` works", {
  data(plants_rf)
  p <- plot_response_curves(plants_rf)
  testthat::expect_true(
    inherits(p, "patchwork")
  )
})
