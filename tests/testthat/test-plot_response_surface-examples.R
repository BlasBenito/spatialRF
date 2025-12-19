test_that("`plot_response_surface()` works", {
  data(plants_rf)
  p <- plot_response_surface(plants_rf)
  expect_s3_class(p, "ggplot")
})
