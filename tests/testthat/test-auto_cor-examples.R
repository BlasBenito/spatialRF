test_that("`auto_cor()` works", {
  data(plant_richness_df)
  out <- auto_cor(x = plant_richness_df[, 5:21])
  expect_type(out, "list")
  expect_length(out, 3)
  expect_equal(ncol(out$cor), length(out$selected.variables))
  expect_named(out, c("cor", "selected.variables", "selected.variables.df"))
})
