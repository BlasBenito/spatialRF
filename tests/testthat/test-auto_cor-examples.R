test_that("`auto_cor()` works", {
  out <- auto_cor(x = plant_richness_df[, 5:20])
  expect_type(out, "list")
  expect_length(out, 2)
  expect_equal(ncol(out$cor), length(out$selected.variables))
  expect_named(out, c("cor", "selected.variables"))
})
