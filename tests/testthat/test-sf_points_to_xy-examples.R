test_that("`sf_points_to_xy()` works", {
  data(plant_richness_sf)
  xy <- sf_points_to_xy(plant_richness_sf)
  expect_s3_class(xy, "data.frame")
  expect_named(xy, c("x", "y"))
  expect_equal(nrow(plant_richness_sf), nrow(xy))
})
