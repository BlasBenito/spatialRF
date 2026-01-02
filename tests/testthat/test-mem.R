test_that("`mem()` works", {
  data(plants_distance)
  plants_distance.mem <- mem(plants_distance)
  expect_s3_class(plants_distance.mem, "data.frame")
  expect_equal("mem_1", colnames(plants_distance.mem)[1])
  expect_equal(nrow(plants_distance), nrow(plants_distance.mem))
})
