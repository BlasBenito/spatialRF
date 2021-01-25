test_that("`mem()` works", {
  data(distance_matrix)
  distance_matrix.mem <- mem(x = distance_matrix)
  expect_s3_class(distance_matrix.mem, "data.frame")
  expect_equal("mem_1", colnames(distance_matrix.mem)[1])
  expect_equal(nrow(distance_matrix), nrow(distance_matrix.mem))
})
