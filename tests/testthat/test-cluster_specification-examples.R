test_that("`cluster_specification()` works", {
  cluster.spec <- cluster_specification(
    ips = c("10.0.1.40", "10.0.1.41"),
    cores = c(6, 4),
    user = Sys.info()[["user"]]
  )
  expect_type(cluster.spec, "list")
  expect_length(cluster.spec, sum(c(6, 4)))
})
