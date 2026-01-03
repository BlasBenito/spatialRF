test_that("`rf()` works", {
  data("plants_df")
  data("plants_distance")
  out <- rf(
    data = plants_df,
    dependent.variable.name = "richness_species_vascular",
    predictor.variable.names = colnames(plants_df)[5:21],
    distance.matrix = plants_distance,
    distance.thresholds = c(100, 100, 1000, 10000),
    verbose = FALSE
  )
  expect_s3_class(out, "rf")
  expect_s3_class(out$importance$per.variable, "data.frame")
  expect_named(out$importance$per.variable, c("variable", "importance"))
  expect_s3_class(out$residuals$autocorrelation$per.distance, "data.frame")
  expect_named(
    out$residuals$autocorrelation$per.distance,
    c(
      "distance.threshold",
      "moran.i",
      "moran.i.null",
      "p.value",
      "interpretation"
    )
  )
})

test_that("`rf()` works with scaled.importance = TRUE", {
  data("plants_df")
  data("plants_distance")
  out <- rf(
    data = plants_df,
    dependent.variable.name = "richness_species_vascular",
    predictor.variable.names = colnames(plants_df)[5:21],
    distance.matrix = plants_distance,
    distance.thresholds = c(100, 100, 1000),
    scaled.importance = TRUE,
    verbose = FALSE
  )
  expect_s3_class(out, "rf")
  expect_s3_class(out$importance$per.variable, "data.frame")
})

test_that("`rf()` works with binary response", {
  data("plants_df")
  data("plants_distance")
  # Create binary response
  plants_df$binary_response <- ifelse(
    plants_df$richness_species_vascular >
      median(plants_df$richness_species_vascular),
    1,
    0
  )
  out <- rf(
    data = plants_df,
    dependent.variable.name = "binary_response",
    predictor.variable.names = colnames(plants_df)[5:21],
    distance.matrix = plants_distance,
    distance.thresholds = c(100, 100, 1000),
    verbose = FALSE
  )
  expect_s3_class(out, "rf")
})
