test_that("`moran()` works", {
   data(distance_matrix)
   data(plant_richness_df)
   out <- moran(
     x = plant_richness_df$richness_species_vascular,
     distance.matrix = distance_matrix
     )
   expect_type(out, "list")
   expect_s3_class(out, "data.frame")
   expect_length(out, 3)
   expect_named(out, c("moran.i", "p.value", "interpretation"))
})
