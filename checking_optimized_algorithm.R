data(plant_richness_df)
data(distance_matrix)

#basic model
rf.model <- rf(
  data = plant_richness_df,
  dependent.variable.name = "richness_species_vascular",
  predictor.variable.names = colnames(plant_richness_df)[5:21],
  distance.matrix = distance_matrix,
  distance.thresholds = c(0, 1000, 2000),
  seed = 50,
  verbose = FALSE
)

rf.spatial.optimized <- rf_spatial(model = rf.model, verbose = TRUE, method = "mem.effect.optimized")

rf.spatial.sequential <- rf_spatial(model = rf.model, verbose = TRUE)

comparison <- rf_compare(
  a = rf.spatial.optimized,
  b = rf.spatial.sequential,
  a.name = "optimized",
  b.name = "sequential",
  xy = plant_richness_df[, c("x", "y")],
  repetitions = 100,
  notch = TRUE
)


x11()
plot_optimization(rf.spatial.optimized)

x11()
plot_optimization(rf.spatial.sequential)

x11()
plot_importance(rf.spatial.optimized)

x11()
plot_importance(rf.spatial.sequential)

print_performance(rf.spatial.optimized)
print_performance(rf.spatial.sequential)
