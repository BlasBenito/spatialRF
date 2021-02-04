#BASIC MODELS TO TEST OTHER THINGIES
#############################################
data(plant_richness_df)
data(distance.matrix)

#basic model
rf.model <- rf(
  data = plant_richness_df,
  dependent.variable.name = "richness_species_vascular",
  predictor.variable.names = colnames(plant_richness_df)[5:21],
  distance.matrix = distance_matrix,
  distance.thresholds = c(0, 1000, 2000),
  verbose = TRUE
)

#with repetitions
rf.repeat <- rf_repeat(model = rf.model, verbose = TRUE)

#spatial model
rf.spatial <- rf_spatial(model = rf.model, verbose = TRUE)

#from repeat
rf.spatial.repeat <- rf_spatial(model = rf.repeat, verbose = FALSE)

#trying rf_evaluate
rf.model <- rf_evaluate(
  model = rf.model,
  xy = plant_richness_df[, c("x", "y")]
)
plot_evaluation(rf.model)

rf.repeat <- rf_evaluate(
  model = rf.repeat,
  xy = plant_richness_df[, c("x", "y")]
)
plot_evaluation(rf.repeat)

rf.spatial <- rf_evaluate(
  model = rf.spatial,
  xy = plant_richness_df[, c("x", "y")]
)
plot_evaluation(rf.spatial)

rf.spatial.repeat <- rf_evaluate(
  model = rf.spatial.repeat,
  xy = plant_richness_df[, c("x", "y")]
)
plot_evaluation(rf.spatial.repeat)










