#BASIC MODELS TO TEST OTHER THINGIES
#############################################
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
print_performance(rf.model)
print_importance(rf.model)



#with repetitions
rf.repeat <- rf_repeat(model = rf.model, verbose = TRUE)

#spatial model
rf.spatial <- rf_spatial(model = rf.model, verbose = TRUE)

#from repeat
rf.spatial.repeat <- rf_spatial(model = rf.repeat, verbose = FALSE)

#trying rf_evaluate
rf.model <- rf_evaluate(
  model = rf.model,
  xy = plant_richness_df[, c("x", "y")],
  verbose = FALSE
)
plot_evaluation(rf.model)
get_evaluation(rf.model)

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


#DETECTING INTERACTIONS
ranger.arguments <- NULL
#basic model
rf.interaction <- suggest_interactions(
  data = plant_richness_df,
  dependent.variable.name = "richness_species_vascular",
  predictor.variable.names = colnames(plant_richness_df)[5:21],
  verbose = TRUE
)

rf.interaction$selected

rf.interaction <- suggest_interactions(model = rf.model)
x <- rf.interaction$selected
rf.interaction$df


#RESPONSE SURFACES
p <- response_surfaces(
  model = rf.model
  )

p <- response_curves(
  model = rf.model
)

p <- response_surfaces(
  model = rf.repeat
)

p <- response_curves(
  model = rf.repeat
)

p <- response_surfaces(
  model = rf.spatial
)

p <- response_curves(
  model = rf.spatial
)


#rf_tuning
###############
data(plant_richness_df)
data(distance_matrix)

#oob method
tuning <- rf_tuning(
  data = plant_richness_df,
  dependent.variable.name = "richness_species_vascular",
  predictor.variable.names = colnames(plant_richness_df)[5:21],
  tuning.method = "oob",
  verbose = TRUE
)

#spatial.cv
tuning <- rf_tuning(
  data = plant_richness_df,
  dependent.variable.name = "richness_species_vascular",
  predictor.variable.names = colnames(plant_richness_df)[5:21],
  tuning.method = "spatial.cv",
  xy = plant_richness_df[, c("x", "y")],
  verbose = TRUE
)
