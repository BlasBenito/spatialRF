#loading example data
data(distance_matrix)
data(plant_richness_df)

#response and preditor names
dependent.variable.name = "richness_species_vascular"
predictor.variable.names = colnames(plant_richness_df)[5:21]

plant_richness_df <- plant_richness_df[1:100, ]
distance_matrix <- distance_matrix[1:100, 1:100]

#non-spatial model
model <- rf(
  data = plant_richness_df[1:100, ],
  dependent.variable.name = dependent.variable.name,
  predictor.variable.names = predictor.variable.names,
  distance.matrix = distance_matrix,
  distance.thresholds = 0
)

#preparing spatial predictors
spatial.predictors <- mem_multithreshold(
  distance.matrix = distance_matrix,
  distance.thresholds = 0
)

#ranking spatial predictors
spatial.predictors.ranking <- rank_spatial_predictors(
  data = plant_richness_df,
  dependent.variable.name = dependent.variable.name,
  predictor.variable.names = predictor.variable.names,
  spatial.predictors.df = spatial.predictors,
  ranking.method = "moran",
  reference.moran.i = model$spatial.correlation.residuals$max.moran,
  distance.matrix = distance_matrix,
  distance.thresholds = 0
)

#selecting the best subset of predictors
selection <- select_spatial_predictors_recursive(
  data = plant_richness_df,
  dependent.variable.name = dependent.variable.name,
  predictor.variable.names = predictor.variable.names,
  distance.matrix = distance_matrix,
  distance.thresholds = 0,
  spatial.predictors.df = spatial.predictors,
  spatial.predictors.ranking = spatial.predictors.ranking
)

selection$optimization
selection$best.spatial.predictors
plot_optimization(selection$optimization)
