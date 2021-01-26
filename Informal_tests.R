data("distance_matrix")
data("plant_richness_df")

data <- plant_richness_df
dependent.variable.name <- "richness_species_vascular"
predictor.variable.names <- colnames(plant_richness_df)[5:21]
distance.matrix <- distance_matrix
distance.thresholds <- c(0, 100, 1000)

model <- rf(
  data = data,
  dependent.variable.name = dependent.variable.name,
  predictor.variable.names = predictor.variable.names,
  distance.matrix = distance.matrix,
  distance.thresholds = distance.thresholds,
  seed = 10
)
model$spatial.correlation.residuals$max.moran

spatial.predictors <- pca_distance_matrix(
  x = distance.matrix,
  distance.thresholds = distance.thresholds
)

spatial.predictors.ranking <- rank_spatial_predictors(
  data = data,
  dependent.variable.name = dependent.variable.name,
  predictor.variable.names = predictor.variable.names,
  spatial.predictors.df = spatial.predictors,
  ranking.method = "moran.i.reduction",
  reference.moran.i = model$spatial.correlation.residuals$max.moran,
  distance.matrix = distance.matrix,
  distance.thresholds = distance.thresholds,
  n.cores = 1,
  multicollinearity.filter = "vif"
)

selection <- select_spatial_predictors_optimized(
  data = data,
  dependent.variable.name = dependent.variable.name,
  predictor.variable.names = predictor.variable.names,
  distance.matrix = distance.matrix,
  distance.thresholds = distance.thresholds,
  spatial.predictors.df = spatial.predictors,
  spatial.predictors.ranking = spatial.predictors.ranking,
  n.cores = 1
)
selection$best.spatial.predictors
selection$optimization

#####################################
#select_spatial_predictors_optimized
data("distance_matrix")
data("plant_richness_df")
data <- plant_richness_df
dependent.variable.name <- "richness_species_vascular"
predictor.variable.names <- colnames(plant_richness_df)[5:21]
distance.matrix <- distance_matrix
distance.thresholds <- c(0, 100, 1000)
model <- rf(
  data = data,
  dependent.variable.name = dependent.variable.name,
  predictor.variable.names = predictor.variable.names,
  distance.matrix = distance.matrix,
  distance.thresholds = distance.thresholds
)
spatial.predictors.df <- mem_multithreshold(
  x = distance.matrix,
  distance.thresholds = distance.thresholds
)
spatial.predictors.ranking <- rank_spatial_predictors(
  data = data,
  dependent.variable.name = dependent.variable.name,
  predictor.variable.names = predictor.variable.names,
  spatial.predictors.df = spatial.predictors.df,
  ranking.method = "moran.i.reduction",
  reference.moran.i = model$spatial.correlation.residuals$max.moran,
  distance.matrix = distance.matrix,
  distance.thresholds = distance.thresholds,
  n.cores = 1
)
selection <- select_spatial_predictors_optimized(
  data = data,
  dependent.variable.name = dependent.variable.name,
  predictor.variable.names = predictor.variable.names,
  distance.matrix = distance.matrix,
  distance.thresholds = distance.thresholds,
  spatial.predictors.df = spatial.predictors.df,
  spatial.predictors.ranking = spatial.predictors.ranking,
  n.cores = 1
)


##################################
data <- plant_richness_df
dependent.variable.name <- "richness_species_vascular"
predictor.variable.names <- colnames(plant_richness_df)[5:21]
distance.matrix <- distance_matrix
distance.thresholds <- c(0, 100, 1000)

#hengl
model <- rf_spatial(
  data = data,
  dependent.variable.name = dependent.variable.name,
  predictor.variable.names = predictor.variable.names,
  distance.matrix = distance.matrix,
  distance.thresholds = distance.thresholds,
  method = "hengl"
)
length(model$selection.spatial.predictors$names)
model$spatial.correlation.residuals$df

#SEQUENTIAL METHODS

#hengl.moran.sequential
model <- rf_spatial(
  data = data,
  dependent.variable.name = dependent.variable.name,
  predictor.variable.names = predictor.variable.names,
  distance.matrix = distance.matrix,
  distance.thresholds = distance.thresholds,
  method = "hengl.moran.sequential",
  seed = 10
)
length(model$selection.spatial.predictors$names)
model$spatial.correlation.residuals$df

#pca.moran.sequential (doesn't work well)
model <- rf_spatial(
  data = data,
  dependent.variable.name = dependent.variable.name,
  predictor.variable.names = predictor.variable.names,
  distance.matrix = distance.matrix,
  distance.thresholds = distance.thresholds,
  method = "pca.moran.sequential",
  seed = 10
)
length(model$selection.spatial.predictors$names)
model$spatial.correlation.residuals$df

#mem.moran.sequential
model <- rf_spatial(
  data = data,
  dependent.variable.name = dependent.variable.name,
  predictor.variable.names = predictor.variable.names,
  distance.matrix = distance.matrix,
  distance.thresholds = distance.thresholds,
  method = "mem.moran.sequential",
  seed = 10
)
length(model$selection.spatial.predictors$names)
model$spatial.correlation.residuals$df


#hengl.effect.sequential
model <- rf_spatial(
  data = data,
  dependent.variable.name = dependent.variable.name,
  predictor.variable.names = predictor.variable.names,
  distance.matrix = distance.matrix,
  distance.thresholds = distance.thresholds,
  method = "hengl.effect.sequential",
  seed = 10
)
length(model$selection.spatial.predictors$names)
model$spatial.correlation.residuals$df


model <- rf_spatial(
  data = data,
  dependent.variable.name = dependent.variable.name,
  predictor.variable.names = predictor.variable.names,
  distance.matrix = distance.matrix,
  distance.thresholds = distance.thresholds,
  method = "pca.effect.sequential",
  seed = 10
)
length(model$selection.spatial.predictors$names)
model$spatial.correlation.residuals$df

#mem.effect.sequential
model <- rf_spatial(
  data = data,
  dependent.variable.name = dependent.variable.name,
  predictor.variable.names = predictor.variable.names,
  distance.matrix = distance.matrix,
  distance.thresholds = distance.thresholds,
  method = "mem.effect.sequential",
  seed = 10
)
length(model$selection.spatial.predictors$names)
model$spatial.correlation.residuals$df



#OPTIMIZED METHODS
model <- rf_spatial(
  data = data,
  dependent.variable.name = dependent.variable.name,
  predictor.variable.names = predictor.variable.names,
  distance.matrix = distance.matrix,
  distance.thresholds = distance.thresholds,
  method = "hengl.effect.optimized",
  seed = 10
)
length(model$selection.spatial.predictors$names)
model$spatial.correlation.residuals$df
model$selection.spatial.predictors$df


#works awful, better remove this option!
model <- rf_spatial(
  data = data,
  dependent.variable.name = dependent.variable.name,
  predictor.variable.names = predictor.variable.names,
  distance.matrix = distance.matrix,
  distance.thresholds = distance.thresholds,
  method = "pca.effect.optimized",
  seed = 10
)
length(model$selection.spatial.predictors$names)
model$spatial.correlation.residuals$df
model$selection.spatial.predictors$df


#works great
model <- rf_spatial(
  data = data,
  dependent.variable.name = dependent.variable.name,
  predictor.variable.names = predictor.variable.names,
  distance.matrix = distance.matrix,
  distance.thresholds = distance.thresholds,
  method = "mem.effect.optimized",
  seed = 10
)
length(model$selection.spatial.predictors$names)
model$spatial.correlation.residuals$df
model$selection.spatial.predictors$df








