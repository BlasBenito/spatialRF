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
length(model$spatial.predictors$predictors.names)
model$spatial.correlation.residuals$max.moran
# 227
# -0.007

#hengl.moran.sequential
model <- rf_spatial(
  data = data,
  dependent.variable.name = dependent.variable.name,
  predictor.variable.names = predictor.variable.names,
  distance.matrix = distance.matrix,
  distance.thresholds = distance.thresholds,
  method = "hengl.moran.sequential"
)
length(model$spatial.predictors$predictors.names)
model$spatial.correlation.residuals$max.moran
# 28
# 0.0575


#hengl.effect.sequential
model <- rf_spatial(
  data = data,
  dependent.variable.name = dependent.variable.name,
  predictor.variable.names = predictor.variable.names,
  distance.matrix = distance.matrix,
  distance.thresholds = distance.thresholds,
  method = "hengl.effect.sequential"
)
length(model$spatial.predictors$predictors.names)
model$spatial.correlation.residuals$max.moran
# 49
# 0.0258

#hengl.effect.optimized WORKS POORLY (4 predictors)
model <- rf_spatial(
  data = data,
  dependent.variable.name = dependent.variable.name,
  predictor.variable.names = predictor.variable.names,
  distance.matrix = distance.matrix,
  distance.thresholds = distance.thresholds,
  method = "hengl.effect.optimized"
)
length(model$spatial.predictors$predictors.names)
model$spatial.correlation.residuals$max.moran
# 4
# 0.0939


#pca.moran.sequential WORKS POORLY (27 predictors)
model <- rf_spatial(
  data = data,
  dependent.variable.name = dependent.variable.name,
  predictor.variable.names = predictor.variable.names,
  distance.matrix = distance.matrix,
  distance.thresholds = distance.thresholds,
  method = "pca.moran.sequential"
)
length(model$spatial.predictors$predictors.names)
model$spatial.correlation.residuals$max.moran
# 27
# 0.083

#pca.effect.sequential
model <- rf_spatial(
  data = data,
  dependent.variable.name = dependent.variable.name,
  predictor.variable.names = predictor.variable.names,
  distance.matrix = distance.matrix,
  distance.thresholds = distance.thresholds,
  method = "pca.effect.sequential"
)
length(model$spatial.predictors$predictors.names)
model$spatial.correlation.residuals$max.moran
# 1
# 0.1358

#hengl.effect.optimized
model <- rf_spatial(
  data = data,
  dependent.variable.name = dependent.variable.name,
  predictor.variable.names = predictor.variable.names,
  distance.matrix = distance.matrix,
  distance.thresholds = distance.thresholds,
  method = "pca.effect.optimized"
)
length(model$spatial.predictors$predictors.names)
model$spatial.correlation.residuals$max.moran
# 2
# 0.1395

#mem.moran.sequential WORKS POORLY (27 predictors)
model <- rf_spatial(
  data = data,
  dependent.variable.name = dependent.variable.name,
  predictor.variable.names = predictor.variable.names,
  distance.matrix = distance.matrix,
  distance.thresholds = distance.thresholds,
  method = "mem.moran.sequential"
)
length(model$spatial.predictors$predictors.names)
model$spatial.correlation.residuals$max.moran
# 27
# 0.0811

#mem.effect.sequential
model <- rf_spatial(
  data = data,
  dependent.variable.name = dependent.variable.name,
  predictor.variable.names = predictor.variable.names,
  distance.matrix = distance.matrix,
  distance.thresholds = distance.thresholds,
  method = "mem.effect.sequential"
)
length(model$spatial.predictors$predictors.names)
model$spatial.correlation.residuals$max.moran
# 1
# 0.1378

#mem.effect.optimized
model <- rf_spatial(
  data = data,
  dependent.variable.name = dependent.variable.name,
  predictor.variable.names = predictor.variable.names,
  distance.matrix = distance.matrix,
  distance.thresholds = distance.thresholds,
  method = "mem.effect.optimized"
)
length(model$spatial.predictors$predictors.names)
model$spatial.correlation.residuals$max.moran
# 1
# 0.1813
