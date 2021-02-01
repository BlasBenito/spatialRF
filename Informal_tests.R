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
rf.spatial.repeat <- rf_spatial(model = rf.repeat)



































ranger.arguments <- list()
ranger.arguments$data <- plant_richness_df
ranger.arguments$dependent.variable.name <- "richness_species_vascular"
ranger.arguments$predictor.variable.names <- colnames(plant_richness_df)[8:21]
ranger.arguments$distance.matrix <- distance_matrix
ranger.arguments$distance.thresholds <- c(0, 100, 1000)

formula <- as.formula("richness_species_vascular ~ climate_hypervolume + climate_velocity_lgm_average + neighbors_count + neighbors_percent_shared_edge")

model <- rf(
  ranger.arguments =  ranger.arguments
)

ranger::treeInfo(object = model, tree = 1)


#trying interfaces to data
model <- rf(
  data = plant_richness_df,
  dependent.variable.name = "richness_species_vascular",
  predictor.variable.names = colnames(plant_richness_df)[5:21]
)
ranger::treeInfo(object = model, tree = 1)


model <- rf(
  data = plant_richness_df,
  ranger.arguments = list(formula = as.formula("richness_species_vascular ~ climate_hypervolume + climate_velocity_lgm_average + neighbors_count + neighbors_percent_shared_edge"
)))
ranger::treeInfo(object = model, tree = 1)

model <- rf(
  ranger.arguments = list(
    y = plant_richness_df$richness_species_vascular,
    x = plant_richness_df[, 5:21]
    )
  )




###################################################################
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
distance.thresholds <- c(0, 500, 1000)

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

model <- rf_spatial(
  data = data,
  dependent.variable.name = dependent.variable.name,
  predictor.variable.names = predictor.variable.names,
  distance.matrix = distance.matrix,
  distance.thresholds = distance.thresholds,
  method = "hengl",
  repetitions = 10
)
length(model$selection.spatial.predictors$names)
model$variable.importance$plot
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




