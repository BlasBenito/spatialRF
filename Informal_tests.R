#PLOTTING MAP
library(sf)
library(ggplot2)
library(patchwork)
library("rnaturalearth")
library("rnaturalearthdata")
data(plant_richness_sf)
data(plant_richness_df)
pr <- cbind(plant_richness_df, plant_richness_sf)

world <- ne_countries(scale = "medium", returnclass = "sf")

map <- ggplot() +
  geom_sf(data = world) +
  geom_sf(data = pr,
          aes(
            geometry = geom_centroids,
            fill = richness_species_vascular
          ),
          shape = 21,
          color = "black",
          size = 2.5
  ) +
  scale_fill_viridis_c(direction = -1) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.key.width=unit(1,"cm")
  ) +
  labs(fill = "Plant richness") +
  scale_x_continuous(limits = c(-170, -30)) +
  scale_y_continuous(limits = c(-58, 75))  +
  ggtitle("Plant richness of the American ecoregions")


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

#with repetitions
rf.repeat <- rf_repeat(model = rf.model, verbose = FALSE)

#spatial model
rf.spatial <- rf_spatial(model = rf.model, verbose = FALSE)

#from repeat
rf.spatial.repeat <- rf_spatial(model = rf.repeat, verbose = FALSE)

#trying rf_evaluate
rf.model <- rf_evaluate(
  model = rf.model,
  xy = plant_richness_df[, c("x", "y")],
  verbose = FALSE,
  metrics = "r.squared"
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


#rf compare
comparison <- rf_compare2(
models <- list(
  rf.model = rf.model,
  rf.repeat = rf.repeat,
  rf.spatial = rf.spatial,
  rf.spatial.repeat = rf.spatial.repeat
),
xy = plant_richness_df[, c("x", "y")],
metrics = "r.squared"
)



#rf_repeat sequential
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

#testing n.cores rf_repeat
rf.repeat <- rf_repeat(
  model = rf.model,
  verbose = FALSE,
  repetitions = 10,
  n.cores = 1
)

rf.repeat <- rf_repeat(
  model = rf.model,
  verbose = FALSE,
  repetitions = 100,
  n.cores = NULL
)

rf.repeat <- rf_repeat(
  model = rf.model,
  verbose = FALSE,
  repetitions = 100,
  n.cores = 6
)

rf.repeat <- rf_repeat(
  model = rf.model,
  verbose = FALSE,
  repetitions = 100,
  cluster.ips = c(
    "10.42.0.1",
    "10.42.0.34",
    "10.42.0.104"
  ),
  cluster.cores = c(7, 4, 4)
  )


#testing rf_interactions
interactions <- rf_interactions(
  data = plant_richness_df,
  dependent.variable.name = "richness_species_vascular",
  predictor.variable.names = colnames(plant_richness_df)[5:21],
  n.cores = 1
)

interactions <- rf_interactions(
  data = plant_richness_df,
  dependent.variable.name = "richness_species_vascular",
  predictor.variable.names = colnames(plant_richness_df)[5:21],
  n.cores = NULL
)

#stand-alone machine
interactions <- rf_interactions(
  data = plant_richness_df,
  dependent.variable.name = "richness_species_vascular",
  predictor.variable.names = colnames(plant_richness_df)[5:21],
  n.cores = 7
)

#in cluster
interactions <- rf_interactions(
  data = plant_richness_df,
  dependent.variable.name = "richness_species_vascular",
  predictor.variable.names = colnames(plant_richness_df)[5:21],
  verbose = FALSE,
  cluster.ips = c(
    "10.42.0.1",
    "10.42.0.34",
    "10.42.0.104"
  ),
  cluster.cores = c(7, 4, 4)
  )


#testing rf_evaluate
evaluation <- rf_evaluate(
  model = rf.model,
  repetitions = 1000,
  xy = plant_richness_df[, c("x", "y")],
  n.cores = 1
)

evaluation <- rf_evaluate(
  model = rf.model,
  repetitions = 1000,
  xy = plant_richness_df[, c("x", "y")],
  n.cores = NULL
)

evaluation <- rf_evaluate(
  model = rf.model,
  repetitions = 1000,
  xy = plant_richness_df[, c("x", "y")],
  n.cores = 6
)

evaluation <- rf_evaluate(
  model = rf.model,
  repetitions = 1000,
  xy = plant_richness_df[, c("x", "y")],
  cluster.ips = c(
    "10.42.0.1",
    "10.42.0.34",
    "10.42.0.104"
  ),
  cluster.cores = c(7, 4, 4)
)



spatial.predictors.df <- mem_multithreshold(
  x = distance_matrix,
  distance.thresholds = c(0, 100, 1000, 10000),
  max.spatial.predictors = NULL
)
rank <- rank_spatial_predictors(
  distance.matrix = distance_matrix,
  distance.thresholds = c(0, 100, 1000, 10000),
  spatial.predictors.df = spatial.predictors.df,
  ranking.method = "moran",
  n.cores = 1
)

rank <- rank_spatial_predictors(
  distance.matrix = distance_matrix,
  distance.thresholds = c(0, 100, 1000, 10000),
  spatial.predictors.df = spatial.predictors.df,
  ranking.method = "moran",
  n.cores = NULL
)

rank <- rank_spatial_predictors(
  distance.matrix = distance_matrix,
  distance.thresholds = c(0, 100, 1000, 10000),
  spatial.predictors.df = spatial.predictors.df,
  ranking.method = "moran",
  n.cores = 6
)

rank <- rank_spatial_predictors(
  distance.matrix = distance_matrix,
  distance.thresholds = c(0, 100, 1000, 10000),
  spatial.predictors.df = spatial.predictors.df,
  ranking.method = "moran",
  cluster.ips = c(
    "10.42.0.1",
    "10.42.0.34",
    "10.42.0.104"
  ),
  cluster.cores = c(7, 4, 4)
)

#SELECT SPATIAL PREDICTORS
data <- plant_richness_df
dependent.variable.name <- "richness_species_vascular"
predictor.variable.names <- colnames(plant_richness_df)[5:21]
distance.matrix <- distance_matrix
distance.thresholds <- c(1000)

model <- rf(
  data = data,
  dependent.variable.name = dependent.variable.name,
  predictor.variable.names = predictor.variable.names,
  distance.matrix = distance.matrix,
  distance.thresholds = distance.thresholds,
  verbose = FALSE
)

spatial.predictors <- mem_multithreshold(
  x = distance.matrix,
  distance.thresholds = distance.thresholds
)

spatial.predictors.ranked <- rank_spatial_predictors(
  distance.matrix = distance.matrix,
  distance.thresholds = distance.thresholds,
  spatial.predictors.df = spatial.predictors,
  ranking.method = "moran",
  reference.moran.i = model$spatial.correlation.residuals$max.moran,
  n.cores = NULL
)

selection <- select_spatial_predictors_sequential(
  data = data,
  dependent.variable.name = dependent.variable.name,
  predictor.variable.names = predictor.variable.names,
  distance.matrix = distance.matrix,
  distance.thresholds = distance.thresholds,
  spatial.predictors.df = spatial.predictors,
  spatial.predictors.ranking = spatial.predictors.ranked,
  n.cores = 1
)

selection <- select_spatial_predictors_sequential(
  data = data,
  dependent.variable.name = dependent.variable.name,
  predictor.variable.names = predictor.variable.names,
  distance.matrix = distance.matrix,
  distance.thresholds = distance.thresholds,
  spatial.predictors.df = spatial.predictors,
  spatial.predictors.ranking = spatial.predictors.ranked,
  n.cores = NULL
)

selection <- select_spatial_predictors_sequential(
  data = data,
  dependent.variable.name = dependent.variable.name,
  predictor.variable.names = predictor.variable.names,
  distance.matrix = distance.matrix,
  distance.thresholds = distance.thresholds,
  spatial.predictors.df = spatial.predictors,
  spatial.predictors.ranking = spatial.predictors.ranked,
  n.cores = 7
)

selection <- select_spatial_predictors_sequential(
  data = data,
  dependent.variable.name = dependent.variable.name,
  predictor.variable.names = predictor.variable.names,
  distance.matrix = distance.matrix,
  distance.thresholds = distance.thresholds,
  spatial.predictors.df = spatial.predictors,
  spatial.predictors.ranking = spatial.predictors.ranked,
  cluster.ips = c(
    "10.42.0.1",
    "10.42.0.34",
    "10.42.0.104"
  ),
  cluster.cores = c(7, 4, 4)
)


#testing rf tuning
tuning <- rf_tuning(
  data = plant_richness_df,
  dependent.variable.name = "richness_species_vascular",
  predictor.variable.names = colnames(plant_richness_df)[5:21],
  method = "spatial.cv",
  xy = plant_richness_df[, c("x", "y")],
  num.trees = c(500, 1000, 1500),
  mtry = c(1, 5, 10, 15),
  min.node.size = c(5, 10, 20),
  n.cores = 1
  )


tuning <- rf_tuning(
  data = plant_richness_df,
  dependent.variable.name = "richness_species_vascular",
  predictor.variable.names = colnames(plant_richness_df)[5:21],
  method = "spatial.cv",
  xy = plant_richness_df[, c("x", "y")],
  num.trees = c(500, 1000, 1500),
  mtry = c(1, 5, 10, 15),
  min.node.size = c(5, 10, 20),
  n.cores = 7
)

tuning <- rf_tuning(
  data = plant_richness_df,
  dependent.variable.name = "richness_species_vascular",
  predictor.variable.names = colnames(plant_richness_df)[5:21],
  method = "spatial.cv",
  xy = plant_richness_df[, c("x", "y")],
  num.trees = c(500, 1000, 1500),
  mtry = c(1, 5, 10, 15),
  min.node.size = c(5, 10, 20),
  n.cores = NULL
)

tuning <- rf_tuning(
  data = plant_richness_df,
  dependent.variable.name = "richness_species_vascular",
  predictor.variable.names = colnames(plant_richness_df)[5:21],
  method = "spatial.cv",
  xy = plant_richness_df[, c("x", "y")],
  num.trees = c(500, 1000, 1500),
  mtry = c(1, 5, 10, 15),
  min.node.size = c(5, 10, 20),
  cluster.ips = c(
    "10.42.0.1",
    "10.42.0.34",
    "10.42.0.104"
  ),
  cluster.cores = c(7, 4, 4)
)



tuning <- rf_tuning(
  data = plant_richness_df,
  dependent.variable.name = "richness_species_vascular",
  predictor.variable.names = colnames(plant_richness_df)[5:21],
  method = "oob",
  num.trees = c(500, 1000),
  mtry = c(1, 5, 10, 15),
  min.node.size = c(5, 10, 20),
  n.cores = 1
)


tuning <- rf_tuning(
  data = plant_richness_df,
  dependent.variable.name = "richness_species_vascular",
  predictor.variable.names = colnames(plant_richness_df)[5:21],
  method = "oob",
  num.trees = c(500, 1000, 1500),
  mtry = c(1, 5, 10, 15),
  min.node.size = c(5, 10, 20),
  n.cores = 7
)

tuning <- rf_tuning(
  data = plant_richness_df,
  dependent.variable.name = "richness_species_vascular",
  predictor.variable.names = colnames(plant_richness_df)[5:21],
  method = "spatial.cv",
  xy = plant_richness_df[, c("x", "y")],
  num.trees = c(500, 1000, 1500),
  mtry = c(1, 5, 10, 15),
  min.node.size = c(5, 10, 20),
  n.cores = NULL
)

tuning <- rf_tuning(
  data = plant_richness_df,
  dependent.variable.name = "richness_species_vascular",
  predictor.variable.names = colnames(plant_richness_df)[5:21],
  method = "spatial.cv",
  xy = plant_richness_df[, c("x", "y")],
  num.trees = c(500, 1000, 1500),
  mtry = c(1, 5, 10, 15),
  min.node.size = c(5, 10, 20),
  cluster.ips = c(
    "10.42.0.1",
    "10.42.0.34",
    "10.42.0.104"
  ),
  cluster.cores = c(7, 4, 4)
)
