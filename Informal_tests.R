library(spatialRF)
library(magrittr)

#loading the example data
data(plant_richness_df)
data("distance_matrix")
xy <- plant_richness_df[, c("x", "y")]
dependent.variable.name <- "richness_species_vascular"
predictor.variable.names <- colnames(plant_richness_df)[5:21]
distance.matrix <- distance_matrix

m <- rf(
  data = plant_richness_df,
  dependent.variable.name = dependent.variable.name,
  predictor.variable.names = predictor.variable.names,
  distance.matrix = distance_matrix,
  xy = xy
) %>%
  rf_importance()

plot_importance(m)

m3 <- rf(
  data = m2$ranger.arguments$data,
  dependent.variable.name = dependent.variable.name,
  predictor.variable.names = m2$importance.cv$predictors.with.positive.effect,
  distance.matrix = distance_matrix,
  xy = xy
) %>%
  rf_importance()

m4 <- rf(
  data = m3$ranger.arguments$data,
  dependent.variable.name = dependent.variable.name,
  predictor.variable.names = m3$importance.cv$predictors.with.positive.effect,
  distance.matrix = distance_matrix,
  xy = xy
) %>%
  rf_importance()




#rf importance

#cluster without pipes
##################################
my.cluster <- parallel::makeCluster(
  7,
  type = "PSOCK"
)

doParallel::registerDoParallel(cl = my.cluster)

m <- rf(
  data = plant_richness_df,
  dependent.variable.name = dependent.variable.name,
  predictor.variable.names = predictor.variable.names,
  distance.matrix = distance_matrix,
  xy = xy,
  cluster = my.cluster
) %>%
  rf_importance()

m <- rf_spatial(model = m)

m <- rf_tuning(model = m)

m <- rf_evaluate(model = m, grow.testing.folds = FALSE )

m <- rf_repeat(model = m)

parallel::stopCluster(cl = my.cluster)


#chaining cluster with pipes
##################################
my.cluster <- parallel::makeCluster(
  7,
  type = "PSOCK"
)

doParallel::registerDoParallel(cl = my.cluster)

  m <- rf(
    data = plant_richness_df,
    dependent.variable.name = dependent.variable.name,
    predictor.variable.names = predictor.variable.names,
    distance.matrix = distance_matrix,
    xy = xy,
    cluster = my.cluster
  ) %>%
  rf_spatial() %>%
  rf_tuning() %>%
  rf_evaluate() %>%
  rf_repeat()

parallel::stopCluster(cl = my.cluster)

#using beowulf cluster
#######################################
beowulf.cluster <- beowulf_cluster(
  cluster.ips = c(
    "10.42.0.1",
    "10.42.0.34",
    "10.42.0.104"
  ),
  cluster.cores = c(7, 4, 4),
  cluster.user = "blas"
)

doParallel::registerDoParallel(cl = beowulf.cluster)

m <- rf(
  data = plant_richness_df,
  dependent.variable.name = dependent.variable.name,
  predictor.variable.names = predictor.variable.names,
  distance.matrix = distance_matrix,
  xy = xy,
  cluster = my.cluster
) %>%
  rf_spatial() %>%
  rf_tuning() %>%
  rf_evaluate() %>%
  rf_repeat()

parallel::stopCluster(cl = beowulf.cluster)







#without cluster
m <- rf(
  data = plant_richness_df,
  dependent.variable.name = dependent.variable.name,
  predictor.variable.names = predictor.variable.names,
  distance.matrix = distance_matrix,
  xy = xy
) %>%
  rf_spatial() %>%
  # rf_tuning() %>%
  rf_evaluate() %>%
  rf_repeat()

m <- rf(
  data = plant_richness_df,
  dependent.variable.name = dependent.variable.name,
  predictor.variable.names = predictor.variable.names,
  distance.matrix = distance_matrix,
  xy = xy,
  n.cores = 1
) %>%
  rf_spatial(n.cores = 1) %>%
  # rf_tuning() %>%
  rf_evaluate(n.cores = 1) %>%
  rf_repeat(n.cores = 1)


#testing cluster
##############################
my.cluster <- parallel::makeCluster(
  4,
  type = "PSOCK"
)

doParallel::registerDoParallel(cl = my.cluster)

interactions <- rf_interactions(
  data = plant_richness_df,
  dependent.variable.name = dependent.variable.name,
  predictor.variable.names = predictor.variable.names,
  xy = xy,
  importance.threshold = 0.50, #uses 50% best predictors
  cor.threshold = 0.60,
  repetitions = 100,
  verbose = TRUE,
  cluster = my.cluster
)

parallel::stopCluster(cl = my.cluster)

#testing n.cores
##############################



#NEW SIMPLIFIED CLUSTER DEFINITION
#this works
rf.repeat <- rf_repeat(
  data = plant_richness_df,
  dependent.variable.name = "richness_species_vascular",
  predictor.variable.names = colnames(plant_richness_df)[5:21],
  distance.matrix = distance_matrix
)



#this works
rf.repeat <- rf_repeat(
  data = plant_richness_df,
  dependent.variable.name = "richness_species_vascular",
  predictor.variable.names = colnames(plant_richness_df)[5:21],
  distance.matrix = distance_matrix,
  n.cores = 4
)

#this works
rf.repeat <- rf_repeat(
  data = plant_richness_df,
  dependent.variable.name = "richness_species_vascular",
  predictor.variable.names = colnames(plant_richness_df)[5:21],
  distance.matrix = distance_matrix,
  n.cores = 1
)

#this works
my.cluster <- parallel::makeCluster(
  4,
  type = "PSOCK"
)

doParallel::registerDoParallel(cl = my.cluster)

rf.repeat <- rf_repeat(
  data = plant_richness_df,
  dependent.variable.name = "richness_species_vascular",
  predictor.variable.names = colnames(plant_richness_df)[5:21],
  distance.matrix = distance_matrix,
  cluster = my.cluster
)

parallel::stopCluster(cl = my.cluster)


##rf_spatial
#this works
rf.spatial <- rf_spatial(
  data = plant_richness_df,
  dependent.variable.name = "richness_species_vascular",
  predictor.variable.names = colnames(plant_richness_df)[5:21],
  distance.matrix = distance_matrix,
  method = "mem.moran.sequential",
  n.cores = 1
)



#this works
rf.spatial <- rf_spatial(
  data = plant_richness_df,
  dependent.variable.name = "richness_species_vascular",
  predictor.variable.names = colnames(plant_richness_df)[5:21],
  distance.matrix = distance_matrix,
  method = "mem.moran.sequential",
  n.cores = 7
)

rf.spatial <- rf_evaluate(
  model = rf.spatial,
  xy = xy,
  n.cores = 7
)

#this works
my.cluster <- parallel::makeCluster(
  4,
  type = "PSOCK"
)

doParallel::registerDoParallel(cl = my.cluster)

#this works
rf.spatial <- rf_spatial(
  data = plant_richness_df,
  dependent.variable.name = "richness_species_vascular",
  predictor.variable.names = colnames(plant_richness_df)[5:21],
  distance.matrix = distance_matrix,
  method = "mem.moran.sequential",
  cluster = my.cluster
)

rf.spatial <- rf_evaluate(
  model = rf.spatial,
  xy = xy,
  cluster = my.cluster
)

parallel::stopCluster(cl = my.cluster)


#this works
rf.spatial <- rf_spatial(
  data = plant_richness_df,
  dependent.variable.name = "richness_species_vascular",
  predictor.variable.names = colnames(plant_richness_df)[5:21],
  distance.matrix = distance_matrix,
  method = "mem.effect.recursive",
  n.cores = 1
)

#this works
rf.spatial <- rf_spatial(
  data = plant_richness_df,
  dependent.variable.name = "richness_species_vascular",
  predictor.variable.names = colnames(plant_richness_df)[5:21],
  distance.matrix = distance_matrix,
  method = "mem.effect.recursive",
  n.cores = 7
)

#this works
my.cluster <- parallel::makeCluster(
  7,
  type = "PSOCK"
)

rf.spatial <- rf_spatial(
  data = plant_richness_df,
  dependent.variable.name = "richness_species_vascular",
  predictor.variable.names = colnames(plant_richness_df)[5:21],
  distance.matrix = distance_matrix,
  method = "mem.effect.recursive",
  cluster = my.cluster
)

parallel::stopCluster(cl = my.cluster)



#BASIC MODELS TO TEST OTHER THINGIES
#############################################
data(plant_richness_df)


#loading the example data
data(plant_richness_df)

#data required to execute the function
xy <- plant_richness_df[, c("x", "y")]
dependent.variable.name <- "richness_species_vascular"
predictor.variable.names <- colnames(plant_richness_df)[5:21]

#finding useful variable combinations
combinations <- rf_interactions(
  data = plant_richness_df,
  dependent.variable.name = dependent.variable.name,
  predictor.variable.names = predictor.variable.names,
  xy = xy,                     #case coordinates for spatial cross-validation
  importance.threshold = 0.50, #selects predictors with importance above quantile 0.5
  cor.threshold = 0.60,        #Pearson correlation threshold to remove redundant combinations
  repetitions = 100,           #number of independent spatial folds to perform spatial cross-validation
  training.fraction = 0.75,    #fraction of records to train and evaluate models via spatial cross-validation
  seed = 1,                    #for reproducibility, results might change with different random seeds
  verbose = TRUE
)

#data frame with all the screened variable combinations
combinations$screening

#data frame with the selected variable combinations
combinations$selected

#plotting the whole thing
patchwork::wrap_plots(combinations$plot)

#fitting a model with all the variable combinations
m <- rf(
  data = combinations$data,
  dependent.variable.name = combinations$dependent.variable.name,
  predictor.variable.names = combinations$predictor.variable.names
)








model <- rf(
  data = data,
  dependent.variable.name = dependent.variable.name,
  predictor.variable.names = predictor.variable.names,
  distance.matrix = distance.matrix,
  verbose = FALSE
)

model <- rf_evaluate(
  model = model,
  xy = xy,
  n.cores = 1
)



















rf.repeat <- rf_repeat(
  data = plant_richness_df,
  dependent.variable.name = "richness_species_vascular",
  predictor.variable.names = colnames(plant_richness_df)[5:21],
  distance.matrix = distance_matrix
)

rf.model <- rf(
  data = plant_richness_df,
  dependent.variable.name = "richness_species_vascular",
  predictor.variable.names = colnames(plant_richness_df)[5:21],
  distance.matrix = distance_matrix
)

rf.model.repeat <- rf_repeat(
  rf.model
)

rf.spatial <- rf_spatial(
  data = plant_richness_df,
  dependent.variable.name = "richness_species_vascular",
  predictor.variable.names = colnames(plant_richness_df)[5:21],
  distance.matrix = distance_matrix
)

rf.spatial.repeat <- rf_repeat(
  rf.spatial
)







#see if rf_repeat respects evaluation and tuning slots
x <- rf_spatial(
  data = plant_richness_df,
  dependent.variable.name = dependent.variable.name,
  predictor.variable.names = predictor.variable.names,
  distance.matrix = distance_matrix,
  xy = xy,
  verbose = FALSE
) %>%
  rf_tuning(
    verbose = FALSE
  ) %>%
  rf_evaluate(
    verbose = FALSE
  ) %>%
  rf_repeat(
    repetitions = 30
  )


#rf_interactions()
#############################################
interactions <- rf_interactions(
  data = plant_richness_df,
  dependent.variable.name = "richness_species_vascular",
  predictor.variable.names = colnames(plant_richness_df)[5:21],
  cor.threshold = 0.75,
  seed = 100
)


#rf()
#############################################
rf.model <- rf(
  data = plant_richness_df,
  dependent.variable.name = "richness_species_vascular",
  predictor.variable.names = colnames(plant_richness_df)[5:21],
  distance.matrix = distance_matrix,
  scaled.importance = TRUE,
  verbose = FALSE
)

#local importance experiment
##################################################
ranger.arguments <- list(
  local.importance = TRUE
)

x.local <- rf(
  data = plant_richness_df,
  dependent.variable.name = "richness_species_vascular",
  predictor.variable.names = colnames(plant_richness_df)[5:21],
  distance.matrix = distance_matrix,
  ranger.arguments = ranger.arguments,
  scaled.importance = FALSE,
  verbose = FALSE
)

randomForestExplainer::measure_importance(x.local)
randomForestExplainer::min_depth_distribution(x.local) %>%
  randomForestExplainer::plot_min_depth_distribution()

local.importance <- cbind(
  xy,
  x.local$importance$local
  ) %>%
  tidyr::pivot_longer(
    cols = colnames(plant_richness_df)[5:21],
    names_to = "variable",
    values_to = "importance"
  ) %>%
  dplyr::mutate(
    importance = importance + abs(min(importance))
  )

world <- rnaturalearth::ne_countries(
  scale = "medium",
  returnclass = "sf"
)

ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = world,
    fill = "white"
  ) +
  ggplot2::facet_wrap("variable", ncol = 6) +
  ggplot2::geom_point(
    data = local.importance,
    ggplot2::aes(
      x = x,
      y = y,
      color = importance
    )
  ) +
  ggplot2::scale_x_continuous(limits = c(-170, -30)) +
  ggplot2::scale_y_continuous(limits = c(-58, 80)) +
  viridis::scale_color_viridis(direction = -1) +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "bottom")



plot_residuals_diagnostics(x)

x <- rf_repeat(
  model = x
)

#get plot and print functions
get_importance(x)
get_moran(x)
get_performance(x)
get_predictions(x)
get_residuals(x)
get_response_curves(x)
plot_importance(x)
plot_response_curves(x)
print_importance(x)
print_moran(x)
print_performance(x)
print(x)

#evaluate
x <- rf_evaluate(
  model = x,
  xy = xy
)

get_evaluation(x)
print_evaluation(x)
plot_evaluation(x)

#tunning
x <- rf_tuning(
  model = x,
  xy = xy
)

plot_tuning(x)

#spatial
x <- rf_spatial(
  model = x
)

rf_spatial <- rf_spatial(
  data = plant_richness_df,
  dependent.variable.name = "richness_species_vascular",
  predictor.variable.names = colnames(plant_richness_df)[5:21],
  distance.matrix = distance_matrix,
  verbose = FALSE
)


#with repetitions
x <- rf_repeat(
  model = x,
  verbose = FALSE
  )

#get plot and print functions
get_importance(x)
get_moran(x)
get_performance(x)
get_predictions(x)
get_residuals(x)
get_response_curves(x)
plot_importance(x)
plot_response_curves(x)
print_importance(x)
print_moran(x)
print_performance(x)
print(x)

#spatial model
rf.spatial <- rf_spatial(
  model = x,
  verbose = TRUE
  )

get_spatial_predictors(rf.spatial)
plot_optimization(rf.spatial)

#from repeat
rf.spatial.repeat <- rf_spatial(model = rf.repeat, verbose = FALSE)

#rf_spatial from data
#basic model
x <- rf_spatial(
  data = plant_richness_df,
  dependent.variable.name = "richness_species_vascular",
  predictor.variable.names = colnames(plant_richness_df)[5:21],
  distance.matrix = distance_matrix,
  distance.thresholds = c(0, 1000, 2000),
  seed = 50,
  verbose = FALSE
)

#trying rf_evaluate
x <- rf_evaluate(
  model = x,
  xy = plant_richness_df[, c("x", "y")],
  verbose = FALSE,
  metrics = "r.squared"
)
plot_evaluation(x)
get_evaluation(x)

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
rf.interaction <- rf_interactions(
  data = plant_richness_df,
  dependent.variable.name = "richness_species_vascular",
  predictor.variable.names = colnames(plant_richness_df)[5:21],
  verbose = TRUE
)

rf.interaction$selected


#RESPONSE SURFACES
p <- plot_response_surfaces(
  model = x
  )

p <- plot_response_curves(
  model = x
)

p <- plot_response_surfaces(
  model = rf.repeat
)

p <- plot_response_curves(
  model = rf.repeat
)

p <- plot_response_surfaces(
  model = rf.spatial
)

p <- plot_response_curves(
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
  distance.matrix = distance_matrix,
  distance.thresholds = c(0, 100, 1000, 10000),
  max.spatial.predictors = NULL
)
rank <- rank_spatial_predictors(
  distance.matrix = distance_matrix,
  distance.thresholds = c(0, 100, 1000, 10000),
  spatial.predictors.df = spatial.predictors.df,
  ranking.method = "effect"
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


#rf spatial methods
tuning <- rf_spatial(
  data = plant_richness_df,
  dependent.variable.name = "richness_species_vascular",
  predictor.variable.names = colnames(plant_richness_df)[5:21],
  method = "mem.moran.sequential",
  distance.matrix = distance_matrix,
  distance.thresholds = c(0, 1000, 10000),
  cluster.ips = c(
    "10.42.0.1",
    "10.42.0.34",
    "10.42.0.104"
  ),
  cluster.cores = c(7, 4, 4)
)


#rf spatial methods
tuning <- rf_spatial(
  data = plant_richness_df,
  dependent.variable.name = "richness_species_vascular",
  predictor.variable.names = colnames(plant_richness_df)[5:21],
  method = "mem.effect.sequential",
  distance.matrix = distance_matrix,
  distance.thresholds = c(0, 1000, 10000),
  cluster.ips = c(
    "10.42.0.1",
    "10.42.0.34",
    "10.42.0.104"
  ),
  cluster.cores = c(7, 4, 4)
)

#rf spatial methods
tuning <- rf_spatial(
  data = plant_richness_df,
  dependent.variable.name = "richness_species_vascular",
  predictor.variable.names = colnames(plant_richness_df)[5:21],
  method = "mem.effect.recursive",
  distance.matrix = distance_matrix,
  distance.thresholds = c(0, 1000, 10000),
  cluster.ips = c(
    "10.42.0.1",
    "10.42.0.34",
    "10.42.0.104"
  ),
  cluster.cores = c(7, 4, 4)
)


#rf spatial methods
tuning <- rf_spatial(
  data = plant_richness_df,
  dependent.variable.name = "richness_species_vascular",
  predictor.variable.names = colnames(plant_richness_df)[5:21],
  method = "hengl",
  distance.matrix = distance_matrix,
  distance.thresholds = c(0, 1000, 10000),
  cluster.ips = c(
    "10.42.0.1",
    "10.42.0.34",
    "10.42.0.104"
  ),
  cluster.cores = c(7, 4, 4)
)


#rf spatial methods
tuning <- rf_spatial(
  data = plant_richness_df,
  dependent.variable.name = "richness_species_vascular",
  predictor.variable.names = colnames(plant_richness_df)[5:21],
  method = "hengl.moran.sequential",
  distance.matrix = distance_matrix,
  distance.thresholds = c(0, 1000, 10000),
  cluster.ips = c(
    "10.42.0.1",
    "10.42.0.34",
    "10.42.0.104"
  ),
  cluster.cores = c(7, 4, 4)
)

#rf spatial methods
tuning <- rf_spatial(
  data = plant_richness_df,
  dependent.variable.name = "richness_species_vascular",
  predictor.variable.names = colnames(plant_richness_df)[5:21],
  method = "hengl.effect.sequential",
  distance.matrix = distance_matrix,
  distance.thresholds = c(0, 1000, 10000),
  cluster.ips = c(
    "10.42.0.1",
    "10.42.0.34",
    "10.42.0.104"
  ),
  cluster.cores = c(7, 4, 4)
)

#rf spatial methods
tuning <- rf_spatial(
  data = plant_richness_df,
  dependent.variable.name = "richness_species_vascular",
  predictor.variable.names = colnames(plant_richness_df)[5:21],
  method = "hengl.effect.recursive",
  distance.matrix = distance_matrix,
  distance.thresholds = c(0, 1000, 10000),
  cluster.ips = c(
    "10.42.0.1",
    "10.42.0.34",
    "10.42.0.104"
  ),
  cluster.cores = c(7, 4, 4)
)

#rf spatial methods
tuning <- rf_spatial(
  data = plant_richness_df,
  dependent.variable.name = "richness_species_vascular",
  predictor.variable.names = colnames(plant_richness_df)[5:21],
  method = "pca.moran.sequential",
  distance.matrix = distance_matrix,
  distance.thresholds = c(0, 1000, 10000),
  cluster.ips = c(
    "10.42.0.1",
    "10.42.0.34",
    "10.42.0.104"
  ),
  cluster.cores = c(7, 4, 4)
)


#rf spatial methods
tuning <- rf_spatial(
  data = plant_richness_df,
  dependent.variable.name = "richness_species_vascular",
  predictor.variable.names = colnames(plant_richness_df)[5:21],
  method = "pca.effect.sequential",
  distance.matrix = distance_matrix,
  distance.thresholds = c(0, 1000, 10000),
  cluster.ips = c(
    "10.42.0.1",
    "10.42.0.34",
    "10.42.0.104"
  ),
  cluster.cores = c(7, 4, 4)
)

#rf spatial methods
tuning <- rf_spatial(
  data = plant_richness_df,
  dependent.variable.name = "richness_species_vascular",
  predictor.variable.names = colnames(plant_richness_df)[5:21],
  method = "mem.effect.recursive",
  distance.matrix = distance_matrix,
  distance.thresholds = c(0, 1000, 10000),
  cluster.ips = c(
    "10.42.0.1",
    "10.42.0.34",
    "10.42.0.104"
  ),
  cluster.cores = c(7, 4, 4),
  verbose = FALSE
)

tuning <- rf_spatial(
  data = plant_richness_df,
  dependent.variable.name = "richness_species_vascular",
  predictor.variable.names = colnames(plant_richness_df)[5:21],
  method = "mem.effect.recursive",
  distance.matrix = distance_matrix,
  distance.thresholds = c(0, 1000, 10000),
  verbose = FALSE
)

spatial.predictors.df <- mem_multithreshold(
  distance.matrix = distance_matrix,
  distance.thresholds = c(0, 100, 1000, 10000),
  max.spatial.predictors = NULL
)
rank <- rank_spatial_predictors(
  data = plant_richness_df,
  dependent.variable.name = "richness_species_vascular",
  predictor.variable.names = colnames(plant_richness_df)[5:21],
  distance.matrix = distance_matrix,
  distance.thresholds = c(0, 100, 1000, 10000),
  spatial.predictors.df = spatial.predictors.df,
  ranking.method = "effect"
)
