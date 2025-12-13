# Extract and run all examples from the spatialRF package documentation
# Generated automatically from .Rd files in man/

library(spatialRF)

# auc ----
out <- auc(
  o = c(0, 0, 1, 1),
  p = c(0.1, 0.6, 0.4, 0.8)
)

# auto_cor ----
data(
  plants_df,
  plants_predictors
)

y <- auto_cor(x = plants_df[, plants_predictors])

#getting the correlation matrix
y$cor

#getting the names of the selected variables
y$selected.variables

#getting the data frame of selected variables
y$selected.variables.df

#with pipes
y <- plants_df %>%
  dplyr::select(
    dplyr::all_of(plants_predictors)
  ) %>%
  auto_vif() %>%
  auto_cor()

# auto_vif ----
#loading data
data(plants_df)

#on a data frame
y <- auto_vif(x = plants_df[, plants_predictors])

#getting the vif data frame
y$vif

#getting the names of the selected variables
y$selected.variables

#getting the data frame of selected variables
y$selected.variables.df

#with pipes
y <- plants_df %>%
  dplyr::select(
    dplyr::all_of(plants_predictors)
  ) %>%
  auto_cor() %>%
  auto_vif()

# beowulf_cluster ----
# beowulf.cluster <- beowulf_cluster(
#  cluster.ips = c(
#    "x.x.x.x",
#    "x.x.x.x",
#    "x.x.x.x"
#    ),
# cluster.cores = c(7, 4, 4),
# cluster.user = "blas",
# cluster.port = "11000"
#)

# doParallel::registerDoParallel(cl = beowulf.cluster)

#PARALLELIZED foreach LOOP HERE

# parallel::stopCluster(cl = beowulf.cluster)

# case_weights ----
data <- data.frame(
  response = c(0, 0, 0, 1, 1)
)

case_weights(
  data = data,
  dependent.variable.name = "response"
)

# default_distance_thresholds ----
data(plants_distance)

default_distance_thresholds(
  distance.matrix = plants_distance
)

# double_center_plants_distance ----
#loading the distance matrix
data(plants_distance)

double_center_plants_distance(
  distance.matrix = plants_distance
) %>%
  head()

# filter_spatial_predictors ----
#loading data
data(plants_df, plants_distance)

#computing Moran's Eigenvector Maps
spatial.predictors.df <- mem_multithreshold(
  distance.matrix = plants_distance,
  distance.thresholds = c(0, 1000)
)

#filtering spatial predictors
spatial.predictors.df <- filter_spatial_predictors(
  data = plants_df,
  predictor.variable.names = plants_predictors,
  spatial.predictors.df = spatial.predictors.df,
  cor.threshold = 0.50
)

# get_evaluation ----
#loading data
data(
  plants_df,
  plants_distance,
  plants_response,
  plants_predictors
)

#fitting a random forest model
m <- rf(
  data = plants_df,
  dependent.variable.name = plants_response,
  predictor.variable.names = plants_predictors,
  distance.matrix = plants_distance,
  distance.thresholds = 0,
  n.cores = 1,
  verbose = FALSE
)

#evaluating the model with spatial cross-validation
m <- rf_evaluate(
  model = m,
  xy = plants_xy,
  n.cores = 1,
  verbose = FALSE
)

#getting evaluation results from the model
x <- get_evaluation(m)
x

# get_importance_local ----
#loading example data
data(
  plants_df,
  plants_response,
  plants_predictors,
  plants_distance
)

#fittinga random forest model
m <- rf(
  data = plants_df,
  dependent.variable.name = plants_response,
  predictor.variable.names = plants_predictors,
  distance.matrix = plants_distance,
  distance.thresholds = 0,
  n.cores = 1,
  verbose = FALSE
)

#getting importance scores
x <- get_importance_local(m)
x

# get_importance ----
data(
  plants_df,
  plants_response,
  plants_predictors,
  plants_distance
)

m <- rf(
  data = plants_df,
  dependent.variable.name = plants_response,
  predictor.variable.names = plants_predictors,
  distance.matrix = plants_distance,
  distance.thresholds = 0,
  n.cores = 1,
  verbose = FALSE
)

x <- get_importance(m)
x

# get_moran ----
#loading example data
data(
  plants_df,
  plants_response,
  plants_predictors,
  plants_distance
)

#fitting a random forest model
m <- rf(
  data = plants_df,
  dependent.variable.name = plants_response,
  predictor.variable.names = plants_predictors,
  distance.matrix = plants_distance,
  distance.thresholds = c(0, 1000, 2000),
  n.cores = 1,
  verbose = FALSE
)

#getting Moran's I of the residuals
x <- get_moran(m)
x

# get_performance ----
#loading example data
data(
  plants_df,
  plants_response,
  plants_predictors,
  plants_distance
)

#fitting random forest model
m <- rf(
  data = plants_df,
  dependent.variable.name = plants_response,
  predictor.variable.names = plants_predictors,
  distance.matrix = plants_distance,
  distance.thresholds = 0,
  n.cores = 1,
  verbose = FALSE
)

#getting model performance
x <- get_performance(m)
x

# get_predictions ----
#loading example data
data(
  plants_df,
  plants_response,
  plants_predictors
)

#fitting a random forest model
m <- rf(
  data = plants_df,
  dependent.variable.name = plants_response,
  predictor.variable.names = plants_predictors,
  n.cores = 1,
  verbose = FALSE
)

#get vector of predictions
x <- get_predictions(m)
x

# get_residuals ----
#load example data
data(
  plants_df,
  plants_response,
  plants_predictors
)

#fit random forest model
m <- rf(
  data = plants_df,
  dependent.variable.name = plants_response,
  predictor.variable.names = plants_predictors,
  n.cores = 1,
  verbose = FALSE
)

#getting vector with residuals
x <- get_residuals(m)
x

# get_response_curves ----
#loading example data
data(
  plants_df,
  plants_response,
  plants_predictors
)

#fitting random forest model
m <- rf(
  data = plants_df,
  dependent.variable.name = plants_response,
  predictor.variable.names = plants_predictors,
  n.cores = 1,
  verbose = FALSE
)

#getting data frame with response curves
get_response_curves(m) %>%
  head()


# get_spatial_predictors ----
#loading example data
data(
  plants_df,
  plants_distance,
  plants_response,
  plants_predictors
)

#subset to speed-up example
idx <- 50:100
plants_df <- plants_df[idx, ]
plants_distance <- plants_distance[idx, idx]

#fittind spatial model
m_spatial <- rf_spatial(
  data = plants_df,
  dependent.variable.name = plants_response,
  predictor.variable.names = plants_predictors,
  distance.matrix = plants_distance,
  distance.thresholds = c(0, 1000),
  n.cores = 1,
  method = "mem.moran.sequential"
)

#getting data frame with the selected spatial predictors
get_spatial_predictors(m_spatial) %>%
  head()

# is_binary ----
#dummy data frame
df <- data.frame(
  response = c(0, 0, 0, 1, 1)
)

#checking if response is binary
is_binary(
  data = df,
  dependent.variable.name = "response"
)

# make_spatial_fold ----
#loading example data
data(plants_df, plants_xy)

#building a spatial fold centered in the first pair of coordinates
y <- make_spatial_fold(
  xy.i = plants_xy[1, ],
  xy = plants_xy,
  training.fraction = 0.6
)

#indices of the training and testing folds
y$training
y$testing

if (interactive()) {
  #plotting the data
  plot(plants_xy[c("x", "y")], type = "n", xlab = "", ylab = "")
  #plots training points
  points(plants_xy[y$training, c("x", "y")], col = "red4", pch = 15)
  #plots testing points
  points(plants_xy[y$testing, c("x", "y")], col = "blue4", pch = 15)
  #plots xy.i
  points(plants_xy[1, c("x", "y")], col = "black", pch = 15, cex = 2)
}

# make_spatial_folds ----
#loading example data
data(plants_df, plants_xy)

#thining til 20 cases
xy.selected <- thinning_til_n(
  xy = plants_xy,
  n = 20
)

#making spatial folds centered on these 20 cases
y <- make_spatial_folds(
  xy.selected = xy.selected,
  xy = plants_xy,
  distance.step.x = 0.05, #degrees
  training.fraction = 0.6,
  n.cores = 1
)

if (interactive()) {
  #plotting training and testing folds
  plot(plants_xy[c("x", "y")], type = "n", xlab = "", ylab = "")
  #plots training points
  points(plants_xy[y[[10]]$training, c("x", "y")], col = "red4", pch = 15)
  #plots testing points
  points(plants_xy[y[[10]]$testing, c("x", "y")], col = "blue4", pch = 15)
  #plots xy.i
  points(plants_xy[10, c("x", "y")], col = "black", pch = 15, cex = 2)
}

# mem_multithreshold ----
#loading example data
data(plants_distance)

#computing Moran's eigenvector maps for 0, 1000, and 2000 km
mem.df <- mem_multithreshold(
  distance.matrix = plants_distance,
  distance.thresholds = c(0, 1000, 2000)
)
head(mem.df)

# mem ----
#loading example distance matrix
data(plants_distance)

#Moran's Eigenvector Maps of the distance matrix
mem <- mem(distance.matrix = plants_distance)

# moran_multithreshold ----
#loading example data
data(plants_df, plants_distance, plants_response)

#computing Moran's I for the response variable at several reference distances
y <- moran_multithreshold(
  x = plants_df[[plants_response]],
  distance.matrix = plants_distance,
  distance.thresholds = c(0, 100, 1000, 10000)
)
y

# moran ----
#loading example data
data(plants_df, plants_distance)

#Moran's I of the response variable
y <- moran(
  x = plants_df[[plants_response]],
  distance.matrix = plants_distance
)
y

# normality ----
residuals_test(residuals = runif(100))

# objects_size ----
#creating dummy objects
x <- matrix(runif(100), 10, 10)
y <- matrix(runif(10000), 100, 100)

#reading their in-memory size
objects_size()

# pca_multithreshold ----
#loading example distance matrix
data(plants_distance)

#PCA factors of the distance matrix for two reference distances
x <- pca_multithreshold(
  distance.matrix = plants_distance,
  distance.thresholds = c(0, 1000)
)
head(x)

# pca ----
#load example distance matrix
data(plants_distance)

#PCA of the distance matrix
y <- pca(x = plants_distance)
y

# plot_evaluation ----
#loading example data
data(
  plants_df,
  plants_distance,
  plants_xy,
  plants_response,
  plants_predictors
)

#fitting a random forest model
m <- rf(
  data = plants_df,
  dependent.variable.name = plants_response,
  predictor.variable.names = plants_predictors,
  distance.matrix = plants_distance,
  distance.thresholds = 0,
  n.cores = 1,
  verbose = FALSE
)

#evaluating the model with spatial cross-validation
m <- rf_evaluate(
  model = m,
  xy = plants_xy,
  repetitions = 10,
  n.cores = 1
)

plot_evaluation(m)

# plot_importance ----
#loading example data
data(
  plants_df,
  plants_distance,
  plants_response,
  plants_predictors
)

#fitting a random forest model
m <- rf(
  data = plants_df,
  dependent.variable.name = plants_response,
  predictor.variable.names = plants_predictors,
  distance.matrix = plants_distance,
  distance.thresholds = 0,
  n.cores = 1,
  verbose = FALSE
)

#plotting variable importance scores
plot_importance(model = m)

# plot_moran ----
#loading example data
data(
  plants_df,
  plants_distance,
  plants_response,
  plants_predictors
)

#fitting a random forest model
m <- rf(
  data = plants_df,
  dependent.variable.name = plants_response,
  predictor.variable.names = plants_predictors,
  distance.matrix = plants_distance,
  distance.thresholds = c(0, 1000, 2000),
  n.cores = 1,
  verbose = FALSE
)

#Incremental/multiscale Moran's I
plot_moran(m)

#Moran's scatterplot
plot_moran(m, option = 2)

# plot_optimization ----
#loading example data
data(
  plants_df,
  plants_distance
)

idx <- 50:100
plants_df <- plants_df[idx, ]
plants_distance <- plants_distance[idx, idx]

#spatial model
m <- rf_spatial(
  data = plants_df,
  dependent.variable.name = plants_response,
  predictor.variable.names = plants_predictors,
  distance.matrix = plants_distance,
  distance.thresholds = 0,
  method = "mem.moran.sequential",
  n.cores = 1
)

#plotting selection of spatial predictors
plot_optimization(model = m)

# plot_residuals_diagnostics ----
#load example data
data(
  plants_df,
  plants_response,
  plants_predictors
)


#fit a random forest model
m <- rf(
  data = plants_df,
  dependent.variable.name = plants_response,
  predictor.variable.names = plants_predictors,
  n.cores = 1,
  verbose = FALSE
)

#residuals diagnostics
plot_residuals_diagnostics(m)

# plot_response_curves ----
#loading example data
data(
  plants_df,
  plants_response,
  plants_predictors
)

#fitting a random forest model
m <- rf(
  data = plants_df,
  dependent.variable.name = plants_response,
  predictor.variable.names = plants_predictors,
  n.cores = 1,
  verbose = FALSE
)

#response curves of most important predictors
plot_response_curves(model = m)

# plot_response_surface ----
#load example data
data(
  plants_df,
  plants_response,
  plants_predictors
)


#fit random forest model
m <- rf(
  data = plants_df,
  dependent.variable.name = plants_response,
  predictor.variable.names = plants_predictors,
  n.cores = 1,
  verbose = FALSE
)

#plot interactions between most important predictors
plot_response_surface(m)

# plot_training_df_moran ----
#load example data
data(
  plants_df,
  plants_response,
  plants_predictors,
  plants_distance
)

#plot Moran's I of training data
plot_training_df_moran(
  data = plants_df,
  dependent.variable.name = plants_response,
  predictor.variable.names = plants_predictors,
  distance.matrix = plants_distance,
  distance.thresholds = c(
    0,
    2000,
    4000,
    6000,
    8000
  )
)

# plot_training_df ----
#load example data
data(
  plants_df,
  plants_response,
  plants_predictors
)

#scatterplot of the training data
plot_training_df(
  data = plants_df,
  dependent.variable.name = plants_response,
  predictor.variable.names = plants_predictors
)

# plot_tuning ----
#load example data
data(
  plants_df,
  plants_response,
  plants_predictors,
  plants_distance,
  plants_xy
)

#fit random forest model
m <- rf(
  data = plants_df,
  dependent.variable.name = plants_response,
  predictor.variable.names = plants_predictors,
  distance.matrix = plants_distance,
  distance.thresholds = 0,
  n.cores = 1,
  verbose = FALSE
)

#tune random forest model
m_tuned <- rf_tuning(
  model = m,
  xy = plants_xy,
  n.cores = 1,
  verbose = FALSE
)

#generate tuning plot
plot_tuning(model = m_tuned)

# prepare_importance_spatial ----
#loading example data
data(
  plants_df,
  plants_response,
  plants_predictors,
  plants_distance
)

#fittind spatial model
m_spatial <- rf_spatial(
  data = plants_df,
  dependent.variable.name = plants_response,
  predictor.variable.names = plants_predictors,
  distance.matrix = plants_distance,
  distance.thresholds = 0,
  n.cores = 1
)

#preparing the importance data frame
prepare_importance_spatial(m_spatial) %>%
  head()

# print_evaluation ----
#loading example data
data(
  plants_df,
  plants_response,
  plants_predictors,
  plants_distance,
  plants_xy
)

#fitting random forest model
m <- rf(
  data = plants_df,
  dependent.variable.name = plants_response,
  predictor.variable.names = plants_predictors,
  distance.matrix = plants_distance,
  distance.thresholds = 0,
  n.cores = 1,
  verbose = FALSE
)

#evaluation with spatial cross-validation
m_evaluated <- rf_evaluate(
  model = m,
  xy = plants_xy,
  n.cores = 1
)

#checking evaluation results
print_evaluation(m_evaluated)

# print_importance ----
#loading example data
data(
  plants_df,
  plants_response,
  plants_predictors,
  plants_distance
)

#fitting a random forest model
m <- rf(
  data = plants_df,
  dependent.variable.name = plants_response,
  predictor.variable.names = plants_predictors,
  distance.matrix = plants_distance,
  distance.thresholds = 0,
  n.cores = 1,
  verbose = FALSE
)

#printing variable importance scores
print_importance(model = m)

# print_moran ----
#loading example data
data(
  plants_df,
  plants_response,
  plants_predictors,
  plants_distance
)

#fitting random forest model
m <- rf(
  data = plants_df,
  dependent.variable.name = plants_response,
  predictor.variable.names = plants_predictors,
  distance.matrix = plants_distance,
  distance.thresholds = c(0, 1000, 2000),
  n.cores = 1,
  verbose = FALSE
)

#printing Moran's I of model's residuals
print_moran(m)

# print_performance ----
#loading example data
data(
  plants_df,
  plants_response,
  plants_predictors,
  plants_distance
)

#fitting a random forest model
m <- rf(
  data = plants_df,
  dependent.variable.name = plants_response,
  predictor.variable.names = plants_predictors,
  distance.matrix = plants_distance,
  distance.thresholds = 0,
  n.cores = 1,
  verbose = FALSE
)

#printing performance scores
print_performance(m)

# print ----
#loading example data
data(
  plants_df,
  plants_response,
  plants_predictors,
  plants_distance
)

#fitting random forest model
m <- rf(
  data = plants_df,
  dependent.variable.name = plants_response,
  predictor.variable.names = plants_predictors,
  distance.matrix = plants_distance,
  distance.thresholds = 0,
  n.cores = 1
)

#printing model summary
print(m)

# rank_spatial_predictors ----
#loading distance matrix
data(plants_distance)

idx <- 50:100
plants_distance <- plants_distance[idx, idx]

#computing Moran's Eigenvector Maps
y <- mem(
  distance.matrix = plants_distance,
  distance.threshold = 0
)

#ranking by the Moran's I of the spatial predictor
y_rank <- rank_spatial_predictors(
  distance.matrix = plants_distance,
  distance.thresholds = 0,
  spatial.predictors.df = y,
  ranking.method = "moran",
  n.cores = 1
)

#checking Moran's I of MEMs
y_rank$criteria

#checking rank of MEMs
y_rank$ranking

# rescale_vector ----
y <- rescale_vector(
  x = rnorm(100),
  new.min = 0,
  new.max = 100,
  integer = TRUE
)
y

# residuals_diagnostics ----
y <- residuals_diagnostics(
  residuals = runif(100),
  predictions = runif(100)
)
y

# rf_compare ----
#loading example data
data(
  plants_df,
  plants_response,
  plants_predictors,
  plants_distance
)

#fitting random forest model
m <- rf(
  data = plants_df,
  dependent.variable.name = plants_response,
  predictor.variable.names = plants_predictors,
  distance.matrix = plants_distance,
  distance.thresholds = 0,
  n.cores = 1
)

#fitting a spatial model with Moran's Eigenvector Maps
m_spatial <- rf_spatial(
  model = m,
  n.cores = 1
)

#comparing the spatial and non spatial models
comparison <- rf_compare(
  models = list(
    `Non spatial` = m,
    Spatial = m_spatial
  ),
  xy = plants_df[, c("x", "y")],
  metrics = c("r.squared", "rmse"),
  n.cores = 1
)

# rf_evaluate ----
#loading example data
data(
  plants_df,
  plants_response,
  plants_predictors,
  plants_distance,
  plants_xy
)

#fitting random forest model
m <- rf(
  data = plants_df,
  dependent.variable.name = plants_response,
  predictor.variable.names = plants_predictors,
  distance.matrix = plants_distance,
  distance.thresholds = 0,
  n.cores = 1,
  verbose = FALSE
)

#evaluation with spatial cross-validation
m_evaluated <- rf_evaluate(
  model = m,
  xy = plants_xy,
  n.cores = 1
)

#checking evaluation results
plot_evaluation(m_evaluated)
print_evaluation(m_evaluated)
get_evaluation(m_evaluated) %>%
  head()

# rf_importance ----
#loading example data
data(
  plants_df,
  plants_response,
  plants_predictors,
  plants_distance,
  plants_xy
)

#fitting random forest model
m <- rf(
  data = plants_df,
  dependent.variable.name = plants_response,
  predictor.variable.names = plants_predictors,
  distance.matrix = plants_distance,
  distance.thresholds = 0,
  xy = plants_xy,
  n.cores = 1,
  verbose = FALSE
)

#computing predictor contribution to model transferability
m_importance <- rf_importance(m)

# rf_repeat ----
#loading example data
data(
  plants_df,
  plants_response,
  plants_predictors,
  plants_distance
)

#fitting 5 random forest models
m_repeat <- rf_repeat(
  data = plants_df,
  dependent.variable.name = plants_response,
  predictor.variable.names = plants_predictors,
  distance.matrix = plants_distance,
  distance.thresholds = 0,
  repetitions = 5,
  n.cores = 1
)

#data frame with ordered variable importance
m_repeat$importance$per.variable

#per repetition
m_repeat$importance$per.repetition

#variable importance plot
m_repeat$importance$per.repetition.plot

#performance
m_repeat$performance


#spatial correlation of the residuals for different distance thresholds
m_repeat$spatial.correlation.residuals$per.distance

#plot of the Moran's I of the residuals for different distance thresholds
m_repeat$spatial.correlation.residuals$plot

#using a model as an input for rf_repeat()
m <- rf(
  data = plants_df,
  dependent.variable.name = plants_response,
  predictor.variable.names = plants_predictors,
  distance.matrix = plants_distance,
  distance.thresholds = 0,
  n.cores = 1
)

#repeating the model 5 times
m_repeat <- rf_repeat(
  model = rf.model,
  n.cores = 1
)

m_repeat$performance
m_repeat$importance$per.repetition.plot

# rf_spatial ----
#loading example data
data(
  plants_df,
  plants_response,
  plants_predictors,
  plants_distance
)

#hengl
m_spatial <- rf_spatial(
  data = plants_df,
  dependent.variable.name = plants_response,
  predictor.variable.names = plants_predictors,
  distance.matrix = plants_distance,
  distance.thresholds = 0,
  method = "hengl",
  n.cores = 1
)

#mem.moran.sequential
m_spatial <- rf_spatial(
  data = plants_df,
  dependent.variable.name = plants_response,
  predictor.variable.names = plants_predictors,
  distance.matrix = plants_distance,
  distance.thresholds = 0,
  method = "mem.moran.sequential",
  n.cores = 1
)

#fitting an rf_spatial model from an rf model
m_spatial <- rf(
  data = plants_df,
  dependent.variable.name = plants_response,
  predictor.variable.names = plants_predictors,
  distance.matrix = plants_distance,
  distance.thresholds = 0,
  n.cores = 1,
  verbose = FALSE
) %>%
  rf_spatial()

m_spatial$spatial.correlation.residuals$plot

# rf_tuning ----
#loading example data
data(
  plants_df,
  plants_response,
  plants_predictors,
  plants_distance,
  plants_xy
)

#fitting model to tune
m <- rf(
  data = plants_df,
  dependent.variable.name = plants_response,
  predictor.variable.names = plants_predictors,
  distance.matrix = plants_distance,
  distance.thresholds = 0,
  n.cores = 1
)

#model tuning
m_tuned <- rf_tuning(
  model = m,
  num.trees = c(100),
  mtry = c(2, 8),
  min.node.size = c(5, 10),
  xy = plants_xy,
  n.cores = 1
)

# rf ----
#loading example data
data(
  plants_df,
  plants_response,
  plants_predictors,
  plants_distance
)

#fittind random forest model
m <- rf(
  data = plants_df,
  dependent.variable.name = plants_response,
  predictor.variable.names = plants_predictors,
  distance.matrix = plants_distance,
  distance.thresholds = 0,
  n.cores = 1
)

class(m)

#data frame with ordered variable importance
m$importance$per.variable

#variable importance plot
m$importance$per.variable.plot

#performance
m$performance

#spatial correlation of the residuals
m$spatial.correlation.residuals$per.distance

#plot of the Moran's I of the residuals for different distance thresholds
m$spatial.correlation.residuals$plot

#predictions for new data as done with ranger models:
y <- stats::predict(
  object = m,
  data = plants_df,
  type = "response"
)$predictions

#alternative data input methods
###############################

#ranger.arguments can contain ranger arguments and any other rf argument
args <- list(
  data = plants_df,
  dependent.variable.name = plants_response,
  predictor.variable.names = plants_predictors,
  distance.matrix = plants_distance,
  distance.thresholds = c(0, 1000)
)

#fitting model with these ranger arguments
m <- rf(
  ranger.arguments = args,
  n.cores = 1
)

# root_mean_squared_error ----
root_mean_squared_error(
  o = runif(10),
  p = runif(10)
)

# select_spatial_predictors_recursive ----
#loading example data
data(
  plants_df,
  plants_response,
  plants_predictors,
  plants_distance
)

#non-spatial model
m <- rf(
  data = plants_df,
  dependent.variable.name = plants_response,
  predictor.variable.names = plants_predictors,
  distance.matrix = plants_distance,
  distance.thresholds = 0,
  n.cores = 1
)

#preparing spatial predictors
spatial.predictors <- mem_multithreshold(
  distance.matrix = plants_distance,
  distance.thresholds = 0
)

#ranking spatial predictors
spatial.predictors.ranking <- rank_spatial_predictors(
  data = plants_df,
  dependent.variable.name = plants_response,
  predictor.variable.names = plants_predictors,
  spatial.predictors.df = spatial.predictors,
  ranking.method = "moran",
  reference.moran.i = m$spatial.correlation.residuals$max.moran,
  distance.matrix = plants_distance,
  distance.thresholds = 0,
  n.cores = 1
)

#selecting the best subset of predictors
selection <- select_spatial_predictors_recursive(
  data = plants_df,
  dependent.variable.name = plants_response,
  predictor.variable.names = plants_predictors,
  distance.matrix = plants_distance,
  distance.thresholds = 0,
  spatial.predictors.df = spatial.predictors,
  spatial.predictors.ranking = spatial.predictors.ranking,
  n.cores = 1
)

selection$optimization
selection$best.spatial.predictors
plot_optimization(selection$optimization)

# select_spatial_predictors_sequential ----
#loading example data
data(
  plants_df,
  plants_response,
  plants_predictors,
  plants_distance
)

#non-spatial model
m <- rf(
  data = plants_df,
  dependent.variable.name = plants_response,
  predictor.variable.names = plants_predictors,
  distance.matrix = plants_distance,
  distance.thresholds = 0,
  n.cores = 1
)

#preparing spatial predictors
spatial.predictors <- mem_multithreshold(
  distance.matrix = distance.matrix,
  distance.thresholds = 0
)

#ranking spatial predictors by their Moran's I (faster option)
spatial.predictors.ranking <- rank_spatial_predictors(
  ranking.method = "moran",
  spatial.predictors.df = spatial.predictors,
  reference.moran.i = model$spatial.correlation.residuals$max.moran,
  distance.matrix = distance.matrix,
  distance.thresholds = 0,
  n.cores = 1
)

#selecting the best subset of predictors
selection <- select_spatial_predictors_sequential(
  data = plants_df,
  dependent.variable.name = dependent.variable.name,
  predictor.variable.names = predictor.variable.names,
  distance.matrix = plants_distance,
  distance.thresholds = 0,
  spatial.predictors.df = spatial.predictors,
  spatial.predictors.ranking = spatial.predictors.ranking,
  n.cores = 1
)

selection$optimization
selection$best.spatial.predictors
plot_optimization(selection$optimization)

# standard_error ----
standard_error(runif(10))

# statistical_mode ----
statistical_mode(c(10, 9, 10, 8))

# the_feature_engineer ----
#load example data
data(
  plants_df,
  plants_response,
  plants_predictors,
  plants_distance
)

y <- the_feature_engineer(
  data = plants_df,
  dependent.variable.name = plants_response,
  predictor.variable.names = plants_predictors,
  n.cores = 1,
  verbose = TRUE
)

y$screening
y$selected
y$columns

# thinning_til_n ----
#loading example data
data(plants_df)

#thinning to ~20 records
y <- thinning_til_n(
  x = plants_df,
  n = 20
)

head(y)

# thinning ----
#load example data
data(plants_df)

#thinning to points separated by 5 degrees
y <- thinning(
  x = plants_df,
  minimum.distance = 5 #points separated by at least 5 degrees
)

head(y)

# vif ----
data(plants_df, plants_predictors)

vif(plants_df[, plants_predictors])

# weights_from_plants_distance ----
#loading example distance matrix
data(plants_distance)

#computing matrix of weights
distance.matrix.weights <- weights_from_plants_distance(
  distance.matrix = plants_distance
)

distance.matrix.weights
