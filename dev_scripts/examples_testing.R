library(spatialRF)
library(tictoc)

# auc ----
#DONE

out <- auc(
  o = c(0, 0, 1, 1),
  p = c(0.1, 0.6, 0.4, 0.8)
)


# auto_cor ----
#DONE

data(
  plants_df,
  plants_predictors
)

y <- auto_cor(
  x = plants_df[, plants_predictors]
)

y$selected.variables
y$cor
head(y$selected.variables.df)


# auto_vif ----
#DONE

#' data(
#'   plants_df,
#'   plants_predictors
#' )
#'
#' y <- auto_vif(
#'   x = plants_df[, plants_predictors]
#' )
#'
#' y$selected.variables
#' y$cor
#' head(y$selected.variables.df)


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

#' case_weights(
#'   data = data.frame(
#'     response = c(0, 0, 0, 1, 1)
#'   ),
#'   dependent.variable.name = "response"
#' )


# default_distance_thresholds ----

#' data(plants_distance)
#'
#' default_distance_thresholds(
#'   distance.matrix = plants_distance
#' )


# double_center_distance_matrix ----

#' data(plants_distance)
#'
#' double_center_distance_matrix(
#'   distance.matrix = plants_distance
#' ) %>%
#'   head()


# filter_spatial_predictors ----

#' data(
#'   plants_df,
#'   plants_predictors,
#'   plants_distance
#' )
#'
#' #generate spatial predictors
#' mem.df <- mem_multithreshold(
#'   distance.matrix = plants_distance,
#'   distance.thresholds = c(0, 1000)
#' )
#'
#' #remove spatial predictors correlated > 0.50 with environmental predictors
#' spatial.predictors.df <- filter_spatial_predictors(
#'   data = plants_df,
#'   predictor.variable.names = plants_predictors,
#'   spatial.predictors.df = mem.df,
#'   cor.threshold = 0.50
#' )


# get_evaluation ----

#' data(
#'   plants_rf,
#'   plants_xy
#' )
#'
#' m_evaluated <- rf_evaluate(
#'   model = plants_rf,
#'   xy = plants_xy,
#'   repetitions = 5,
#'   n.cores = 1
#' )
#'
#' plot_evaluation(m_evaluated, notch = FALSE)
#'
#' print_evaluation(m_evaluated)
#'
#' get_evaluation(m_evaluated)

# get_importance_local ----

#' data(plants_rf)
#'
#' get_importance_local(plants_rf) %>%
#'   head()


# get_importance ----

#' data(plants_rf)
#'
#' get_importance(plants_rf)


# get_moran ----

#' data(plants_rf)
#'
#' get_moran(plants_rf)


# get_performance ----

#' data(plants_rf)
#'
#' get_performance(plants_rf)


# get_predictions ----

#' data(plants_rf)
#'
#' get_predictions(plants_rf) %>%
#'   head()


# get_residuals ----

#' data(plants_rf)
#'
#' get_residuals(plants_rf) %>%
#'   head()


# get_response_curves ----

#' data(plants_rf)
#'
#' get_response_curves(plants_rf) %>%
#'   head()


# get_spatial_predictors ----

#' data(plants_rf_spatial)
#'
#' get_spatial_predictors(plants_rf_spatial) %>%
#'   head()


# is_binary ----

#' is_binary(
#'   data = data.frame(
#'     response = c(0, 0, 0, 1, 1)
#'   ),
#'   dependent.variable.name = "response"
#' )


# make_spatial_fold ----

#' data(
#'   plants_df,
#'   plants_xy
#' )
#'
#' #spatial fold centered on first coordinate
#' y <- make_spatial_fold(
#'   xy.i = plants_xy[1, ],
#'   xy = plants_xy,
#'   training.fraction = 0.6
#' )
#'
#' y$training
#' y$testing
#'
#' if (interactive()) {
#'   plot(plants_xy[c("x", "y")], type = "n", xlab = "", ylab = "")
#'   points(plants_xy[y$training, c("x", "y")], col = "red4", pch = 15)
#'   points(plants_xy[y$testing, c("x", "y")], col = "blue4", pch = 15)
#'   points(plants_xy[1, c("x", "y")], col = "black", pch = 15, cex = 2)
#' }


# make_spatial_folds ----

#' data(
#'   plants_df,
#'   plants_xy
#' )
#'
#' #thin to 10 cases to speed up example
#' xy.thin <- thinning_til_n(
#'   xy = plants_xy,
#'   n = 10
#' )
#'
#' #create spatial folds centered on 10 thinned cases
#' y <- make_spatial_folds(
#'   xy.selected = xy.thin,
#'   xy = plants_xy,
#'   distance.step.x = 0.05,
#'   training.fraction = 0.6,
#'   n.cores = 1
#' )
#'
#' if (interactive()) {
#'   plot(plants_xy[c("x", "y")], type = "n", xlab = "", ylab = "")
#'   points(plants_xy[y[[1]]$training, c("x", "y")], col = "red4", pch = 15)
#'   points(plants_xy[y[[1]]$testing, c("x", "y")], col = "blue4", pch = 15)
#'   points(
#'     plants_xy[y[[1]]$training[1], c("x", "y")],
#'     col = "black",
#'     pch = 15,
#'     cex = 2
#'   )
#' }


# mem_multithreshold ----

#' data(plants_distance)
#'
#' #Moran's eigenvector maps for multiple distance thresholds
#' mem.df <- mem_multithreshold(
#'   distance.matrix = plants_distance,
#'   distance.thresholds = c(0, 1000, 5000)
#' )
#' head(mem.df)


# mem ----

#' data(plants_distance)
#'
#' y <- mem(distance.matrix = plants_distance)
#'
#' head(y)


# moran_multithreshold ----

#' data(
#'   plants_df,
#'   plants_distance,
#'   plants_response
#' )
#'
#' #Moran's I at multiple distance thresholds
#' y <- moran_multithreshold(
#'   x = plants_df[[plants_response]],
#'   distance.matrix = plants_distance,
#'   distance.thresholds = c(0, 1000, 5000)
#' )
#' y


# moran ----

#' data(
#'   plants_df,
#'   plants_distance,
#'   plants_response
#' )
#'
#' y <- moran(
#'   x = plants_df[[plants_response]],
#'   distance.matrix = plants_distance,
#'   distance.threshold = 1000
#' )
#' y


# normality ----

#' residuals_test(residuals = runif(100))


# objects_size ----

#' x <- matrix(runif(100), 10, 10)
#' y <- matrix(runif(10000), 100, 100)
#'
#' objects_size()


# pca_multithreshold ----

#' data(plants_distance)
#'
#' #PCA factors for multiple distance thresholds
#' y <- pca_multithreshold(
#'   distance.matrix = plants_distance,
#'   distance.thresholds = c(0, 1000)
#' )
#' head(y)


# pca ----

#' data(plants_distance)
#'
#' y <- pca(x = plants_distance)
#' y


# plot_evaluation ----

#' data(
#'   plants_rf,
#'   plants_xy
#' )
#'
#' m_evaluated <- rf_evaluate(
#'   model = plants_rf,
#'   xy = plants_xy,
#'   repetitions = 5,
#'   n.cores = 1
#' )
#'
#' plot_evaluation(m_evaluated, notch = FALSE)
#'
#' print_evaluation(m_evaluated)
#'
#' get_evaluation(m_evaluated)

# plot_importance ----

#' data(plants_rf)
#'
#' plot_importance(plants_rf)


# plot_moran ----

#' data(plants_rf)
#'
#' plot_moran(plants_rf)
#'
#' plot_moran(plants_rf, option = 2)


# plot_optimization ----

#' data(plants_rf_spatial)
#'
#' plot_optimization(plants_rf_spatial)


# plot_residuals_diagnostics ----

#' data(plants_rf)
#'
#' plot_residuals_diagnostics(plants_rf)


# plot_response_curves ----

#' data(plants_rf)
#'
#' plot_response_curves(
#'   model = plants_rf,
#'   variables = "climate_bio1_average"
#' )
#'
#' plot_response_curves(
#'   model = plants_rf,
#'   variables = "climate_bio1_average",
#'   show.data = TRUE
#' )


# plot_response_surface ----

#' data(plants_rf)
#'
#' plot_response_surface(
#'   model = plants_rf,
#'   a = "climate_bio1_average",
#'   b = "human_population",
#'   grid.resolution = 50
#' )


# plot_training_df_moran ----

#' data(
#'   plants_df,
#'   plants_response,
#'   plants_predictors,
#'   plants_distance
#' )
#'
#' plot_training_df_moran(
#'   data = plants_df,
#'   dependent.variable.name = plants_response,
#'   predictor.variable.names = plants_predictors[1:4],
#'   distance.matrix = plants_distance,
#'   distance.thresholds = c(1000, 2000, 4000)
#' )


# plot_training_df ----

#' data(
#'   plants_df,
#'   plants_response,
#'   plants_predictors
#' )
#'
#' plot_training_df(
#'   data = plants_df,
#'   dependent.variable.name = plants_response,
#'   predictor.variable.names = plants_predictors[1:4]
#' )


# plot_tuning ----

#' if(interactive(){
#'   data(
#'     plants_rf,
#'     plants_xy
#'   )
#'
#'   plants_rf_tuned <- rf_tuning(
#'     model = plants_rf,
#'     num.trees = c(25, 50),
#'     mtry = c(5, 10),
#'     min.node.size = c(10, 20),
#'     xy = plants_xy,
#'     repetitions = 5,
#'     n.cores = 1
#'   )
#'
#'   plot_tuning(plants_rf_tuned)
#' }

# prepare_importance_spatial ----

#' data(plants_rf_spatial)
#'
#' prepare_importance_spatial(plants_rf_spatial) %>%
#'   head()


# print_evaluation ----

#' data(
#'   plants_rf,
#'   plants_xy
#' )
#'
#' m_evaluated <- rf_evaluate(
#'   model = plants_rf,
#'   xy = plants_xy,
#'   repetitions = 5,
#'   n.cores = 1
#' )
#'
#' plot_evaluation(m_evaluated, notch = FALSE)
#'
#' print_evaluation(m_evaluated)
#'
#' get_evaluation(m_evaluated)

# print_importance ----

#' data(plants_rf)
#'
#' print_importance(plants_rf)


# print_moran ----

#' data(plants_rf)
#'
#' print_moran(plants_rf)


# print_performance ----

#' data(plants_rf)
#'
#' print_performance(plants_rf)


# print ----

#' data(plants_rf)
#'
#' print(plants_rf)
#'
#' #or
#' plants_rf


# rank_spatial_predictors ----

#' data(
#'   plants_df,
#'   plants_response,
#'   plants_distance
#' )
#'
#' #subset to speed up example
#' idx <- 50:90
#' plants_distance_sub <- plants_distance[idx, idx]
#'
#' y <- mem(
#'   distance.matrix = plants_distance_sub,
#'   distance.threshold = 1000
#' )
#'
#' #rank spatial predictors by Moran's I
#' y_rank <- rank_spatial_predictors(
#'   distance.matrix = plants_distance_sub,
#'   distance.thresholds = 1000,
#'   spatial.predictors.df = y,
#'   ranking.method = "moran",
#'   n.cores = 1
#' )
#'
#' y_rank$criteria
#' y_rank$ranking
#'
#' #rank spatial predictors by association with response
#' y_rank <- rank_spatial_predictors(
#'   data = plants_df[idx, ],
#'   dependent.variable.name = plants_response,
#'   distance.matrix = plants_distance_sub,
#'   distance.thresholds = 1000,
#'   spatial.predictors.df = y,
#'   ranking.method = "effect",
#'   n.cores = 1
#' )
#'
#' y_rank$criteria
#' y_rank$ranking


# rescale_vector ----

#' y <- rescale_vector(
#'   x = rnorm(100),
#'   new.min = 0,
#'   new.max = 100,
#'   integer = TRUE
#' )
#' y


# residuals_diagnostics ----

#' data(plants_rf)
#'
#' y <- residuals_diagnostics(
#'   residuals = get_residuals(plants_rf),
#'   predictions = get_predictions(plants_rf)
#' )
#' y


# rf_compare ----

#' data(
#'   plants_rf,
#'   plants_rf_spatial,
#'   plants_xy
#' )
#'
#' comparison <- rf_compare(
#'   models = list(
#'     `Non spatial` = plants_rf,
#'     Spatial = plants_rf_spatial
#'   ),
#'   repetitions = 5,
#'   xy = plants_xy,
#'   metrics = "rmse",
#'   n.cores = 1
#' )

# rf_evaluate ----

#' data(
#'   plants_rf,
#'   plants_xy
#' )
#'
#' m_evaluated <- rf_evaluate(
#'   model = plants_rf,
#'   xy = plants_xy,
#'   repetitions = 5,
#'   n.cores = 1
#' )
#'
#' plot_evaluation(m_evaluated, notch = FALSE)
#'
#' print_evaluation(m_evaluated)
#'
#' get_evaluation(m_evaluated)

# rf_importance ----

#' if(interactive()){
#'   data(plants_rf)
#'
#'   m_importance <- rf_importance(
#'     model = plants_rf,
#'     repetitions = 5
#'   )
#' }

# rf_repeat ----

#' if(interactive()){
#'
#'   data(plants_rf)
#'
#'   m_repeat <- rf_repeat(
#'     model = plants_rf,
#'     repetitions = 5,
#'     n.cores = 1
#'   )
#'
#'   #performance scores across repetitions
#'   m_repeat$performance
#'   print_performance(m_repeat)
#'
#'   #variable importance
#'   plot_importance(m_repeat)
#'
#'   #response curves
#'   plot_response_curves(
#'     model = m_repeat,
#'     variables = "climate_bio1_average",
#'     quantiles = 0.5
#'   )
#'
#' }

# rf_spatial ----

#' if (interactive()) {
#'   data(
#'     plants_df,
#'     plants_response,
#'     plants_predictors,
#'     plants_distance,
#'     plants_rf
#'   )
#'
#'   #subset to speed up example
#'   idx <- 1:100
#'   plants_df <- plants_df[idx, ]
#'   plants_distance <- plants_distance[idx, idx]
#'
#'   #fit spatial model from scratch
#'   m_spatial <- rf_spatial(
#'     data = plants_df,
#'     dependent.variable.name = plants_response,
#'     predictor.variable.names = plants_predictors,
#'     distance.matrix = plants_distance,
#'     distance.thresholds = c(100, 1000, 2000),
#'     method = "mem.moran.sequential",
#'     ranger.arguments = list(num.trees = 30),
#'     n.cores = 1
#'   )
#'
#'   plot_residuals_diagnostics(m_spatial)
#'
#'   #optimization of MEM selection
#'   plot_optimization(m_spatial)
#'
#'   #from non-spatial to spatial model
#'   m_spatial <- rf_spatial(
#'     model = plants_rf
#'     )
#'
#' }
#'

# rf_tuning ----

#' if(interactive(){
#'   data(
#'     plants_rf,
#'     plants_xy
#'   )
#'
#'   plants_rf_tuned <- rf_tuning(
#'     model = plants_rf,
#'     num.trees = c(25, 50),
#'     mtry = c(5, 10),
#'     min.node.size = c(10, 20),
#'     xy = plants_xy,
#'     repetitions = 5,
#'     n.cores = 1
#'   )
#'
#'   plot_tuning(plants_rf_tuned)
#' }

# rf ----

#' data(
#'   plants_df,
#'   plants_response,
#'   plants_predictors,
#'   plants_distance
#' )
#'
#' m <- rf(
#'   data = plants_df,
#'   dependent.variable.name = plants_response,
#'   predictor.variable.names = plants_predictors,
#'   distance.matrix = plants_distance,
#'   distance.thresholds = c(100, 1000, 2000),
#'   ranger.arguments = list(
#'     num.trees = 50,
#'     min.node.size = 20
#'   ),
#'   verbose = FALSE,
#'   n.cores = 1
#' )
#'
#' class(m)
#' #variable importance
#' m$importance$per.variable
#' m$importance$per.variable.plot
#'
#' #model performance
#' m$performance
#'
#' #autocorrelation of residuals
#' m$residuals$autocorrelation$per.distance
#' m$residuals$autocorrelation$plot
#'
#' #model predictions
#' m$predictions$values
#'
#' #predictions for new data (using stats::predict)
#' y <- stats::predict(
#'   object = m,
#'   data = plants_df[1:5, ],
#'   type = "response"
#' )$predictions
#'
#' #alternative: pass arguments via ranger.arguments list
#' args <- list(
#'   data = plants_df,
#'   dependent.variable.name = plants_response,
#'   predictor.variable.names = plants_predictors,
#'   distance.matrix = plants_distance,
#'   distance.thresholds = c(100, 1000, 2000),
#'   num.trees = 50,
#'   min.node.size = 20,
#'   num.threads = 1
#' )
#'
#' m <- rf(
#'   ranger.arguments = args,
#'   verbose = FALSE
#' )

# root_mean_squared_error ----

#' root_mean_squared_error(
#'   o = runif(10),
#'   p = runif(10)
#' )

# select_spatial_predictors_recursive ----

#' if (interactive()) {
#'   data(
#'     plants_df,
#'     plants_response,
#'     plants_predictors,
#'     plants_distance,
#'     plants_rf
#'   )
#'
#'   #subset to speed up example
#'   idx <- 1:20
#'   plants_df <- plants_df[idx, ]
#'   plants_distance <- plants_distance[idx, idx]
#'
#'   #generate spatial predictors
#'   mems <- mem_multithreshold(
#'     distance.matrix = plants_distance,
#'     distance.thresholds = 100
#'   )
#'
#'   #rank them from higher to lower moran
#'   mems.rank <- rank_spatial_predictors(
#'     ranking.method = "moran",
#'     spatial.predictors.df = mems,
#'     reference.moran.i = plants_rf$residuals$autocorrelation$max.moran,
#'     distance.matrix = plants_distance,
#'     distance.thresholds = 100,
#'     n.cores = 1
#'   )
#'
#'   #select best subset via sequential addition
#'   selection <- select_spatial_predictors_recursive(
#'     data = plants_df,
#'     dependent.variable.name = plants_response,
#'     predictor.variable.names = plants_predictors,
#'     distance.matrix = plants_distance,
#'     distance.thresholds = 0,
#'     spatial.predictors.df = mems,
#'     spatial.predictors.ranking = mems.rank,
#'     ranger.arguments = list(num.trees = 30),
#'     n.cores = 1
#'   )
#'
#'   #names of selected spatial predictors
#'   selection$best.spatial.predictors
#'
#'   #optimization plot
#'   plot_optimization(selection$optimization)
#' }

# select_spatial_predictors_sequential ----

#' data(
#'   plants_df,
#'   plants_response,
#'   plants_predictors,
#'   plants_distance,
#'   plants_rf
#' )
#'
#' #subset to speed up example
#' idx <- 1:20
#' plants_df <- plants_df[idx, ]
#' plants_distance <- plants_distance[idx, idx]
#'
#' #generate spatial predictors
#' mems <- mem_multithreshold(
#'   distance.matrix = plants_distance,
#'   distance.thresholds = 100
#' )
#'
#' #rank them from higher to lower moran
#' mems.rank <- rank_spatial_predictors(
#'   ranking.method = "moran",
#'   spatial.predictors.df = mems,
#'   reference.moran.i = plants_rf$residuals$autocorrelation$max.moran,
#'   distance.matrix = plants_distance,
#'   distance.thresholds = 100,
#'   n.cores = 1
#' )
#'
#' #select best subset via sequential addition
#' selection <- select_spatial_predictors_sequential(
#'   data = plants_df,
#'   dependent.variable.name = plants_response,
#'   predictor.variable.names = plants_predictors,
#'   distance.matrix = plants_distance,
#'   distance.thresholds = 0,
#'   spatial.predictors.df = mems,
#'   spatial.predictors.ranking = mems.rank,
#'   ranger.arguments = list(num.trees = 30),
#'   n.cores = 1
#' )
#'
#' #names of selected spatial predictors
#' selection$best.spatial.predictors
#'
#' #optimization plot
#' plot_optimization(selection$optimization)

# standard_error ----

#' standard_error(x = runif(10))

# statistical_mode ----

#' statistical_mode(x = c(10, 9, 10, 8))

# the_feature_engineer ----

#'
#' if (interactive()) {
#'   data(
#'     plants_df,
#'     plants_response,
#'     plants_predictors,
#'     plants_xy,
#'     plants_rf
#'   )
#'
#'   #get five most important predictors from plants_rf to speed-up example
#'   predictors <- get_importance(plants_rf)[1:5, "variable"]
#'
#'   #subset to speed-up example
#'   idx <- 1:30
#'   plants_df <- plants_df[idx, ]
#'   plants_xy <- plants_xy[idx, ]
#'
#'   #data subsetted to speed-up example runtime
#'   y <- the_feature_engineer(
#'     data = plants_df,
#'     dependent.variable.name = plants_response,
#'     predictor.variable.names = predictors,
#'     xy = plants_xy,
#'     repetitions = 5,
#'     n.cores = 1,
#'     ranger.arguments = list(
#'       num.trees = 30
#'     ),
#'     verbose = TRUE
#'   )
#'
#'   #all tested interactions
#'   y$screening
#'
#'   #selected interaction (same as above in this case)
#'   y$selected
#'
#'   #new column added to data
#'   head(y$data[, y$selected$interaction.name])
#' }

# thinning_til_n ----

#' data(plants_xy)
#'
#' y <- thinning_til_n(
#'   xy = plants_xy,
#'   n = 10
#' )
#'
#' if (interactive()) {
#'   plot(
#'     plants_xy[, c("x", "y")],
#'     col = "blue",
#'     pch = 15
#'   )
#'
#'   points(
#'     y[, c("x", "y")],
#'     col = "red",
#'     pch = 15,
#'     cex = 1.5
#'   )
#' }

# thinning ----

#' data(plants_xy)
#'
#' y <- thinning(
#'   xy = plants_xy,
#'   minimum.distance = 10
#' )
#'
#' if (interactive()) {
#'   plot(
#'     plants_xy[, c("x", "y")],
#'     col = "blue",
#'     pch = 15
#'   )
#'
#'   points(
#'     y[, c("x", "y")],
#'     col = "red",
#'     pch = 15
#'   )
#' }

# weights_from_distance_matrix ----

#' data(plants_distance)
#'
#' y <- weights_from_distance_matrix(
#'   distance.matrix = plants_distance
#' )
#'
#' y[1:5, 1:5]
