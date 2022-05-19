#rf() WORKS
#rf_repeat() WORKS
#rf_evaluate() WORKS
#make_spatial_folds() WORKS
#rf_compare() WORKS
#rf_importance() WORKS
#rf_tuning() WORKS
#the_feature_engineer() WORKS
#rf_spatial() WORKS

library(tictoc)
library(spatialRF)

#local cluster
cluster <- parallel::makeCluster(
  spec = parallel::detectCores() - 1,
  type = "PSOCK"
)

#network cluster
# cluster <- beowulf_cluster(
#  cluster.ips = c(
#    "10.42.0.1",
#    "10.42.0.34",
#    "10.42.0.104"
#    ),
# cluster.cores = c(7, 4, 4),
# cluster.user = "blas",
# cluster.port = "11000"
# )

#regular model no cluster required
rf.model <- spatialRF::rf(
  data = ecoregions_df,
  dependent.variable.name = ecoregions_depvar_name,
  predictor.variable.names = ecoregions_predvar_names,
  distance.matrix = ecoregions_distance_matrix,
  xy = ecoregions_df[, c("x", "y")],
  verbose = FALSE,
  n.cores = parallel::detectCores() - 1
)

#rf_repeat WORKS!
######################


#with n.cores
tic()
rf.model <- spatialRF::rf_repeat(
  model = rf.model,
  verbose = FALSE,
  n.cores = parallel::detectCores() - 1,
  cluster = NULL
)
toc()
#3.991 sec elapsed

#with cluster
tic()
rf.model <- spatialRF::rf_repeat(
  model = rf.model,
  verbose = FALSE,
  n.cores = parallel::detectCores() - 1,
  cluster = cluster
)
toc()
#10.789 sec elapsed


#rf_evaluate WORKS!
####################

#with n.cores
tic()
rf.model <- spatialRF::rf_evaluate(
  model = rf.model,
  n.cores = parallel::detectCores() - 1,
  cluster = NULL
)
toc()
#5.277 sec elapsed

#with cluster
tic()
rf.model <- spatialRF::rf_evaluate(
  model = rf.model,
  n.cores = parallel::detectCores() - 1,
  cluster = cluster
)
toc()
#3.213 sec elapsed


#make_spatial_folds() WORKS
#####################

xy <- ecoregions_df[, c("x", "y")]

ecoregions_df$id <- xy$id <- seq(1, nrow(ecoregions_df))

xy.selected <- thinning_til_n(xy = xy, n = 50)

#with n.cores
tic()
x <- spatialRF::make_spatial_folds(
  data = ecoregions_df,
  dependent.variable.name = ecoregions_depvar_name,
  xy.selected = xy.selected,
  xy = xy,
  n.cores = parallel::detectCores() - 1,
  cluster = NULL
)
toc()
#1.253 sec elapsed

#with cluster
tic()
x <- spatialRF::make_spatial_folds(
  data = ecoregions_df,
  dependent.variable.name = ecoregions_depvar_name,
  xy.selected = xy.selected,
  xy = xy,
  n.cores = parallel::detectCores() - 1,
  cluster = cluster
)
toc()
#0.947 sec elapsed


#rf_compare() WORKS
#############

#with n.cores
tic()
x <- spatialRF::rf_compare(
  models = list(
    a = rf.model,
    b = rf.model
  ),
  n.cores = parallel::detectCores() - 1,
  cluster = NULL
)
toc()
#10.793 sec elapsed


#with cluster
tic()
x <- spatialRF::rf_compare(
  models = list(
    a = rf.model,
    b = rf.model
  ),
  n.cores = parallel::detectCores() - 1,
  cluster = cluster
)
toc()
#5.554 sec elapsed


# rf_importance() WORKS
#################

#with n.cores
tic()
rf.model <- spatialRF::rf_importance(
  model = rf.model,
  n.cores = parallel::detectCores() - 1,
  cluster = NULL
)
toc()
#221.949 sec elapsed

#with cluster
tic()
rf.model <- spatialRF::rf_importance(
  model = rf.model,
  n.cores = parallel::detectCores() - 1,
  cluster = cluster
)
toc()
#90.624 sec elapsed


#rf_tuning() WORKS
#####################

#with n.cores
tic()
rf.model <- spatialRF::rf_tuning(
  model = rf.model,
  n.cores = parallel::detectCores() - 1,
  cluster = NULL
)
toc()
#2231.351 sec elapsed

#with cluster
tic()
rf.model <- spatialRF::rf_tuning(
  model = rf.model,
  n.cores = parallel::detectCores() - 1,
  cluster = cluster
)
toc()
#577.303 sec elapsed


#the_feature_engineer() WORKS
########################

#with n.cores
tic()
x <- spatialRF::the_feature_engineer(
  data = ecoregions_df,
  dependent.variable.name = ecoregions_depvar_name,
  predictor.variable.names = ecoregions_predvar_names,
  xy = ecoregions_df[, c("x", "y")],
  n.cores = parallel::detectCores() - 1,
  cluster = NULL
)
toc()
#278.972 sec elapsed

#with cluster
tic()
x <- spatialRF::the_feature_engineer(
  data = ecoregions_df,
  dependent.variable.name = ecoregions_depvar_name,
  predictor.variable.names = ecoregions_predvar_names,
  xy = ecoregions_df[, c("x", "y")],
  n.cores = parallel::detectCores() - 1,
  cluster = cluster
)
toc()
#86.074 sec elapsed


#rf_spatial() WORKS
##################

#with n.cores
tic()
rf.model.spatial <- rf_spatial(
  model = rf.model,
  method = "mem.moran.sequential",
  n.cores = parallel::detectCores() - 1,
  cluster = NULL,
  verbose = FALSE
)
toc()
#26.426 sec elapsed

#with cluster
tic()
rf.model.spatial <- rf_spatial(
  model = rf.model,
  method = "mem.moran.sequential",
  n.cores = parallel::detectCores() - 1,
  cluster = cluster,
  verbose = FALSE
)
toc()
#8.915 sec elapsed

#with n.cores
tic()
rf.model.spatial <- rf_spatial(
  model = rf.model,
  method = "mem.effect.recursive",
  n.cores = parallel::detectCores() - 1,
  cluster = NULL,
  verbose = FALSE
)
toc()
# 542.019 sec elapsed

#with cluster
tic()
rf.model.spatial <- rf_spatial(
  model = rf.model,
  method = "mem.effect.recursive",
  n.cores = parallel::detectCores() - 1,
  cluster = cluster,
  verbose = FALSE
)
toc()
# 258.065 sec elapsed




#stopping cluster
parallel::stopCluster(cl = cluster)

