library(spatialRF)
library(tictoc)

#loading example data
data(
  ecoregions_df,
  ecoregions_distance_matrix,
  ecoregions_predvar_names,
  ecoregions_depvar_name
)

#automatic variable selection
tic()
rf.selection <- rf_select(
  data = ecoregions_df,
  dependent.variable.name = ecoregions_depvar_name,
  predictor.variable.names = ecoregions_predvar_names,
  xy = ecoregions_df[, c("x", "y")],
  jackknife = TRUE
)
toc()

tic()
rf.selection <- rf_select(
  data = ecoregions_df,
  dependent.variable.name = ecoregions_depvar_name,
  predictor.variable.names = ecoregions_predvar_names,
  xy = ecoregions_df[, c("x", "y")],
  jackknife = TRUE,
  cluster = make_cluster()
)
toc()

spatialRF::stop_cluster()

#the result is a model!
plot_importance(rf.selection)
print_performance(rf.selection)

#you can use this model as input for other functions
rf.selection <- rf_evaluate(
  model = rf.selection
)

#or you can connect it with other modelling functions using the %>% pìpe
rf.selection <- rf_select(
  data = ecoregions_df,
  dependent.variable.name = ecoregions_depvar_name,
  predictor.variable.names = ecoregions_predvar_names,
  xy = ecoregions_df[, c("x", "y")],
  n.cores = 1
) %>%
  rf_evaluate()

#example of complete pipeline (this will take a while to execute)
cl <- make_cluster()

rf.selection <- rf_select(
  data = ecoregions_df,
  dependent.variable.name = ecoregions_depvar_name,
  predictor.variable.names = ecoregions_predvar_names,
  distance.matrix = ecoregions_distance_matrix,
  xy = ecoregions_df[, c("x", "y")],
  n.cores = 1
) %>%
  rf_tuning(cluster = cl) %>%
  rf_spatial(cluster = cl) %>%
  rf_evaluate(cluster = cl) %>%
  rf_importance(cluster = cl)

#automatic variable selection with jackknife
rf.selection <- rf_select(
  data = ecoregions_df,
  dependent.variable.name = ecoregions_depvar_name,
  predictor.variable.names = ecoregions_predvar_names,
  jackknife = TRUE,
  n.cores = 1
)

#variable selection with preference order
rf.selection <- rf_select(
  data = ecoregions_df,
  dependent.variable.name = ecoregions_depvar_name,
  predictor.variable.names = ecoregions_predvar_names,
  preference.order = c(
    "climate_bio5_average",
    "climate_hypervolume",
    "human_population_density"
  ),
  n.cores = 1
)

#variable selection with preference order and jackknife
rf.selection <- rf_select(
  data = ecoregions_df,
  dependent.variable.name = ecoregions_depvar_name,
  predictor.variable.names = ecoregions_predvar_names,
  preference.order = c(
    "climate_bio5_average",
    "climate_hypervolume",
    "human_population_density"
  ),
  jackknife = TRUE,
  n.cores = 1
)

#variable selection with preference order, jackknife, and repetitions (for more robust estimates of importance but higher computational cost)
rf.selection <- rf_select(
  data = ecoregions_df,
  dependent.variable.name = ecoregions_depvar_name,
  predictor.variable.names = ecoregions_predvar_names,
  preference.order = c(
    "climate_bio5_average",
    "climate_hypervolume",
    "human_population_density"
  ),
  jackknife = TRUE,
  repetitions = 10,
  n.cores = 1
)
