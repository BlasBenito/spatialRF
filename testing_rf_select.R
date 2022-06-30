library(spatialRF)
library(doParallel)
library(tidyverse)
library(tictoc)

#loading example data
data(
  ecoregions_df,
  ecoregions_distance_matrix,
  ecoregions_predvar_names,
  ecoregions_depvar_name
  )

#auto variable selection without cluster
m <- rf_select(
  data = ecoregions_df,
  dependent.variable.name = ecoregions_depvar_name,
  predictor.variable.names = ecoregions_predvar_names,
  jackknife = TRUE
)

m.custom <- rf(
  model = m,
  ranger.arguments = list(
    num.tres = 5000
  )
)

#auto variable selection with cluster
tic()
selected.variables <- rf_select(
  data = ecoregions_df,
  dependent.variable.name = ecoregions_depvar_name,
  predictor.variable.names = ecoregions_predvar_names,
  cluster = spatialRF::make_cluster()
)
toc()

#variable selection with jackknife


#using preference order
selected.variables <- rf_select(
  data = ecoregions_df,
  dependent.variable.name = ecoregions_depvar_name,
  predictor.variable.names = ecoregions_predvar_names,
  preference.order = ecoregions_predvar_names[1:3]
)

selected.variables <- rf_select(
  data = ecoregions_df,
  dependent.variable.name = ecoregions_depvar_name,
  predictor.variable.names = ecoregions_predvar_names,
  preference.order = ecoregions_predvar_names[1:3],
  jackknife = TRUE
)

selected.variables <- rf_select(
  data = ecoregions_df,
  dependent.variable.name = ecoregions_depvar_name,
  predictor.variable.names = ecoregions_predvar_names,
  jackknife = TRUE
)

selected.variables <- rf_select(
  data = ecoregions_df,
  dependent.variable.name = ecoregions_depvar_name,
  predictor.variable.names = ecoregions_predvar_names,
  jackknife = FALSE
)

