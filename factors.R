library(spatialRF)
library(tidyverse)
library(ranger)

#loading data
data(
  ecoregions_df,
  ecoregions_continuous_response,
  ecoregions_numeric_predictors
  )

#converting a few variables into factors
ecoregions_df <- ecoregions_df %>%
  dplyr::mutate(
    human_footprint_average = dplyr::ntile(human_footprint_average, 30) %>% factor(ordered = TRUE),
    climate_aridity_index_average = dplyr::ntile(human_footprint_average, 20) %>% factor(ordered = TRUE)
    )

model.ranger <- ranger::ranger(
  dependent.variable.name = ecoregions_continuous_response,
  data = ecoregions_df[, c(
    ecoregions_continuous_response,
    ecoregions_numeric_predictors)],
  importance = "permutation"
)



