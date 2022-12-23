#' #loading example data
data(
  ecoregions_df,
  ecoregions_sf,
  ecoregions_tibble,
  ecoregions_continuous_response,
  ecoregions_binary_response,
  ecoregions_all_predictors
  )

ecoregions_df <- ecoregions_sf

#the dataframe ecoregions_df contains two categorical variables
unique(ecoregions_df$dominant_landcover)
unique(ecoregions_df$primary_productivity)

#applying all methods for a continuous response
output <- fe_target_encoding(
  data = ecoregions_df,
  dependent.variable.name = ecoregions_continuous_response,
  predictor.variable.names = ecoregions_all_predictors,
  methods = c(
    "mean",
    "rank",
    "rnorm",
    "loo"
  ),
  sd.width = c(0.01, 0.1, 1),
  noise = c(0, 1)
)

#the output has several objects
names(output)

#names of the encoded predictors
output$encoded_predictors

#the data with the original and the encoded predictors
colnames(output$data)

#a leakage test assessing the correlation between the response and the encoded predictors
output$leakage_test

#plotting the transformations of "primary_productivity"
tidyr::pivot_longer(
  data = output$data,
  cols = dplyr::all_of(
    grep(
      pattern = "primary_productivity",
      x = output$encoded_predictors,
      value = TRUE)
    )
) %>%
  dplyr::select(
    plant_richness,
    primary_productivity,
    name,
    value
  ) %>%
  ggplot2::ggplot() +
  ggplot2::aes(
    x = plant_richness,
    y = value,
    color = primary_productivity
  ) +
  ggplot2::facet_wrap(~name, scales = "free_y") +
  ggplot2::geom_point() +
  ggplot2::labs(
    x = "Response values",
    y = "Encoded values",
    color = "Original\ngroups"
  )





#target_encoding mean
data(
  ecoregions_df,
  ecoregions_continuous_response,
  ecoregions_all_predictors
)

#the dataframe ecoregions_df contains a categorical variable named primary_productivity
unique(ecoregions_df$primary_productivity)

#transforming primary_productivity
ecoregions_df <- fe_target_encoding_loo(
  data = ecoregions_df,
  dependent.variable.name = ecoregions_continuous_response,
  categorical.variable.name = "primary_productivity"
  )

#the encoded variable is named primary_productivity__encoded_loo

#plotting it against the response
ggplot2::ggplot(data = ecoregions_df) +
  ggplot2::aes(
    x = plant_richness,
    y = primary_productivity__encoded_loo,
    color = primary_productivity
  ) +
  ggplot2::geom_point()
