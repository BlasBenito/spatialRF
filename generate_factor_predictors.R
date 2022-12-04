library(dplyr)
library(magrittr)

data(
  ecoregions_df,
  ecoregions_tibble,
  ecoregions_sf
)

ecoregions_df$ndvi_character_ordered <- NULL
ecoregions_df$landlocked_binary <- NULL
ecoregions_df$landcover_factor_unordered <- NULL

ecoregions_tibble$ndvi_character_ordered <- NULL
ecoregions_tibble$landlocked_binary <- NULL
ecoregions_tibble$landcover_factor_unordered <- NULL

ecoregions_sf$ndvi_character_ordered <- NULL
ecoregions_sf$landlocked_binary <- NULL
ecoregions_sf$landcover_factor_unordered <- NULL


#plant_cover_character and landlocked_boolean
ecoregions_df <- ecoregions_df %>%
  dplyr::mutate(primary_productivity = dplyr::case_when(
    landcover_ndvi_average < -0.1                               ~ "abysmal",
    landcover_ndvi_average >= -0.1 & landcover_ndvi_average < 0 ~ "ultra low",
    landcover_ndvi_average >= 0 & landcover_ndvi_average < 0.1 ~ "rather low",
    landcover_ndvi_average >= 0.1 & landcover_ndvi_average < 0.2 ~ "quite low",
    landcover_ndvi_average >= 0.2 & landcover_ndvi_average < 0.3 ~ "just low",
    landcover_ndvi_average >= 0.3 & landcover_ndvi_average < 0.4 ~ "not bad not great",
    landcover_ndvi_average >= 0.4 & landcover_ndvi_average < 0.5 ~ "just better",
    landcover_ndvi_average >= 0.5 & landcover_ndvi_average < 0.6 ~ "quite good",
    landcover_ndvi_average >= 0.6 & landcover_ndvi_average < 0.7 ~ "rather good",
    landcover_ndvi_average >= 0.7 & landcover_ndvi_average < 0.8 ~ "ultra good",
    landcover_ndvi_average >= 0.8 & landcover_ndvi_average < 0.9 ~ "awesome",
    TRUE                                                         ~ "unknown"
  )
  ) %>%
  dplyr::mutate(
    landlocked = dplyr::case_when(
      neighbors_percent_shared_edge < 99.9 ~ 0,
      neighbors_percent_shared_edge >= 99.9 ~ 1
    )
  )

#dominant dominant_landcover
temp_df <- ecoregions_df %>%
  dplyr::select(
    landcover_bare_percent_average,
    landcover_herbs_percent_average,
    landcover_trees_percent_average
  ) %>%
  dplyr::rename(
    bare = landcover_bare_percent_average,
    herbs = landcover_herbs_percent_average,
    trees = landcover_trees_percent_average
  )
ecoregions_df$dominant_landcover <- colnames(temp_df)[apply(temp_df,1,which.max)]
ecoregions_df$dominant_landcover <- factor(ecoregions_df$dominant_landcover)

usethis::use_data(ecoregions_df, overwrite = TRUE)




#plant_cover_character and landlocked_boolean
ecoregions_tibble <- ecoregions_tibble %>%
  dplyr::mutate(primary_productivity = dplyr::case_when(
    landcover_ndvi_average < -0.1                               ~ "abysmal",
    landcover_ndvi_average >= -0.1 & landcover_ndvi_average < 0 ~ "ultra low",
    landcover_ndvi_average >= 0 & landcover_ndvi_average < 0.1 ~ "rather low",
    landcover_ndvi_average >= 0.1 & landcover_ndvi_average < 0.2 ~ "quite low",
    landcover_ndvi_average >= 0.2 & landcover_ndvi_average < 0.3 ~ "just low",
    landcover_ndvi_average >= 0.3 & landcover_ndvi_average < 0.4 ~ "not bad not great",
    landcover_ndvi_average >= 0.4 & landcover_ndvi_average < 0.5 ~ "just better",
    landcover_ndvi_average >= 0.5 & landcover_ndvi_average < 0.6 ~ "quite good",
    landcover_ndvi_average >= 0.6 & landcover_ndvi_average < 0.7 ~ "rather good",
    landcover_ndvi_average >= 0.7 & landcover_ndvi_average < 0.8 ~ "ultra good",
    landcover_ndvi_average >= 0.8 & landcover_ndvi_average < 0.9 ~ "awesome",
    TRUE                                                         ~ "unknown"
  )
  ) %>%
  dplyr::mutate(
    landlocked = dplyr::case_when(
      neighbors_percent_shared_edge < 99.9 ~ 0,
      neighbors_percent_shared_edge >= 99.9 ~ 1
    )
  )

#dominant dominant_landcover
temp_df <- ecoregions_tibble %>%
  dplyr::select(
    landcover_bare_percent_average,
    landcover_herbs_percent_average,
    landcover_trees_percent_average
  ) %>%
  dplyr::rename(
    bare = landcover_bare_percent_average,
    herbs = landcover_herbs_percent_average,
    trees = landcover_trees_percent_average
  )
ecoregions_tibble$dominant_landcover <- colnames(temp_df)[apply(temp_df,1,which.max)]
ecoregions_tibble$dominant_landcover <- factor(ecoregions_tibble$dominant_landcover)

usethis::use_data(ecoregions_tibble, overwrite = TRUE)




#plant_cover_character and landlocked_boolean
ecoregions_sf <- ecoregions_sf %>%
  dplyr::mutate(primary_productivity = dplyr::case_when(
    landcover_ndvi_average < -0.1                               ~ "abysmal",
    landcover_ndvi_average >= -0.1 & landcover_ndvi_average < 0 ~ "ultra low",
    landcover_ndvi_average >= 0 & landcover_ndvi_average < 0.1 ~ "rather low",
    landcover_ndvi_average >= 0.1 & landcover_ndvi_average < 0.2 ~ "quite low",
    landcover_ndvi_average >= 0.2 & landcover_ndvi_average < 0.3 ~ "just low",
    landcover_ndvi_average >= 0.3 & landcover_ndvi_average < 0.4 ~ "not bad not great",
    landcover_ndvi_average >= 0.4 & landcover_ndvi_average < 0.5 ~ "just better",
    landcover_ndvi_average >= 0.5 & landcover_ndvi_average < 0.6 ~ "quite good",
    landcover_ndvi_average >= 0.6 & landcover_ndvi_average < 0.7 ~ "rather good",
    landcover_ndvi_average >= 0.7 & landcover_ndvi_average < 0.8 ~ "ultra good",
    landcover_ndvi_average >= 0.8 & landcover_ndvi_average < 0.9 ~ "awesome",
    TRUE                                                         ~ "unknown"
  )
  ) %>%
  dplyr::mutate(
    landlocked = dplyr::case_when(
      neighbors_percent_shared_edge < 99.9 ~ 0,
      neighbors_percent_shared_edge >= 99.9 ~ 1
    )
  )

#dominant dominant_landcover
temp_df <- ecoregions_sf %>%
  dplyr::select(
    landcover_bare_percent_average,
    landcover_herbs_percent_average,
    landcover_trees_percent_average
  ) %>%
  dplyr::rename(
    bare = landcover_bare_percent_average,
    herbs = landcover_herbs_percent_average,
    trees = landcover_trees_percent_average
  )
ecoregions_sf$dominant_landcover <- colnames(temp_df)[apply(temp_df,1,which.max)]
ecoregions_sf$dominant_landcover <- factor(ecoregions_sf$dominant_landcover)

usethis::use_data(ecoregions_sf, overwrite = TRUE)

ecoregions_all_predictors[44] <- "primary_productivity"
ecoregions_all_predictors[45] <- "landlocked"
ecoregions_all_predictors[46] <- "dominant_landcover"
