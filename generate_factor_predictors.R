library(dplyr)
library(magrittr)

data(
  ecoregions_df,
  ecoregions_tibble,
  ecoregions_sf
)

#plant_cover_character and landlocked_boolean
ecoregions_df <- ecoregions_df %>%
  dplyr::mutate(ndvi_character_ordered = dplyr::case_when(
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
    landlocked_binary = dplyr::case_when(
      neighbors_percent_shared_edge < 99.9 ~ 0,
      neighbors_percent_shared_edge >= 99.9 ~ 1
    )
  )

#dominant landcover
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
ecoregions_df$landcover_character_unordered <- colnames(temp_df)[apply(temp_df,1,which.max)]

usethis::use_data(ecoregions_df, overwrite = TRUE)




#plant_cover_character and landlocked_boolean
ecoregions_tibble <- ecoregions_tibble %>%
  dplyr::mutate(ndvi_character_ordered = dplyr::case_when(
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
    landlocked_binary = dplyr::case_when(
      neighbors_percent_shared_edge < 99.9 ~ 0,
      neighbors_percent_shared_edge >= 99.9 ~ 1
    )
  )

#dominant landcover
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
ecoregions_tibble$landcover_character_unordered <- colnames(temp_df)[apply(temp_df,1,which.max)]

usethis::use_data(ecoregions_tibble, overwrite = TRUE)




#plant_cover_character and landlocked_boolean
ecoregions_sf <- ecoregions_sf %>%
  dplyr::mutate(ndvi_character_ordered = dplyr::case_when(
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
    landlocked_binary = dplyr::case_when(
      neighbors_percent_shared_edge < 99.9 ~ 0,
      neighbors_percent_shared_edge >= 99.9 ~ 1
    )
  )

#dominant landcover
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
ecoregions_sf$landcover_character_unordered <- colnames(temp_df)[apply(temp_df,1,which.max)]

usethis::use_data(ecoregions_sf, overwrite = TRUE)
