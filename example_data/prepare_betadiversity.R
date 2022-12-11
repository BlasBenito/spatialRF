library(tidyverse)
library(arrow)
library(arules)

#load
load("/home/blas/Dropbox/GITHUB/gbif_plantae/ecoregions_plant_diversity_ready.RData")

#data
ecoregions_betadiversity_df <- ecoregions_pairs$data

ecoregions_df <- ecoregions$data

ecoregions_betadiversity_df <- dplyr::inner_join(
  x = ecoregions_betadiversity_df,
  y = ecoregions_df[, c("ecoregion_name", "ecoregion_biome", "ecoregion_realm", "ecoregion_continent")],
  by = "ecoregion_name"
) %>%
  dplyr::relocate(
    ecoregion_biome,
    ecoregion_realm,
    ecoregion_continent,
    .after = neighbor_name
  )

ecoregions_betadiversity_df <- dplyr::inner_join(
  x = ecoregions_betadiversity_df,
  y = ecoregions_df %>%
    dplyr::select(
      ecoregion_name,
      ecoregion_biome,
      ecoregion_realm,
      ecoregion_continent
    ) %>%
    dplyr::rename(
      neighbor_biome = ecoregion_biome,
      neighbor_realm = ecoregion_realm,
      neighbor_continent = ecoregion_continent
    ),
  by = c("neighbor_name" = "ecoregion_name")
) %>%
  dplyr::relocate(
    neighbor_biome,
    neighbor_realm,
    neighbor_continent,
    .after = ecoregion_continent
  )

#other columns
other_columns <- c(
  "ecoregion_name",
  "neighbor_name"
)

#predictors
ecoregions_betadiversity_predictors <- colnames(ecoregions_betadiversity_df)[
  c(3:21,26:41,122:125)
]

#responses
ecoregions_betadiversity_responses <- c(
  "betadiversity_Bsor_species_vascular",
  "betadiversity_Bsor_genera_vascular",
  "betadiversity_Bsor_species_trees",
  "betadiversity_Bsor_genera_trees",
  "betadiversity_Bsor_species_grasses",
  "betadiversity_Bsor_genera_grasses"
)


#subset dataset
ecoregions_betadiversity_df <- ecoregions_betadiversity_df[, c(other_columns, ecoregions_betadiversity_responses,  ecoregions_betadiversity_predictors)]

#order
ecoregions_betadiversity_df <- dplyr::arrange(
  ecoregions_betadiversity_df,
  ecoregion_name, neighbor_name
)

#dictionary
ecoregions_ids <- data.frame(
  name = unique(ecoregions_betadiversity_df$ecoregion_name),
  id = seq_along(unique(ecoregions_betadiversity_df$ecoregion_name))
)

#replace names in ecoregions_betadiversity_df
ecoregions_betadiversity_df <- dplyr::inner_join(
  x = ecoregions_betadiversity_df,
  y = ecoregions_ids,
  by = c("ecoregion_name" = "name")
)

ecoregions_betadiversity_df <- ecoregions_betadiversity_df %>% dplyr::select(
  -ecoregion_name
) %>%
  dplyr::rename(
    ecoregion_id = id
  ) %>%
  dplyr::relocate(
    ecoregion_id,
    .before = neighbor_name
  )

ecoregions_betadiversity_df <- dplyr::inner_join(
  x = ecoregions_betadiversity_df,
  y = ecoregions_ids,
  by = c("neighbor_name" = "name")
)

ecoregions_betadiversity_df <- ecoregions_betadiversity_df %>% dplyr::select(
  -neighbor_name
) %>%
  dplyr::rename(
    neighbor_id = id
  ) %>%
  dplyr::relocate(
    neighbor_id,
    .after = ecoregion_id
  )

#replace values
ecoregions_betadiversity_df <- ecoregions_betadiversity_df %>%
  dplyr::mutate(
    ecoregion_area = as.integer(ecoregion_area),
    neighbor_area = as.integer(neighbor_area),
    ecoregion_topography_elevation_average = as.integer(ecoregion_topography_elevation_average),
    neighbor_topography_elevation_average = as.integer(neighbor_topography_elevation_average),
    connection_distance = as.integer(connection_distance),
    connection_are_neighbors = case_when(
      connection_are_neighbors == FALSE ~ 0,
      connection_are_neighbors == TRUE ~ 1
    ),
    connection_same_biome = case_when(
      connection_same_biome == "different biome" ~ 0,
      connection_same_biome == "same biome" ~ 1
    ),
    connection_same_realm = case_when(
      connection_same_realm == "different realm" ~ 0,
      connection_same_realm == "same realm" ~ 1
    ),
    connection_same_continent = case_when(
      connection_same_continent == "different continent" ~ 0,
      connection_same_continent == "same continent" ~ 1
    ),
    connection_shared_edge = as.integer(connection_shared_edge),
    connection_area_diff = as.integer(connection_area_diff),
    diff_climate_bio12_average = as.integer(diff_climate_bio12_average),
    diff_human_population = as.integer(diff_human_population),
    diff_topography_elevation = as.integer(diff_topography_elevation),
    environmental_overlap_frac_unique_ecoregion = as.integer(environmental_overlap_frac_unique_ecoregion * 10000),
    environmental_overlap_frac_unique_neighbor = as.integer(environmental_overlap_frac_unique_neighbor * 10000),
    environmental_overlap_jaccard = as.integer(environmental_overlap_jaccard * 10000),
    environmental_overlap_sorensen = as.integer(environmental_overlap_sorensen * 10000),
    diff_climate_bio1_average = as.integer(diff_climate_bio1_average),
    diff_climate_bio4_average = as.integer(diff_climate_bio4_average),
    diff_climate_bio5_average = as.integer(diff_climate_bio5_average),
    diff_climate_bio15_average = as.integer(diff_climate_bio15_average * 10),
    diff_human_footprint_average = as.integer(diff_human_footprint_average * 100),
    diff_human_population_density = as.integer(diff_human_population_density),
    diff_landcover_bare_percent_average = as.integer(diff_landcover_bare_percent_average * 100),
    diff_landcover_herbs_percent_average = as.integer(diff_landcover_herbs_percent_average * 100),
    diff_landcover_trees_percent_average = as.integer(diff_landcover_trees_percent_average * 100),
    diff_landcover_ndvi_min = as.integer(diff_landcover_ndvi_min * 100),
    diff_landcover_ndvi_max = as.integer(diff_landcover_ndvi_max * 100),
    ecoregion_climate_aridity_index_average = as.integer(ecoregion_climate_aridity_index_average * 10000),
    neighbor_climate_aridity_index_average = as.integer(neighbor_climate_aridity_index_average * 10000),
    diff_climate_aridity_index = as.integer(diff_climate_aridity_index * 10000),
    diff_climate_velocity_lgm_average = as.integer(diff_climate_velocity_lgm_average * 10),
    ecoregion_geo_latitude_average = round(ecoregion_geo_latitude_average, 2),
    neighbor_geo_latitude_average = round(neighbor_geo_latitude_average, 2),
    ecoregion_geo_longitude_average = round(ecoregion_geo_longitude_average, 2),
    neighbor_geo_longitude_average = round(neighbor_geo_longitude_average, 2),
    connection_latitude_diff = round(connection_latitude_diff, 2),
    connection_longitude_diff = round(connection_longitude_diff, 2)
  )

ecoregions_betadiversity_df <- ecoregions_betadiversity_df %>%
  dplyr::mutate(
    dplyr::across(
      dplyr::all_of(ecoregions_betadiversity_responses),
      ~as.integer(. * 10000)
      )
    )





#save to parquet
arrow::write_parquet(
  x = ecoregions_betadiversity_df,
  sink = "example_data/ecoregions_betadiversity_df.parquet",
  compression = "gzip", compression_level = 5
)

arrow::write_parquet(
  x = ecoregions_ids,
  sink = "example_data/ecoregions_ids.parquet"
)

save(
  ecoregions_betadiversity_predictors,
  ecoregions_betadiversity_responses,
  file = "example_data/ecoregions_betadiversity_variables.RData"
)



#replace a few values

#661
library(arrow)

