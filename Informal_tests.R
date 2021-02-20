#PLOTTING MAP
library(sf)
library(ggplot2)
library(patchwork)
library("rnaturalearth")
library("rnaturalearthdata")
data(plant_richness_sf)
data(plant_richness_df)
pr <- cbind(plant_richness_df, plant_richness_sf)

world <- ne_countries(scale = "medium", returnclass = "sf")

map <- ggplot() +
  geom_sf(data = world) +
  geom_sf(data = pr,
          aes(
            geometry = geom_centroids,
            fill = richness_species_vascular
          ),
          shape = 21,
          color = "black",
          size = 2.5
  ) +
  scale_fill_viridis_c(direction = -1) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.key.width=unit(1,"cm")
  ) +
  labs(fill = "Plant richness") +
  scale_x_continuous(limits = c(-170, -30)) +
  scale_y_continuous(limits = c(-58, 75))  +
  ggtitle("Plant richness of the American ecoregions")


#BASIC MODELS TO TEST OTHER THINGIES
#############################################
data(plant_richness_df)
data(distance_matrix)

#basic model
rf.model <- rf(
  data = plant_richness_df,
  dependent.variable.name = "richness_species_vascular",
  predictor.variable.names = colnames(plant_richness_df)[5:21],
  distance.matrix = distance_matrix,
  distance.thresholds = c(0, 1000, 2000),
  seed = 50,
  verbose = FALSE
)

#with repetitions
rf.repeat <- rf_repeat(model = rf.model, verbose = FALSE)

#spatial model
rf.spatial <- rf_spatial(model = rf.model, verbose = FALSE)

#from repeat
rf.spatial.repeat <- rf_spatial(model = rf.repeat, verbose = FALSE)

#trying rf_evaluate
rf.model <- rf_evaluate(
  model = rf.model,
  xy = plant_richness_df[, c("x", "y")],
  verbose = FALSE,
  metrics = "r.squared"
)
plot_evaluation(rf.model)
get_evaluation(rf.model)

rf.repeat <- rf_evaluate(
  model = rf.repeat,
  xy = plant_richness_df[, c("x", "y")]
)
plot_evaluation(rf.repeat)

rf.spatial <- rf_evaluate(
  model = rf.spatial,
  xy = plant_richness_df[, c("x", "y")]
)
plot_evaluation(rf.spatial)

rf.spatial.repeat <- rf_evaluate(
  model = rf.spatial.repeat,
  xy = plant_richness_df[, c("x", "y")]
)
plot_evaluation(rf.spatial.repeat)


#DETECTING INTERACTIONS
ranger.arguments <- NULL
#basic model
rf.interaction <- suggest_interactions(
  data = plant_richness_df,
  dependent.variable.name = "richness_species_vascular",
  predictor.variable.names = colnames(plant_richness_df)[5:21],
  verbose = TRUE
)

rf.interaction$selected

rf.interaction <- suggest_interactions(model = rf.model)
x <- rf.interaction$selected
rf.interaction$df


#RESPONSE SURFACES
p <- response_surfaces(
  model = rf.model
  )

p <- response_curves(
  model = rf.model
)

p <- response_surfaces(
  model = rf.repeat
)

p <- response_curves(
  model = rf.repeat
)

p <- response_surfaces(
  model = rf.spatial
)

p <- response_curves(
  model = rf.spatial
)


#rf_tuning
###############
data(plant_richness_df)
data(distance_matrix)

#oob method
tuning <- rf_tuning(
  data = plant_richness_df,
  dependent.variable.name = "richness_species_vascular",
  predictor.variable.names = colnames(plant_richness_df)[5:21],
  tuning.method = "oob",
  verbose = TRUE
)

#spatial.cv
tuning <- rf_tuning(
  data = plant_richness_df,
  dependent.variable.name = "richness_species_vascular",
  predictor.variable.names = colnames(plant_richness_df)[5:21],
  tuning.method = "spatial.cv",
  xy = plant_richness_df[, c("x", "y")],
  verbose = TRUE
)


#rf compare
comparison <- rf_compare2(
models <- list(
  rf.model = rf.model,
  rf.repeat = rf.repeat,
  rf.spatial = rf.spatial,
  rf.spatial.repeat = rf.spatial.repeat
),
xy = plant_richness_df[, c("x", "y")],
metrics = "r.squared"
)



#rf_repeat sequential
data(plant_richness_df)
data(distance_matrix)

#basic model
rf.model <- rf(
  data = plant_richness_df,
  dependent.variable.name = "richness_species_vascular",
  predictor.variable.names = colnames(plant_richness_df)[5:21],
  distance.matrix = distance_matrix,
  distance.thresholds = c(0, 1000, 2000),
  seed = 50,
  verbose = FALSE
)

#with repetitions
rf.repeat <- rf_repeat(model = rf.model, verbose = FALSE, disable.parallel = TRUE, repetitions = 100)

#interactions
interactions <- rf_interactions(
  data = plant_richness_df,
  dependent.variable.name = "richness_species_vascular",
  predictor.variable.names = colnames(plant_richness_df)[5:21],
  cluster.ips = c(
    "10.42.0.1",
    "10.42.0.34",
    "10.42.0.104"
  ),
  cluster.cores = c(7, 4, 4)
  )
