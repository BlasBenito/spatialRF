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

importance <- plot_importance(rf.spatial.repeat) +
  ggtitle("Variable importance via spatial regression with Random Forest") + theme_bw() + theme(legend.position = "none")

map | importance
