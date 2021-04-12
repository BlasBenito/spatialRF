library(spatialRF)
library(tmap)
library(sp)
library(spdep)

#data preparation
data(plant_richness_df)
data(distance_matrix)
x <- plant_richness_df
dependent.variable.name <- "richness_species_vascular"
predictor.variable.names <- colnames(plant_richness_df)[5:21]
distance.thresholds <- c(0, 1500, 3000)
distance.matrix <- distance_matrix
random.seed <- 100

#RANDOM FOREST
m.rf <- spatialRF::rf(
  data = x,
  dependent.variable.name = dependent.variable.name,
  predictor.variable.names = predictor.variable.names,
  distance.matrix = distance_matrix,
  distance.thresholds = distance.thresholds,
  seed = random.seed,
  verbose = FALSE
)
plot_moran(m.rf)

#LINEAR MODEL
formula <- as.formula(
  paste(
    dependent.variable.name,
    " ~ ",
    paste(predictor.variable.names, collapse = " + "))
)
m.lm <- lm(formula = formula, data = x)
moran_multithreshold(
  x = residuals(m.lm),
  distance.matrix = distance.matrix,
  distance.thresholds = distance.thresholds
)

#residuals using spdep
x.sp <- sp::SpatialPointsDataFrame(
  coords = plant_richness_df[, c("x", "y")],
  data = plant_richness_df,
  proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
)

#neighbors
x.sp.neighbors <- knn2nb(knearneigh(x.sp))
x.sp.neighbors.matrix <- nb2mat(x.sp.neighbors, style = "B")
x.sp.neighbors.matrix.weights <- mat2listw(x.sp.neighbors.matrix, style = "W")

#lm.morantest
lm.morantest(
  model = m.lm,
  listw = x.sp.neighbors.matrix.weights,
  alternative = "two.sided"
)

#moran.test
moran.test(
  x = residuals(m.lm),
  listw = x.sp.neighbors.matrix.weights,
  alternative = "two.sided"
)

my.morantest <- moran_multithreshold(
  x = residuals(m.lm),
  distance.matrix = distance.matrix,
  distance.thresholds = distance.thresholds
)
my.morantest
