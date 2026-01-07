devtools::load_all()
library(sf)

data(plants_df, plants_distance, plants_response, plants_predictors)

plants_sf <- st_as_sf(
  plants_df[1:50, ],
  coords = c('x', 'y'),
  crs = 4326
)

print("SF object created")
print(class(plants_sf))

m <- rf(
  data = plants_sf,
  dependent.variable.name = plants_response,
  predictor.variable.names = plants_predictors,
  distance.matrix = plants_distance[1:50, 1:50],
  ranger.arguments = list(num.trees = 10),
  verbose = FALSE,
  n.cores = 1
)

print("Model fitted successfully!")
print("Data columns:")
print(colnames(m$ranger.arguments$data))
print("xy extracted:")
print(head(m$xy))
