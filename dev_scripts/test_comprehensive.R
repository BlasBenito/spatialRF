devtools::load_all()
library(sf)

data(plants_df, plants_distance, plants_response, plants_predictors)

# Subset for faster testing
idx <- 1:100
plants_df_sub <- plants_df[idx, ]
plants_distance_sub <- plants_distance[idx, idx]

message("\n=== Comprehensive SF Integration Tests ===\n")

# Test 1: Basic sf with POINT
message("Test 1: Basic SF with POINT geometry")
plants_sf <- st_as_sf(plants_df_sub, coords = c('x', 'y'), crs = 4326)
m1 <- rf(
  data = plants_sf,
  dependent.variable.name = plants_response,
  predictor.variable.names = plants_predictors,
  distance.matrix = plants_distance_sub,
  ranger.arguments = list(num.trees = 50),
  verbose = FALSE,
  n.cores = 1
)
stopifnot(!is.null(m1$xy))
stopifnot(!("geometry" %in% colnames(m1$ranger.arguments$data)))
message("  ✓ Test 1 passed\n")

# Test 2: Spatial model with sf
message("Test 2: Spatial model with sf")
m2 <- rf_spatial(
  data = plants_sf,
  dependent.variable.name = plants_response,
  predictor.variable.names = plants_predictors,
  distance.matrix = plants_distance_sub,
  method = "mem.moran.sequential",
  ranger.arguments = list(num.trees = 50),
  verbose = FALSE,
  n.cores = 1
)
stopifnot(!is.null(m2$spatial$names))
stopifnot(!("geometry" %in% colnames(m2$ranger.arguments$data)))
message("  ✓ Test 2 passed\n")

# Test 3: Flexible column names
message("Test 3: Flexible column names (lon/lat)")
plants_df_lonlat <- plants_df_sub
names(plants_df_lonlat)[names(plants_df_lonlat) == "x"] <- "longitude"
names(plants_df_lonlat)[names(plants_df_lonlat) == "y"] <- "latitude"
m3 <- rf(
  data = plants_df_lonlat,
  dependent.variable.name = plants_response,
  predictor.variable.names = plants_predictors,
  distance.matrix = plants_distance_sub,
  ranger.arguments = list(num.trees = 50),
  verbose = FALSE,
  n.cores = 1
)
stopifnot(!is.null(m3$xy))
message("  ✓ Test 3 passed\n")

# Test 4: Backward compatibility
message("Test 4: Backward compatibility (regular df + explicit xy)")
m4 <- rf(
  data = plants_df_sub,
  dependent.variable.name = plants_response,
  predictor.variable.names = plants_predictors,
  distance.matrix = plants_distance_sub,
  xy = plants_df_sub[, c("x", "y")],
  ranger.arguments = list(num.trees = 50),
  verbose = FALSE,
  n.cores = 1
)
stopifnot(!is.null(m4$xy))
message("  ✓ Test 4 passed\n")

# Test 5: Cross-validation
message("Test 5: Cross-validation with sf-derived xy")
m5 <- rf_evaluate(
  model = m1,
  repetitions = 5,
  verbose = FALSE,
  n.cores = 1
)
stopifnot(!is.null(m5$evaluation))
message("  ✓ Test 5 passed\n")

message("\n=== All Key Tests Passed! ===\n")
message("✓ SF POINT geometry extraction")
message("✓ Spatial models with sf")
message("✓ Flexible column detection")
message("✓ Backward compatibility")
message("✓ Cross-validation with sf")
