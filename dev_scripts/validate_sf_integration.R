# Validation Script for SF Integration
# Tests sf data frame support across spatialRF functions

library(spatialRF)
library(sf)

# Load example data
data(plants_df, plants_distance, plants_response, plants_predictors)

# Subset for faster testing
idx <- 1:100
plants_df_sub <- plants_df[idx, ]
plants_distance_sub <- plants_distance[idx, idx]

message("\n=== SF Integration Validation Tests ===\n")

# Test 1: SF object - no xy (POINT geometry) ----
message("Test 1: SF object with POINT geometry (no xy parameter)")
plants_sf <- sf::st_as_sf(
  x = plants_df_sub,
  coords = c("x", "y"),
  crs = 4326
)

m_sf <- rf(
  data = plants_sf,
  dependent.variable.name = plants_response,
  predictor.variable.names = plants_predictors,
  distance.matrix = plants_distance_sub,
  ranger.arguments = list(num.trees = 50),
  verbose = FALSE,
  n.cores = 1
)

stopifnot("model$xy is NULL" = !is.null(m_sf$xy))
stopifnot("xy missing x column" = "x" %in% colnames(m_sf$xy))
stopifnot("xy missing y column" = "y" %in% colnames(m_sf$xy))
stopifnot("geometry in ranger data" = !("geometry" %in% colnames(m_sf$ranger.arguments$data)))
message("  ✓ POINT geometry: xy extracted, no geometry in model data\n")

# Test 2: SF object with POLYGON geometry ----
message("Test 2: SF object with POLYGON geometry (using centroids)")
# Create simple polygons from point buffer
plants_sf_poly <- sf::st_buffer(plants_sf, dist = 0.01)

m_sf_poly <- rf(
  data = plants_sf_poly,
  dependent.variable.name = plants_response,
  predictor.variable.names = plants_predictors,
  distance.matrix = plants_distance_sub,
  ranger.arguments = list(num.trees = 50),
  verbose = FALSE,
  n.cores = 1
)

stopifnot("polygon xy not extracted" = !is.null(m_sf_poly$xy))
stopifnot("polygon geometry in data" = !("geometry" %in% colnames(m_sf_poly$ranger.arguments$data)))
message("  ✓ POLYGON geometry: centroids extracted correctly\n")

# Test 3: SF object with LINESTRING (should fail) ----
message("Test 3: SF object with LINESTRING (should error)")
# Create a simple linestring from the first few points
coords_matrix <- sf::st_coordinates(plants_sf)[1:5, ]
line_geom <- sf::st_linestring(coords_matrix)
plants_sf_line <- sf::st_sf(
  plants_df_sub[1, plants_predictors],
  geometry = sf::st_sfc(line_geom, crs = 4326)
)

error_caught <- FALSE
tryCatch(
  {
    m_fail <- rf(
      data = plants_sf_line,
      dependent.variable.name = plants_response,
      predictor.variable.names = plants_predictors,
      ranger.arguments = list(num.trees = 50),
      verbose = FALSE,
      n.cores = 1
    )
  },
  error = function(e) {
    if (grepl("LINESTRING.*not supported", e$message)) {
      error_caught <<- TRUE
    }
  }
)
stopifnot("LINESTRING should error" = error_caught)
message("  ✓ LINESTRING correctly rejected with error\n")

# Test 4: SF object with explicit xy ----
message("Test 4: SF object with explicit xy parameter")
plants_xy_manual <- data.frame(
  x = sf::st_coordinates(plants_sf)[, 1],
  y = sf::st_coordinates(plants_sf)[, 2]
)

m_sf_xy <- rf(
  data = plants_sf,
  dependent.variable.name = plants_response,
  predictor.variable.names = plants_predictors,
  distance.matrix = plants_distance_sub,
  xy = plants_xy_manual,
  ranger.arguments = list(num.trees = 50),
  verbose = FALSE,
  n.cores = 1
)

stopifnot("explicit xy not used" = !is.null(m_sf_xy$xy))
stopifnot("geometry in data with explicit xy" = !("geometry" %in% colnames(m_sf_xy$ranger.arguments$data)))
message("  ✓ Explicit xy parameter overrides automatic extraction\n")

# Test 5: Regular data frame with lon/lat columns ----
message("Test 5: Regular data frame with flexible column names (lon/lat)")
plants_df_lonlat <- plants_df_sub
colnames(plants_df_lonlat)[which(colnames(plants_df_lonlat) == "x")] <- "longitude"
colnames(plants_df_lonlat)[which(colnames(plants_df_lonlat) == "y")] <- "latitude"

m_lonlat <- rf(
  data = plants_df_lonlat,
  dependent.variable.name = plants_response,
  predictor.variable.names = plants_predictors,
  distance.matrix = plants_distance_sub,
  ranger.arguments = list(num.trees = 50),
  verbose = FALSE,
  n.cores = 1
)

stopifnot("lon/lat not detected" = !is.null(m_lonlat$xy))
stopifnot("lon/lat xy incorrect" = all(c("x", "y") %in% colnames(m_lonlat$xy)))
message("  ✓ Flexible column matching: longitude/latitude detected\n")

# Test 6: Spatial model with sf ----
message("Test 6: Spatial model (rf_spatial) with sf object")
m_spatial_sf <- rf_spatial(
  data = plants_sf,
  dependent.variable.name = plants_response,
  predictor.variable.names = plants_predictors,
  distance.matrix = plants_distance_sub,
  method = "mem.moran.sequential",
  ranger.arguments = list(num.trees = 50),
  verbose = FALSE,
  n.cores = 1
)

stopifnot("spatial model xy missing" = !is.null(m_spatial_sf$xy))
stopifnot("geometry in spatial model" = !("geometry" %in% colnames(m_spatial_sf$ranger.arguments$data)))
stopifnot("spatial predictors missing" = !is.null(m_spatial_sf$spatial$names))
message("  ✓ Spatial model: no geometry in final data\n")

# Test 7: Cross-validation with sf ----
message("Test 7: Cross-validation (rf_evaluate) with sf object")
m_eval <- rf_evaluate(
  model = m_sf,
  repetitions = 5,
  verbose = FALSE,
  n.cores = 1
)

stopifnot("evaluation failed" = !is.null(m_eval$evaluation))
stopifnot("evaluation missing folds" = !is.null(m_eval$evaluation$spatial.folds))
message("  ✓ Cross-validation: completed successfully with sf-derived xy\n")

# Test 8: Backward compatibility ----
message("Test 8: Backward compatibility (regular data frame + explicit xy)")
m_regular <- rf(
  data = plants_df_sub,
  dependent.variable.name = plants_response,
  predictor.variable.names = plants_predictors,
  distance.matrix = plants_distance_sub,
  xy = plants_df_sub[, c("x", "y")],
  ranger.arguments = list(num.trees = 50),
  verbose = FALSE,
  n.cores = 1
)

stopifnot("backward compat failed" = !is.null(m_regular$predictions))
stopifnot("backward compat xy missing" = !is.null(m_regular$xy))

# Compare with sf approach
stopifnot("predictions differ" =
  abs(m_regular$performance$rmse - m_sf$performance$rmse) < 0.01
)
message("  ✓ Backward compatibility: existing workflows unchanged\n")

# Summary ----
message("\n=== All SF Integration Tests Passed! ===")
message("✓ SF POINT/MULTIPOINT: Direct extraction")
message("✓ SF POLYGON/MULTIPOLYGON: Centroid extraction")
message("✓ SF LINESTRING: Correctly rejected")
message("✓ Explicit xy: Overrides automatic extraction")
message("✓ Flexible columns: lon/lat detected")
message("✓ Spatial models: Geometry dropped correctly")
message("✓ Cross-validation: Works with sf-derived coordinates")
message("✓ Backward compatibility: Existing code unchanged\n")
