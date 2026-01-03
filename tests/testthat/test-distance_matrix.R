# Test distance_matrix function

# Helper function to create test data
create_test_coords <- function(n = 5, type = "geographic") {
  if (type == "geographic") {
    data.frame(
      longitude = seq(-122, -118, length.out = n),
      latitude = seq(37, 41, length.out = n)
    )
  } else {
    # For projected coordinates, use appropriate CRS (UTM)
    data.frame(
      x = seq(500000, 510000, length.out = n),
      y = seq(4000000, 4010000, length.out = n)
    )
  }
}

# =============================================================================
# Test 1: Input validation
# =============================================================================

test_that("distance_matrix rejects NULL data", {
  expect_error(
    distance_matrix(data = NULL),
    "Argument 'data' is required"
  )
})

test_that("distance_matrix rejects invalid input types", {
  expect_error(
    distance_matrix(data = "invalid"),
    "Argument 'data' must be of class 'data.frame'"
  )
  expect_error(
    distance_matrix(data = c(1, 2, 3)),
    "Argument 'data' must be of class 'data.frame'"
  )
})

test_that("distance_matrix rejects data frames with missing coordinate columns", {
  df <- data.frame(
    a = 1:5,
    b = 6:10
  )
  expect_error(
    distance_matrix(data = df),
    "Argument 'data' must have one of these column names"
  )
})

# =============================================================================
# Test 2: Column name detection - geographic coordinates
# =============================================================================

test_that("distance_matrix detects lon/lat columns", {
  df <- data.frame(
    lon = c(-122, -73, -87),
    lat = c(37, 40, 41)
  )
  dm <- distance_matrix(data = df, verbose = FALSE)
  expect_true(is.matrix(dm))
  expect_equal(nrow(dm), 3)
  expect_equal(ncol(dm), 3)
})

test_that("distance_matrix detects long/latitude columns", {
  df <- data.frame(
    long = c(-122, -73, -87),
    latitude = c(37, 40, 41)
  )
  dm <- distance_matrix(data = df, verbose = FALSE)
  expect_true(is.matrix(dm))
  expect_equal(nrow(dm), 3)
})

test_that("distance_matrix detects longitude/latitude columns", {
  df <- data.frame(
    longitude = c(-122, -73, -87),
    latitude = c(37, 40, 41)
  )
  dm <- distance_matrix(data = df, verbose = FALSE)
  expect_true(is.matrix(dm))
  expect_equal(nrow(dm), 3)
})

test_that("distance_matrix detects longitud/latitud columns (Spanish)", {
  df <- data.frame(
    longitud = c(-122, -73, -87),
    latitud = c(37, 40, 41)
  )
  dm <- distance_matrix(data = df, verbose = FALSE)
  expect_true(is.matrix(dm))
  expect_equal(nrow(dm), 3)
})

test_that("distance_matrix detects x/y columns", {
  df <- data.frame(
    x = c(-122, -73, -87),
    y = c(37, 40, 41)
  )
  dm <- distance_matrix(data = df, verbose = FALSE)
  expect_true(is.matrix(dm))
  expect_equal(nrow(dm), 3)
  expect_true(all(dm >= 0))
})

test_that("distance_matrix detects x with latitude as geographic", {
  df <- data.frame(
    x = c(-122, -73, -87),
    latitude = c(37, 40, 41)
  )
  dm <- distance_matrix(data = df, verbose = FALSE)
  expect_true(is.matrix(dm))
  expect_equal(nrow(dm), 3)
  # Should produce reasonable geographic distances (meters)
  expect_true(max(dm) > 1000000)  # > 1000 km
})

test_that("distance_matrix handles case variations (X/Y)", {
  df <- data.frame(
    X = c(-122, -73, -87),
    Y = c(37, 40, 41)
  )
  dm <- distance_matrix(data = df, verbose = FALSE)
  expect_true(is.matrix(dm))
  expect_equal(nrow(dm), 3)
})

test_that("distance_matrix handles mixed case columns", {
  df <- data.frame(
    Longitude = c(-122, -73, -87),
    Latitude = c(37, 40, 41)
  )
  dm <- distance_matrix(data = df, verbose = FALSE)
  expect_true(is.matrix(dm))
  expect_equal(nrow(dm), 3)
})

# =============================================================================
# Test 3: Projected coordinates with appropriate CRS
# =============================================================================

test_that("distance_matrix handles projected coordinates with UTM CRS", {
  skip_if_not_installed("sf")

  # Use coordinates that are clearly projected (UTM)
  df <- data.frame(
    x = c(500000, 510000, 520000),
    y = c(4000000, 4010000, 4020000)
  )
  # Use appropriate UTM CRS (EPSG:32610 - UTM Zone 10N)
  dm <- distance_matrix(data = df, crs = 32610, verbose = FALSE)
  expect_true(is.matrix(dm))
  expect_equal(nrow(dm), 3)
  # Distance should be reasonable (around 14,000 m between adjacent points)
  expect_true(dm[1, 2] > 10000 && dm[1, 2] < 20000)
})

# =============================================================================
# Test 4: CRS parameter
# =============================================================================

test_that("distance_matrix respects custom CRS for geographic coordinates", {
  df <- data.frame(
    longitude = c(-122, -73, -87),
    latitude = c(37, 40, 41)
  )

  # Default (4326)
  dm_default <- distance_matrix(data = df, verbose = FALSE)

  # Explicit 4326
  dm_explicit <- distance_matrix(data = df, crs = 4326, verbose = FALSE)

  # Should be identical
  expect_equal(dm_default, dm_explicit)
})

test_that("distance_matrix uses specified CRS for projected coordinates", {
  skip_if_not_installed("sf")

  # UTM coordinates
  df <- data.frame(
    x = c(500000, 510000, 520000),
    y = c(4000000, 4010000, 4020000)
  )

  # With explicit UTM CRS (EPSG:32610 - UTM Zone 10N)
  dm_utm <- distance_matrix(data = df, crs = 32610, verbose = FALSE)

  expect_true(is.matrix(dm_utm))
  expect_equal(nrow(dm_utm), 3)

  # Distance should be reasonable (around 14,000 m between adjacent points)
  expect_true(dm_utm[1, 2] > 10000 && dm_utm[1, 2] < 20000)
})

test_that("distance_matrix crs parameter ignored for sf objects", {
  skip_if_not_installed("sf")

  df <- data.frame(
    longitude = c(-122, -73, -87),
    latitude = c(37, 40, 41)
  )
  pts <- sf::st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326)

  # CRS parameter should be ignored for sf objects
  dm1 <- distance_matrix(data = pts, verbose = FALSE)
  dm2 <- distance_matrix(data = pts, crs = 32610, verbose = FALSE)  # Different CRS ignored

  expect_equal(dm1, dm2)
})

# =============================================================================
# Test 5: sf objects
# =============================================================================

test_that("distance_matrix works with sf POINT objects", {
  skip_if_not_installed("sf")

  df <- data.frame(
    longitude = c(-122, -73, -87),
    latitude = c(37, 40, 41)
  )
  pts <- sf::st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326)

  dm <- distance_matrix(data = pts, verbose = FALSE)
  expect_true(is.matrix(dm))
  expect_equal(nrow(dm), 3)
  expect_equal(ncol(dm), 3)
})

test_that("distance_matrix works with sf POLYGON objects", {
  skip_if_not_installed("sf")

  df <- data.frame(
    longitude = c(-122, -73, -87),
    latitude = c(37, 40, 41)
  )
  pts <- sf::st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326)
  polys <- sf::st_buffer(pts, dist = 10000)

  dm <- distance_matrix(data = polys, verbose = FALSE)
  expect_true(is.matrix(dm))
  expect_equal(nrow(dm), 3)
})

test_that("distance_matrix works with sf objects without CRS", {
  skip_if_not_installed("sf")

  df <- data.frame(
    x = c(1000, 2000, 3000),
    y = c(5000, 6000, 7000)
  )
  pts <- sf::st_as_sf(df, coords = c("x", "y"), crs = NA)

  # Function should work without warning (no CRS validation in current implementation)
  dm <- distance_matrix(data = pts, verbose = FALSE)
  expect_true(is.matrix(dm))
  expect_equal(nrow(dm), 3)
})

test_that("distance_matrix works with MULTIPOINT geometries", {
  skip_if_not_installed("sf")

  # Create MULTIPOINT geometry (one feature with multiple points)
  mp <- sf::st_multipoint(matrix(c(-122, 37, -122.1, 37.1), ncol = 2, byrow = TRUE))
  mp_sf <- sf::st_sf(id = 1, geometry = sf::st_sfc(mp, crs = 4326))

  # Function should work (no geometry type validation in current implementation)
  dm <- distance_matrix(data = mp_sf, verbose = FALSE)
  expect_true(is.matrix(dm))
  expect_equal(nrow(dm), 1)  # One feature
})

test_that("distance_matrix works with LINESTRING geometries", {
  skip_if_not_installed("sf")

  # Create LINESTRING geometry
  line <- sf::st_linestring(matrix(c(-122, 37, -122.1, 37.1), ncol = 2, byrow = TRUE))
  line_sf <- sf::st_sf(id = 1, geometry = sf::st_sfc(line, crs = 4326))

  # Function should work (no geometry type validation in current implementation)
  dm <- distance_matrix(data = line_sf, verbose = FALSE)
  expect_true(is.matrix(dm))
  expect_equal(nrow(dm), 1)  # One feature
})

# =============================================================================
# Test 6: Output validation
# =============================================================================

test_that("distance_matrix returns correct dimensions", {
  df <- create_test_coords(n = 10)
  dm <- distance_matrix(data = df, verbose = FALSE)

  expect_equal(nrow(dm), 10)
  expect_equal(ncol(dm), 10)
})

test_that("distance_matrix diagonal is zero", {
  df <- create_test_coords(n = 5)
  dm <- distance_matrix(data = df, verbose = FALSE)

  expect_true(all(diag(dm) == 0))
})

test_that("distance_matrix is symmetric", {
  df <- create_test_coords(n = 5)
  dm <- distance_matrix(data = df, verbose = FALSE)

  expect_equal(dm, t(dm))
})

test_that("distance_matrix contains no negative values", {
  df <- create_test_coords(n = 5)
  dm <- distance_matrix(data = df, verbose = FALSE)

  expect_true(all(dm >= 0))
})

test_that("distance_matrix contains no NA values", {
  df <- create_test_coords(n = 5)
  dm <- distance_matrix(data = df, verbose = FALSE)

  expect_true(!any(is.na(dm)))
})

test_that("distance_matrix row and column names are character indices", {
  df <- create_test_coords(n = 5)
  dm <- distance_matrix(data = df, verbose = FALSE)

  expect_equal(rownames(dm), c("1", "2", "3", "4", "5"))
  expect_equal(colnames(dm), c("1", "2", "3", "4", "5"))
})

test_that("distance_matrix returns distances in meters", {
  # Known distance: SF to NYC is approximately 4,100 km = 4,100,000 m
  df <- data.frame(
    longitude = c(-122.4, -73.9),
    latitude = c(37.8, 40.7)
  )
  dm <- distance_matrix(data = df, verbose = FALSE)

  # Check that the distance is in reasonable range for meters
  expect_true(dm[1, 2] > 4000000)
  expect_true(dm[1, 2] < 4200000)
})

test_that("distance_matrix off-diagonal values are positive", {
  df <- create_test_coords(n = 5)
  dm <- distance_matrix(data = df, verbose = FALSE)

  # Get off-diagonal elements
  off_diag <- dm[upper.tri(dm)]
  expect_true(all(off_diag > 0))
})

test_that("distance_matrix has units attribute", {
  df <- create_test_coords(n = 3)
  dm <- distance_matrix(data = df, verbose = FALSE)

  # Check that units attribute exists
  expect_true(!is.null(attr(dm, "units")))
  expect_equal(attr(dm, "units"), "m")
})

# =============================================================================
# Test 7: Verbose output
# =============================================================================

test_that("distance_matrix verbose = TRUE produces message", {
  df <- create_test_coords(n = 3)

  expect_message(
    distance_matrix(data = df, verbose = TRUE),
    "Distance matrix:"
  )
})

test_that("distance_matrix verbose = FALSE produces no message", {
  df <- create_test_coords(n = 3)

  expect_silent(
    distance_matrix(data = df, verbose = FALSE)
  )
})

test_that("distance_matrix verbose message includes distance range", {
  df <- create_test_coords(n = 3)

  expect_message(
    distance_matrix(data = df, verbose = TRUE),
    "range"
  )
})

# =============================================================================
# Test 8: Consistency with plants_df example dataset
# =============================================================================

test_that("distance_matrix works with plants_df", {
  skip_if_not_installed("spatialRF")

  # Load the data
  data(plants_df, package = "spatialRF")

  # plants_df has x and y columns (geographic coordinates)
  dm <- distance_matrix(data = plants_df, verbose = FALSE)

  expect_true(is.matrix(dm))
  expect_equal(nrow(dm), nrow(plants_df))
  expect_equal(ncol(dm), nrow(plants_df))
  expect_true(all(diag(dm) == 0))
  expect_true(all(dm >= 0))
})

# =============================================================================
# Test 9: Edge cases
# =============================================================================

test_that("distance_matrix handles exactly 2 observations", {
  df <- data.frame(
    longitude = c(-122, -73),
    latitude = c(37, 40)
  )
  dm <- distance_matrix(data = df, verbose = FALSE)

  expect_equal(nrow(dm), 2)
  expect_equal(ncol(dm), 2)
  expect_equal(dm[1, 1], 0)
  expect_equal(dm[2, 2], 0)
  expect_true(dm[1, 2] > 0)
  expect_equal(dm[1, 2], dm[2, 1])
})

test_that("distance_matrix handles single observation", {
  df <- data.frame(
    longitude = -122,
    latitude = 37
  )
  # Function should work (no minimum observation validation)
  dm <- distance_matrix(data = df, verbose = FALSE)

  expect_equal(nrow(dm), 1)
  expect_equal(ncol(dm), 1)
  expect_equal(dm[1, 1], 0)
})

test_that("distance_matrix handles large datasets efficiently", {
  skip_on_cran()

  df <- create_test_coords(n = 1000)

  # Should complete without error
  dm <- distance_matrix(data = df, verbose = FALSE)

  expect_equal(nrow(dm), 1000)
  expect_equal(ncol(dm), 1000)
})

# =============================================================================
# Test 10: Comparison with example results
# =============================================================================

test_that("distance_matrix produces consistent results", {
  # Use fixed coordinates to ensure reproducibility
  df <- data.frame(
    longitude = c(-122.4, -73.9, -87.6),
    latitude = c(37.8, 40.7, 41.9)
  )

  dm1 <- distance_matrix(data = df, verbose = FALSE)
  dm2 <- distance_matrix(data = df, verbose = FALSE)

  # Results should be identical
  expect_equal(dm1, dm2)
})

test_that("distance_matrix with different column orders produces same results", {
  df1 <- data.frame(
    longitude = c(-122, -73, -87),
    latitude = c(37, 40, 41)
  )

  df2 <- data.frame(
    latitude = c(37, 40, 41),
    longitude = c(-122, -73, -87)
  )

  dm1 <- distance_matrix(data = df1, verbose = FALSE)
  dm2 <- distance_matrix(data = df2, verbose = FALSE)

  expect_equal(dm1, dm2)
})
