#' @title Compute pairwise distance matrix from spatial coordinates
#' @description Computes a square distance matrix from point or polygon geometries using [sf::st_distance()]. Supports multiple input formats including data frames, coordinate vectors, and sf objects. Automatically handles both geographic (lat/lon) and projected coordinate systems.
#' @param x Numeric vector of x-coordinates/longitudes, OR data frame with coordinate columns (x,y or lon/long/longitude, lat/latitude), OR sf object. Default: `NULL`.
#' @param y Numeric vector of y-coordinates/latitudes. Required if `x` is a numeric vector. Default: `NULL`.
#' @param units Character. Output distance units: `"km"` (kilometers) or `"m"` (meters). Default: `"km"`.
#' @param verbose Logical. If `TRUE`, prints information about detected coordinate type and geometry. Default: `TRUE`.
#' @return Numeric matrix (n×n) with pairwise distances in specified units. Diagonal is 0. Row and column names are character indices ("1", "2", "3", ...).
#' @details
#' ## Implementation
#'
#' This function uses [sf::st_distance()] for all distance calculations, which:
#' \itemize{
#'   \item Handles both point and polygon geometries
#'   \item Automatically uses geodesic (great circle) distances for geographic coordinates
#'   \item Uses Euclidean distances for projected coordinates
#'   \item Computes edge-to-edge distances for polygons
#'   \item Returns accurate distances with proper unit handling
#' }
#'
#' ## Coordinate Detection
#'
#' **For sf objects:** Uses existing CRS information
#'
#' **For data frames:** Detects coordinate columns by name (case-insensitive):
#' \itemize{
#'   \item Geographic: lon/long/longitude + lat/latitude → assumes EPSG:4326 (WGS84)
#'   \item Projected: x + y → assumes coordinates in meters, uses Euclidean distance
#' }
#'
#' **For numeric vectors:** Checks if values fall within lat/lon ranges (-90 to 90, -180 to 180) to infer coordinate type
#'
#' ## Computational Complexity
#'
#' Distance matrix computation requires O(n²) pairwise distance calculations for n observations. Memory footprint is 8n² bytes (approximately 800 MB for n=10,000). For large datasets (n > 5,000), computation may take several minutes.
#'
#' ## Dependencies
#'
#' This function requires the sf package. Install with: `install.packages("sf")`
#'
#' @examples
#' \donttest{
#' # Example 1: Data frame with lon/lat (geographic)
#' coords_df <- data.frame(
#'   longitude = c(-122.4, -73.9, -87.6),
#'   latitude = c(37.8, 40.7, 41.9),
#'   city = c("SF", "NYC", "Chicago")
#' )
#' dm1 <- distance_matrix(coords_df, units = "km")
#' dm1
#'
#' # Example 2: Separate x, y vectors (projected coordinates in meters)
#' x <- c(500000, 510000, 520000)
#' y <- c(4000000, 4010000, 4020000)
#' dm2 <- distance_matrix(x, y, units = "km")
#' dm2
#'
#' # Example 3: Data frame with x, y columns
#' proj_coords <- data.frame(
#'   x = c(1000, 2000, 3000),
#'   y = c(5000, 6000, 7000)
#' )
#' dm3 <- distance_matrix(proj_coords, units = "m")
#' dm3
#' }
#'
#' \dontrun{
#' # Example 4: sf object with POINT geometry (requires sf package)
#' library(sf)
#' pts <- st_as_sf(coords_df, coords = c("longitude", "latitude"), crs = 4326)
#' dm4 <- distance_matrix(pts, units = "km")
#' dm4
#'
#' # Example 5: sf object with POLYGON geometry (edge-to-edge)
#' polys <- st_buffer(pts, dist = 10000) # 10 km buffers
#' dm5 <- distance_matrix(polys, units = "km")
#' dm5
#' }
#'
#' @family preprocessing
#' @rdname distance_matrix
#' @export
#' @autoglobal
distance_matrix <- function(
  x = NULL,
  y = NULL,
  units = "km",
  verbose = TRUE
) {
  # Validation: x not NULL
  if (is.null(x)) {
    stop("Argument 'x' is required.")
  }

  # Validation: units
  if (!units %in% c("km", "m")) {
    stop("Argument 'units' must be 'km' or 'm'.")
  }

  # Check sf availability
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' is required. Install with: install.packages('sf')")
  }

  # Initialize variables
  sf_object <- NULL
  is_geographic <- FALSE
  geometry_type <- "point"
  n <- NULL

  # ========================================
  # STEP 1: CONVERT INPUT TO SF OBJECT
  # ========================================

  # Case 1: Already an sf object
  if (inherits(x, "sf")) {
    sf_object <- x
    n <- nrow(sf_object)

    # Get CRS info
    crs_info <- sf::st_crs(sf_object)
    if (!is.na(crs_info$input)) {
      is_geographic <- sf::st_is_longlat(sf_object)
    } else {
      warning("sf object has no CRS. Assuming projected coordinates.")
      is_geographic <- FALSE
    }

    # Detect geometry type
    geom_types <- unique(as.character(sf::st_geometry_type(sf_object)))
    if (all(geom_types %in% c("POINT", "MULTIPOINT"))) {
      geometry_type <- "point"
    } else if (all(geom_types %in% c("POLYGON", "MULTIPOLYGON"))) {
      geometry_type <- "polygon"
    } else {
      geometry_type <- paste(geom_types, collapse = ", ")
    }
  } else if (is.data.frame(x)) {
    # Case 2: Data frame with coordinate columns
    n <- nrow(x)

    # Check for minimum observations
    if (n < 2) {
      stop("At least 2 observations required to compute distance matrix.")
    }

    # Detect column names (case-insensitive)
    col_names <- tolower(colnames(x))

    # Check for geographic indicators
    has_lon <- any(col_names %in% c("lon", "long", "longitude"))
    has_lat <- any(col_names %in% c("lat", "latitude"))
    has_xy <- all(c("x", "y") %in% colnames(x))

    if (has_lon && has_lat) {
      # Geographic coordinates
      lon_col <- colnames(x)[col_names %in% c("lon", "long", "longitude")][1]
      lat_col <- colnames(x)[col_names %in% c("lat", "latitude")][1]

      # Create sf object with geographic CRS (WGS84)
      sf_object <- sf::st_as_sf(
        x,
        coords = c(lon_col, lat_col),
        crs = 4326
      )
      is_geographic <- TRUE
    } else if (has_xy) {
      # Projected coordinates - assume meters, no CRS
      sf_object <- sf::st_as_sf(
        x,
        coords = c("x", "y"),
        crs = NA
      )
      is_geographic <- FALSE
    } else {
      stop(
        "Data frame must contain columns: (x, y) OR (lon/long/longitude, lat/latitude)."
      )
    }

    geometry_type <- "point"
  } else if (is.numeric(x)) {
    # Case 3: Separate x, y vectors
    # Check y is provided
    if (is.null(y)) {
      stop("Argument 'y' is required when 'x' is a numeric vector.")
    }

    # Check lengths match
    if (length(x) != length(y)) {
      stop("Vectors 'x' and 'y' must have the same length.")
    }

    n <- length(x)

    # Check for minimum observations
    if (n < 2) {
      stop("At least 2 observations required to compute distance matrix.")
    }

    # Check for NAs
    if (any(is.na(x)) || any(is.na(y))) {
      stop(
        "Coordinate vectors contain missing values (NA). Remove or impute NAs before computing distance matrix."
      )
    }

    # Detect if geographic based on value ranges
    distance_matrix_is_geographic <- function(coords) {
      # Check if values fall within lat/lon ranges
      # lon: -180 to 180, lat: -90 to 90
      x_range <- range(coords[, 1], na.rm = TRUE)
      y_range <- range(coords[, 2], na.rm = TRUE)

      # Geographic if both within valid lat/lon ranges
      x_in_range <- x_range[1] >= -180 && x_range[2] <= 180
      y_in_range <- y_range[1] >= -90 && y_range[2] <= 90

      return(x_in_range && y_in_range)
    }

    is_geographic <- distance_matrix_is_geographic(cbind(x, y))

    # Create sf object
    coords_df <- data.frame(x = x, y = y)
    if (is_geographic) {
      # Assume WGS84 for geographic coordinates
      sf_object <- sf::st_as_sf(
        coords_df,
        coords = c("x", "y"),
        crs = 4326
      )
    } else {
      # No CRS for projected
      sf_object <- sf::st_as_sf(
        coords_df,
        coords = c("x", "y"),
        crs = NA
      )
    }

    geometry_type <- "point"
  } else {
    # Case 4: Invalid input
    stop(
      "Argument 'x' must be: numeric vector (with y), data frame with coordinates, or sf object."
    )
  }

  # ========================================
  # STEP 2: COMPUTE DISTANCES WITH SF
  # ========================================

  # Use sf::st_distance for all calculations
  dist_matrix <- sf::st_distance(sf_object)

  # Convert to requested units
  if (units == "km") {
    dist_matrix <- units::set_units(dist_matrix, "km")
  } else if (units == "m") {
    dist_matrix <- units::set_units(dist_matrix, "m")
  }

  # Drop units and convert to numeric matrix
  dist_matrix <- units::drop_units(dist_matrix)
  dist_matrix <- matrix(as.numeric(dist_matrix), nrow = n, ncol = n)

  # ========================================
  # STEP 3: OUTPUT FORMATTING
  # ========================================

  # Set row/column names as character indices
  rownames(dist_matrix) <- as.character(1:n)
  colnames(dist_matrix) <- as.character(1:n)

  # Ensure diagonal is exactly 0 (handle floating point errors)
  diag(dist_matrix) <- 0

  # Validate output
  if (any(is.na(dist_matrix))) {
    stop(
      "Distance matrix contains NA values. This should not happen - please report this issue."
    )
  }

  if (any(dist_matrix < 0)) {
    stop(
      "Distance matrix contains negative values. This should not happen - please report this issue."
    )
  }

  # ========================================
  # STEP 4: VERBOSE OUTPUT
  # ========================================

  if (verbose) {
    cat("\n")
    cat("Distance matrix computation\n")
    cat("---------------------------\n")
    cat(sprintf("Observations: %d\n", n))
    cat(sprintf("Geometry type: %s\n", geometry_type))
    cat(sprintf(
      "Coordinate type: %s\n",
      ifelse(is_geographic, "geographic (lat/lon)", "projected (x/y)")
    ))
    cat(sprintf("Method: sf::st_distance()\n"))
    cat(sprintf("Output units: %s\n", units))

    # Get range excluding diagonal zeros
    non_zero_dists <- dist_matrix[dist_matrix > 0]
    if (length(non_zero_dists) > 0) {
      cat(sprintf(
        "Distance range: %.2f to %.2f %s\n",
        min(non_zero_dists),
        max(non_zero_dists),
        units
      ))
    }
    cat("\n")
  }

  # Return distance matrix
  return(dist_matrix)
}
