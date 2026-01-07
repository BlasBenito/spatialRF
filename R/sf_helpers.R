# R/sf_helpers.R
# Internal helper functions for handling sf data frames
# None of these functions are exported

# Helper 1: is_sf() ----

#' Check if object is an sf data frame
#' @param x Object to check
#' @return Logical, TRUE if x is an sf object
#' @noRd
is_sf <- function(x) {
  inherits(x = x, what = "sf")
}

# Helper 2: sf_to_xy() ----

#' Extract coordinates from sf geometry
#' @param sf_data An sf data frame
#' @param add_id Logical, if TRUE adds an "id" column with row numbers
#' @return Data frame with columns "x", "y", and optionally "id"
#' @noRd
sf_to_xy <- function(sf_data, add_id = FALSE) {

  # Check if geometry is empty
  if (nrow(sf_data) == 0) {
    stop("sf object has empty geometry. Cannot extract coordinates.")
  }

  # Get geometry types
  geom_types <- as.character(sf::st_geometry_type(sf_data, by_geometry = FALSE))

  # Handle different geometry types
  if (geom_types %in% c("POINT", "MULTIPOINT")) {
    # Direct extraction for POINT and MULTIPOINT
    coords <- sf::st_coordinates(sf_data)

  } else if (geom_types %in% c("POLYGON", "MULTIPOLYGON")) {
    # Use centroids for POLYGON and MULTIPOLYGON
    centroids <- sf::st_centroid(sf_data)
    coords <- sf::st_coordinates(centroids)

  } else if (geom_types %in% c("LINESTRING", "MULTILINESTRING")) {
    # Error for LINESTRING geometries
    stop(
      "LINESTRING geometries not supported. ",
      "Convert to POINT geometries before using spatialRF functions."
    )

  } else {
    # Catch-all for other geometry types
    stop(
      "Geometry type '", geom_types, "' not supported. ",
      "Supported types: POINT, MULTIPOINT, POLYGON, MULTIPOLYGON."
    )
  }

  # Validate coordinates were extracted
  if (nrow(coords) == 0) {
    stop("Failed to extract coordinates from sf geometry.")
  }

  # Create xy data frame
  xy <- data.frame(
    x = coords[, 1],
    y = coords[, 2],
    stringsAsFactors = FALSE
  )

  # Add id column if requested
  if (add_id) {
    xy$id <- seq_len(nrow(xy))
  }

  return(xy)
}

# Helper 3: extract_xy_from_data_frame() ----

#' Extract coordinates from regular data frame using flexible column matching
#' @param data A data frame (not sf)
#' @return Data frame with "x" and "y" columns if found, NULL otherwise
#' @noRd
extract_xy_from_data_frame <- function(data) {

  # Convert column names to lowercase for matching
  colnames_lower <- tolower(colnames(data))

  # Define flexible column names
  x_names <- c("x", "lon", "long", "longitude", "longitud")
  y_names <- c("y", "lat", "latitude", "latitud")

  # Find matching columns
  x_column <- intersect(
    x = colnames_lower,
    y = x_names
  )

  y_column <- intersect(
    x = colnames_lower,
    y = y_names
  )

  # Return NULL if no matches found
  if (length(x_column) == 0 || length(y_column) == 0) {
    return(NULL)
  }

  # Get original column names (preserve case)
  x_col_original <- colnames(data)[which(colnames_lower == x_column[1])[1]]
  y_col_original <- colnames(data)[which(colnames_lower == y_column[1])[1]]

  # Extract coordinates
  xy <- data.frame(
    x = data[[x_col_original]],
    y = data[[y_col_original]],
    stringsAsFactors = FALSE
  )

  return(xy)
}

# Helper 4: drop_geometry_if_sf() ----

#' Drop geometry column from sf object and coerce to data frame
#' @param data Data frame or sf object
#' @return Plain data frame with geometry removed (if was sf)
#' @noRd
drop_geometry_if_sf <- function(data) {

  # Drop sf geometry if present
  if (is_sf(data)) {
    data <- sf::st_drop_geometry(data)
  }

  # Coerce tibbles to data frames
  if (inherits(data, "tbl_df") || inherits(data, "tbl")) {
    data <- as.data.frame(data)
  }

  return(data)
}

# Helper 5: extract_xy_from_data() ----

#' Unified xy extraction logic for sf and regular data frames
#' @param data Data frame or sf object
#' @param xy User-provided xy data frame (can be NULL)
#' @param require_id Logical, if TRUE ensures xy has an "id" column
#' @return Named list with elements 'data' (geometry-free) and 'xy' (coordinates or NULL)
#' @noRd
extract_xy_from_data <- function(data, xy = NULL, require_id = FALSE) {

  # Scenario 1: sf + no xy
  if (is_sf(data) && is.null(xy)) {
    xy <- sf_to_xy(data, add_id = require_id)
    data <- drop_geometry_if_sf(data)
    return(list(data = data, xy = xy))
  }

  # Scenario 2: sf + xy provided
  if (is_sf(data) && !is.null(xy)) {
    data <- drop_geometry_if_sf(data)
    return(list(data = data, xy = xy))
  }

  # Scenario 3: Regular data + no xy
  if (!is_sf(data) && is.null(xy)) {
    # Try flexible column matching
    xy <- extract_xy_from_data_frame(data)
    # Add id if required and xy was found
    if (!is.null(xy) && require_id) {
      xy$id <- seq_len(nrow(xy))
    }
    return(list(data = data, xy = xy))
  }

  # Scenario 4: Regular data + xy provided
  if (!is_sf(data) && !is.null(xy)) {
    return(list(data = data, xy = xy))
  }

  # Fallback (should not reach here)
  return(list(data = data, xy = xy))
}
