#' @title Compute pairwise distance matrix
#' @description Computes a square distance matrix from point or polygon geometries using [sf::st_distance()]. Supports sf objects and data frames with coordinate columns. Automatically handles both geographic (lat/lon) and projected coordinate systems. Output distances are always in meters (consistent with sf::st_distance()).
#' @param data Data frame with coordinate columns OR sf data frame. Coordinate columns can be named: longitude (x, X, lon, long, longitude, longitud) and latitude (y, Y, lat, latitude, latitud). Column name matching is case-insensitive. Default: `NULL`.
#' @param crs Coordinate reference system for data frame inputs. Can be numeric EPSG code (e.g., `4326` for WGS84), a proj4string, or `NA`. Ignored if `data` is an sf object (uses existing CRS). Default: `4326`
#' @param verbose Logical. If `TRUE`, prints distance range information. Default: `TRUE`.
#' @return Numeric matrix (n×n) with pairwise distances in meters. Diagonal is 0. Row and column names are character indices ("1", "2", "3", ...").
#' @details
#'
#' ## Output Units
#'
#' The returned matrix is in **meters** to maintain consistency with [sf::st_distance()], and has an attribute "units" with the value "m".
#'
#' **IMPORTANT:** When using the distance matrix with other spatialRF functions, ensure `distance.thresholds` are also specified in meters to maintain unit consistency.
#'
#' ## Computational Complexity
#'
#' Distance matrix computation requires O(n²) pairwise distance calculations for n observations. Memory footprint is 8n² bytes (approximately 800 MB for n=10,000). For large datasets (n > 5,000), computation may take several minutes.
#'
#' @examples
#' #from dataframe
#' plants_distance <- distance_matrix(
#'   data = plants_df,
#'   crs = 4326
#' )
#'
#' #it's in meters
#' attributes(plants_distance)$units
#'
#' #convert to km
#' plants_distance <- plants_distance / 1000
#' #not required
#' attributes(plants_distance)$units <- "km"
#'
#' #from sf
#' plants_sf <- sf::st_as_sf(
#'   x = plants_df,
#'   coords = c("x", "y"),
#'   crs = 4326
#' )
#'
#' plants_distance <- distance_matrix(
#'   data = plants_sf
#' )
#'
#' @family preprocessing
#' @rdname distance_matrix
#' @export
#' @autoglobal
distance_matrix <- function(
  data = NULL,
  crs = 4326,
  verbose = TRUE
) {
  # Validation: data not NULL
  if (is.null(data)) {
    stop("Argument 'data' is required.")
  }

  if (!is.data.frame(data)) {
    stop("Argument 'data' must be of class 'data.frame'.")
  }

  if (!inherits(x = data, what = "sf")) {
    colnames(data) <- tolower(colnames(data))

    x_names <- c("x", "lon", "long", "longitude", "longitud")
    y_names <- c("y", "lat", "latitude", "latitud")

    x_column <- intersect(
      x = colnames(data),
      y = x_names
    )

    if (length(x_column) == 0) {
      stop(
        "Argument 'data' must have one of these column names: ",
        paste0(x_names, collapse = ", ")
      )
    }

    y_column <- intersect(
      x = colnames(data),
      y = y_names
    )

    if (length(y_column) == 0) {
      stop(
        "Argument 'data' must have one of these column names: ",
        paste0(y_names, collapse = ", ")
      )
    }

    data_sf <- sf::st_as_sf(
      x = data[, c(x_column, y_column)],
      coords = c(x_column, y_column),
      crs = crs
    )
  } else {
    data_sf <- data
  }

  n <- nrow(data_sf)

  # distance calculation
  dist_matrix <- sf::st_distance(x = data_sf)

  #drop units
  dist_matrix_units <- attributes(dist_matrix)$units$numerator

  dist_matrix <- unclass(dist_matrix)
  attr(dist_matrix, "units") <- dist_matrix_units

  # row/column names as character indices
  rownames(dist_matrix) <- as.character(1:n)
  colnames(dist_matrix) <- as.character(1:n)

  # diagonal is exactly 0 (handle floating point errors)
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

  if (verbose) {
    # Get range excluding diagonal zeros
    non_zero_dists <- dist_matrix[dist_matrix > 0]
    if (length(non_zero_dists) > 0) {
      message(sprintf(
        "Distance matrix: %d observations, range %.2f to %.2f %s",
        n,
        min(non_zero_dists),
        max(non_zero_dists),
        dist_matrix_units
      ))
    }
  }

  dist_matrix
}
