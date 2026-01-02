#' @title Default distance thresholds for spatial predictors
#' @description Generates evenly-spaced distance thresholds for spatial predictor generation, ranging up to half the maximum distance in the matrix.
#' @param distance.matrix Numeric distance matrix (typically square and symmetric). Default: `NULL`.
#' @return Numeric vector with unique positive distance thresholds (floored to integers).
#' @details
#' The maximum threshold is set to half the maximum distance to avoid spatial predictors based on distances that are too large to capture meaningful spatial autocorrelation. Thresholds are evenly spaced using [seq()] with `length.out = 4`.
#'
#' Zero is excluded from the output since a threshold of 0 would include no neighbors. If flooring creates duplicate values (which can happen with small distance matrices), the function ensures all returned thresholds are unique. This may result in fewer than 4 thresholds for very small distance matrices.
#' @examples
#' data(plants_distance)
#'
#' thresholds <- default_distance_thresholds(
#'   distance.matrix = plants_distance
#' )
#'
#' thresholds
#' # Example output: c(2399, 4798, 7198)
#' # Three evenly-spaced thresholds up to max(plants_distance)/2
#'
#' @rdname default_distance_thresholds
#' @family preprocessing
#' @export
#' @autoglobal
default_distance_thresholds <- function(
  distance.matrix = NULL
) {
  #stopping if no distance matrix
  if (is.null(distance.matrix)) {
    stop("The argument 'distance.matrix' is missing.")
  }

  # Validate input
  if (!is.numeric(distance.matrix)) {
    stop("The argument 'distance.matrix' must be numeric.")
  }

  # Get maximum distance (diagonal is 0, so this gets max off-diagonal distance)
  max_distance <- max(distance.matrix, na.rm = TRUE)

  # Generate thresholds
  distance.thresholds <- floor(
    seq(
      0,
      max_distance / 2,
      length.out = 4
    )
  )

  # Remove duplicates (can happen with small distance matrices)
  distance.thresholds <- unique(distance.thresholds)

  # Remove zero (threshold of 0 includes no neighbors)
  distance.thresholds <- distance.thresholds[distance.thresholds > 0]

  # Return unique positive thresholds
  distance.thresholds
}
