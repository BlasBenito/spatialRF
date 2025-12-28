#' @title Default distance thresholds for spatial predictors
#' @description Generates four evenly-spaced distance thresholds for spatial predictor generation, ranging from 0 to half the maximum distance in the matrix.
#' @param distance.matrix Numeric distance matrix (typically square and symmetric). Default: `NULL`.
#' @return Numeric vector of length 4 with distance thresholds (floored to integers).
#' @details
#' The maximum threshold is set to half the maximum distance to avoid spatial predictors based on distances that are too large to capture meaningful spatial autocorrelation. The four thresholds are evenly spaced using [seq()] with `length.out = 4`.
#' @examples
#' data(plants_distance)
#'
#' thresholds <- default_distance_thresholds(
#'   distance.matrix = plants_distance
#' )
#'
#' thresholds
#' # Example output: c(0, 3333, 6666, 10000)
#' # Four evenly-spaced thresholds from 0 to max(plants_distance)/2
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

  distance.thresholds <- floor(
    seq(
      0,
      max(distance.matrix, na.rm = TRUE) / 2,
      length.out = 4
    )
  )

  distance.thresholds
}
