#' @title Default distance thresholds for spatial predictors
#' @description Generates evenly-spaced distance thresholds for spatial predictor generation, or validates user-provided thresholds. When `distance.thresholds` is NULL, generates defaults ranging up to half the maximum distance. When provided, validates that thresholds are within the valid range of `distance.matrix`.
#' @param distance.thresholds Numeric vector of distance thresholds, or NULL to generate defaults. If provided, must be positive numeric values within the range of `distance.matrix`. Default: `NULL`.
#' @param distance.matrix Numeric distance matrix (typically square and symmetric). Default: `NULL`.
#' @return Numeric vector with unique positive distance thresholds (floored to integers when auto-generated).
#' @details
#' ## Default Generation
#'
#' When `distance.thresholds = NULL`, the function generates evenly-spaced thresholds using [seq()] with `length.out = 4`. The maximum threshold is set to half the maximum distance to avoid spatial predictors based on distances too large to capture meaningful spatial autocorrelation.
#'
#' Zero is excluded from auto-generated thresholds since a threshold of 0 would include no neighbors. If flooring creates duplicate values (in small distance matrices), only unique thresholds are returned.
#'
#' ## Validation
#'
#' When `distance.thresholds` is provided, validates that:
#' \itemize{
#'   \item Values are numeric
#'   \item All values are positive (> 0)
#'   \item Values fall within the range of `distance.matrix`
#' }
#' @examples
#' data(plants_distance)
#'
#' # Generate default thresholds
#' thresholds <- default_distance_thresholds(
#'   distance.matrix = plants_distance
#' )
#' thresholds
#' # Example output: c(2399, 4798, 7198)
#' # Three evenly-spaced thresholds up to max(plants_distance)/2
#'
#' # Validate user-provided thresholds
#' custom_thresholds <- c(1000, 3000, 5000)
#' validated <- default_distance_thresholds(
#'   distance.thresholds = custom_thresholds,
#'   distance.matrix = plants_distance
#' )
#' validated
#'
#' # Invalid thresholds trigger informative errors
#' try(
#'   default_distance_thresholds(
#'     distance.thresholds = c(-100, 2000),
#'     distance.matrix = plants_distance
#'   )
#' )
#' # Error: All values in 'distance.thresholds' must be greater than 0.
#'
#' @rdname default_distance_thresholds
#' @family preprocessing
#' @export
#' @autoglobal
default_distance_thresholds <- function(
  distance.thresholds = NULL,
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

  # If distance.thresholds provided, validate and return
  if (!is.null(distance.thresholds)) {
    # Validation 1: Must be numeric
    if (!is.numeric(distance.thresholds)) {
      stop("The argument 'distance.thresholds' must be numeric.")
    }

    # Validation 2: Must be a vector (not matrix)
    if (is.matrix(distance.thresholds)) {
      stop("The argument 'distance.thresholds' must be a vector, not a matrix.")
    }

    # Validation 3: Must have length >= 1
    if (length(distance.thresholds) < 1) {
      stop("The argument 'distance.thresholds' must have at least one element.")
    }

    # Validation 4: All values must be > 0
    if (any(distance.thresholds <= 0)) {
      stop("All values in 'distance.thresholds' must be greater than 0.")
    }

    # Validation 5: Values must be <= max distance in matrix
    max_distance <- max(distance.matrix, na.rm = TRUE)

    if (any(distance.thresholds > max_distance)) {
      stop(
        "All values in 'distance.thresholds' must be <= max(distance.matrix) = ",
        round(max_distance, 2), "."
      )
    }

    # If all validations pass, return user-provided thresholds
    return(distance.thresholds)
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
