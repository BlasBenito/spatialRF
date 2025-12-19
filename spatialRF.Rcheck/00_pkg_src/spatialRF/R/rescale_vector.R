#' @title Rescales a numeric vector into a new range
#' @description Rescales a numeric vector to a new range.
#' @usage
#' rescale_vector(
#'   x = NULL,
#'   new.min = 0,
#'   new.max = 1,
#'   integer = FALSE
#' )
#' @param x Numeric vector. Default: `NULL`
#' @param new.min New minimum value. Default: `0`
#' @param new.max New maximum value. Default: `1`
#' @param integer Logical, if `TRUE`, coerces the output to integer. Default: `FALSE`
#' @return A numeric vector of the same length as x, but with its values rescaled between `new.min` and `new.max.`
#' @examples
#'
#' y <- rescale_vector(
#'   x = rnorm(100),
#'   new.min = 0,
#'   new.max = 100,
#'   integer = TRUE
#' )
#' y
#'
#' @rdname rescale_vector
#' @family utilities
#' @export
rescale_vector <- function(
  x = NULL,
  new.min = 0,
  new.max = 1,
  integer = FALSE
) {
  if (is.null(x) || !is.vector(x) || !is.numeric(x)) {
    stop("x must be a numeric vector.")
  }

  #data extremes
  old.min <- min(x)
  old.max <- max(x)

  #scaling
  x <- ((x - old.min) / (old.max - old.min)) * (new.max - new.min) + new.min

  #to integer
  if (integer) {
    x <- floor(x)
  }

  x
}
