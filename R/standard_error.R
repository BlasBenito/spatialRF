#' @title Standard error of the mean of a numeric vector
#' @description Computes the standard error of the mean of a numeric vector as `round(sqrt(var(x)/length(x)), 3)`
#' @param x A numeric vector.
#' @return A numeric value.
#' @details The function removes `NA` values before computing the standard error, and rounds the result to 3 decimal places.
#' @examples
#'
#'  standard_error(runif(10))
#'
#' @rdname standard_error
#' @export
standard_error <- function(x) {
  x <- na.omit(x)
  x <- round(sqrt(var(x) / length(x)), 3)
  x
}
