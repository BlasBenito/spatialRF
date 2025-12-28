#' @title Applies thinning to pairs of coordinates until reaching a given n
#' @description Resamples a set of points with x and y coordinates by increasing the distance step by step until a given sample size is obtained.
#' @usage
#' thinning_til_n(
#'   xy,
#'   n = 30,
#'   distance.step = NULL
#' )
#' @param xy A data frame with columns named "x" and "y" representing geographic coordinates. Default: `NULL`
#' @param n Integer, number of samples to obtain. Must be lower than `nrow(xy)`. Default: `30`
#' @param distance.step Numeric, distance step used during the thinning iterations. If `NULL`, the one percent of the maximum distance among points in `xy` is used. Default: `NULL`
#' @return A data frame with the same columns as xy with a row number close to n.
#' @seealso [thinning()]
#' @examples
#'
#' data(plants_xy)
#'
#' y <- thinning_til_n(
#'   xy = plants_xy,
#'   n = 10
#' )
#'
#' if (interactive()) {
#'   plot(
#'     plants_xy[, c("x", "y")],
#'     col = "blue",
#'     pch = 15
#'   )
#'
#'   points(
#'     y[, c("x", "y")],
#'     col = "red",
#'     pch = 15,
#'     cex = 1.5
#'   )
#' }
#'
#' @rdname thinning_til_n
#' @family utilities
#' @autoglobal
#' @export
thinning_til_n <- function(
  xy = NULL,
  n = 30,
  distance.step = NULL
) {
  #coerce to data frame if tibble
  if (inherits(xy, "tbl_df") || inherits(xy, "tbl")) {
    xy <- as.data.frame(xy)
  }
  if (!is.data.frame(xy) || is.null(xy)) {
    stop("xy must be a data frame.")
  }
  if (!("x" %in% colnames(xy))) {
    stop("column x is missing from xy.")
  }
  if (!("y" %in% colnames(xy))) {
    stop("column y is missing from xy.")
  }
  if (!is.numeric(n)) {
    stop("'n' is not a number.")
  }
  #in case raster::res() is used to get the distance
  if (length(distance.step) > 1) {
    distance.step <- distance.step[1]
  }

  #initiating distances
  if (is.null(distance.step)) {
    #getting all distances among points
    xy.distances <- sort(as.vector(stats::dist(xy[, c("x", "y")])))

    #getting the 1%
    min.distance <- distance.i <- max(xy.distances) / 1000

    rm(xy.distances)
  } else {
    #in case it comes from raster::res()
    if (length(distance.step) > 1) {
      distance.step <- distance.step[1]
    }

    #user defined value
    min.distance <- distance.i <- distance.step
  }

  #initiating xy.thin
  xy.thin <- xy

  #apply thinning iteratively
  while (nrow(xy.thin) > n) {
    distance.i <- distance.i + min.distance
    xy.thin <- thinning(xy, minimum.distance = distance.i)
  }

  xy.thin
}
