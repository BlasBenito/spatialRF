#' @title Applies thinning to pairs of coordinates
#' @description Resamples a set of points with x and y coordinates to impose a minimum distance among nearby points.
#' @param xy A data frame with columns named "x" and "y" representing geographic coordinates.
#' @param minimum.distance Numeric, minimum distance to be set between nearby points, in the same units as the coordinates of xy.
#' @return A data frame with the same columns as `xy` with points separated by the defined minimum distance.
#' @details Generally used to remove redundant points that could produce pseudo-replication, and to limit sampling bias by disaggregating clusters of points.
#' @seealso [thinning_til_n()]
#' @examples
#'
#' data(plants_xy)
#'
#' y <- thinning(
#'   xy = plants_xy,
#'   minimum.distance = 10
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
#'     pch = 15
#'   )
#' }
#'
#' @rdname thinning
#' @family utilities
#' @export
thinning <- function(
  xy,
  minimum.distance = NULL
) {
  #coerce to data frame if tibble
  if (inherits(xy, "tbl_df") || inherits(xy, "tbl")) {
    xy <- as.data.frame(xy)
  }

  if (!is.data.frame(xy)) {
    stop("xy must be a data frame.")
  }
  if (!("x" %in% colnames(xy))) {
    stop("column x is missing from xy.")
  }
  if (!("y" %in% colnames(xy))) {
    stop("column y is missing from xy.")
  }
  if (is.null(minimum.distance)) {
    stop("minimum.distance is empty.")
  }
  if (length(minimum.distance) > 1) {
    minimum.distance <- minimum.distance[1]
  }

  #count rows
  row.i <- 1

  #repeats til thinning is done
  repeat {
    #target row
    f <- xy[row.i, ]

    #rectangle around the target row
    ymax <- f$y + minimum.distance
    ymin <- f$y - minimum.distance
    xmax <- f$x + minimum.distance
    xmin <- f$x - minimum.distance

    #removes other coordinates within the rectangle
    xy <- xy[
      !((xy$y <= ymax) &
        (xy$y >= ymin) &
        (xy$x <= xmax) &
        (xy$x >= xmin) &
        (xy$y != f$y | xy$x != f$x)),
    ]

    #grows row.i
    row.i <- row.i + 1

    #if no more records to thin, break
    if (row.i >= nrow(xy)) {
      break
    }
  }

  if (nrow(xy) <= 1) {
    message("minimum distance was likely too high, returning very few rows!")
  }

  #removing duplicates
  xy <- dplyr::distinct(xy)

  xy
}
