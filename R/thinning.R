#' @title Thinning of spatial points
#' @description Extracts a sub-sample from a set of spatial points by imposing a given minimum distance between them. Used to reduce sample size while keeping spatial representativity, reduce spatial autocorrelation, or select fold centers for spatial cross validation (see [rf_evaluate()]).
#'
#' The function `thinning()` imposes a `minimum.distance` between spatial points, while `thinning_til_n()` increases the distance between nearby points until a given sample (`n`) size is reached.
#' @param xy (required; data frame) A data frame with columns named "x" and "y" representing  the geographic coordinates of the spatial points.
#' @param minimum.distance (required; numeric) Minimum distance nearby points in the sub-sample. Must be in the same units as the coordinates of `xy`.
#' @param n (optional; integer) Number of samples to obtain. Must be lower than `nrow(xy)`. Default: `30`
#' @param distance.step (optional; numeric) Distance step used during thinning iterations in `thinning_til_n()`. If `NULL`, the one percent of the maximum distance among points in `xy` is used. Default: `NULL`
#'
#' @return A data frame with the same columns as `xy` with spatial points separated by the defined minimum distance in the case of `thinning()`, or with a sample size as close as possible to `n` in the case of `thinning_til_n()`.
#'
#' @details Generally used to remove spatially redundant data to minimize pseudo-replication, to limit sampling bias by disaggregating clusters of points, and to select fold centers for spatial cross-validation.
#' @examples
#' if(interactive()){
#'
#'  #load example data
#'  data(ecoregions_df)
#'
#'  #thinning to obtain points separated by 5 degrees
#'  x <- thinning(
#'    x = ecoregions_df,
#'    minimum.distance = 5 #output points separated by at least 5 degrees
#'    )
#'
#'  x
#'
#'  #sub-sample of given sample size
#'  x <- thinning_til_n(
#'    x = ecoregions_df,
#'    n = 30
#'    )
#'
#'  nrow(ecoregions_df)
#'  nrow(x)
#'
#' }
#' @importFrom dplyr distinct
#' @rdname thinning
#' @export
thinning <- function(
  xy,
  minimum.distance = NULL
  ){

  #declaring variables
  x <- NULL
  y <- NULL

  if(!is.data.frame(xy)){
    stop("xy must be a data frame.")
  }
  if(!("x" %in% colnames(xy))){
    stop("column x is missing from xy.")
  }
  if(!("y" %in% colnames(xy))){
    stop("column y is missing from xy.")
  }
  if(is.null(minimum.distance)){
    stop("minimum.distance is empty.")
  }
  if(length(minimum.distance) > 1){
    minimum.distance <- minimum.distance[1]
  }

  #removing duplicated pairs of coordinates
  xy <- dplyr::distinct(
    xy,
    x, y,
    .keep_all = TRUE
  )

  #count rows
  row.i <- 1

  #repeats til thinning is done
  repeat{

    #target row
    f <- xy[row.i, ]

    #rectangle around the target row
    ymax <- f$y + minimum.distance
    ymin <- f$y - minimum.distance
    xmax <- f$x + minimum.distance
    xmin <- f$x - minimum.distance

    #removes other coordinates within the rectangle
    xy <- xy[!((xy$y <= ymax) & (xy$y >= ymin) & (xy$x <= xmax) & (xy$x >= xmin) & (xy$y != f$y | xy$x != f$x)), ]

    #grows row.i
    row.i <- row.i + 1

    #if no more records to thin, break
    if(row.i >= nrow(xy)){break}
  }

  if(nrow(xy) <= 1){
    message("minimum distance was likely too high, returning very few rows!")
  }

  #removing duplicates
  xy <- dplyr::distinct(xy)

  xy

}


#' @rdname thinning
#' @importFrom stats dist
#' @export
thinning_til_n <- function(
    xy = NULL,
    n = 30,
    distance.step = NULL
){


  if(!is.data.frame(xy) | is.null(xy)){
    stop("xy must be a data frame.")
  }
  if(!("x" %in% colnames(xy))){
    stop("column x is missing from xy.")
  }
  if(!("y" %in% colnames(xy))){
    stop("column y is missing from xy.")
  }
  if(!is.numeric(n)){
    stop("'n' is not a number.")
  }
  #in case raster::res() is used to get the distance
  if(length(distance.step) > 1){
    distance.step <- distance.step[1]
  }

  #initiating distances
  if(is.null(distance.step)){

    #getting all distances among points
    xy.distances <- sort(as.vector(dist(xy[, c("x", "y")])))

    #getting the 1%
    min.distance <- distance.i <- max(xy.distances) / 1000

    rm(xy.distances)

  } else {

    #in case it comes from raster::res()
    if(length(distance.step) > 1){
      distance.step <- distance.step[1]
    }

    #user defined value
    min.distance <- distance.i <- distance.step

  }

  #initiating xy.thin
  xy.thin <- xy

  #apply thinning iteratively
  while(nrow(xy.thin) > n){

    distance.i <- distance.i + min.distance

    xy.thin <- thinning(
      xy = xy,
      minimum.distance = distance.i
    )

  }

  xy.thin

}

