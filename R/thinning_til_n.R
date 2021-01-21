#' @title thinning_til_n
#' @description Resamples a set of points with x and y coordinates by increasing the distance step by step until a given sample size is obtained.
#' @param xy a data frame with columns named "x" and "y" representing geographic coordinates.
#' @param n number of samples to obtain.
#' @return a data frame with the same columns as xy with a row number close to n.
#' @examples
#'  data(plant_richness)
#'  plant_richness.thin <- thinning_til_n(
#'    x = plant_richness_df,
#'    n = 100
#'    )
#'  plant_richness.thin
#' @rdname thinning_til_n
#' @importFrom stats dist
#' @export
thinning_til_n <- function(xy, n = 30){

  if(!is.data.frame(xy)){
    stop("xy must be a data frame.")
  }
  if(!("x" %in% colnames(xy))){
    stop("column x is missing from xy.")
  }
  if(!("y" %in% colnames(xy))){
    stop("column y is missing from xy.")
  }

  #initiating distances
  xy.distances <- dist(xy)
  min.distance <- distance.i <- min(xy.distances) / 2
  rm(xy.distances)

  #initiating xy.thin
  xy.thin <- xy

  while(nrow(xy.thin) > n){
    distance.i <- distance.i + min.distance
    xy.thin <- thinning(xy, minimum.distance = distance.i)
  }

  xy.thin

}
