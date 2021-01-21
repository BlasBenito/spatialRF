#' @title thinning
#' @description Resamples a set of points with x and y coordinates to impose a minimum distance among nearby points.
#' @param xy a data frame with columns named "x" and "y" representing geographic coordinates.
#' @param minimum.distance minimum distance among nearby points, in the same units as the coordinates of xy.
#' @return a data frame with the same columns as xy with points separated by the defined minimum distance.
#' @details Generally to remove redundant points that could produce pseudo-replication.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  data(plant_richness)
#'  plant_richness.thin <- thinning(
#'    x = plant_richness_df,
#'    minimum.distance = 5 #oints separated by at least 5 degrees
#'    )
#'  plant_richness.thin
#'  }
#' }
#' @rdname thinning
#' @export
thinning <- function(xy, minimum.distance = NULL){

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

  xy

}
