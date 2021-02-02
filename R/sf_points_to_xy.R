#' @title sf_points_to_xy
#' @description Transforms an \link[sf]{sf} data frame with points into a data frame with columns *x* and *y*.
#' @param sf.points An \link[sf]{sf} data frame with points (geometry class "sfc_POINT").
#' @return A data frame with the point coordinates stored in the columns *x* and *y*.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  data(plant_richness_sf)
#'  xy <- sf_points_to_xy(plant_richness_sf)
#'  }
#' }
#' @rdname sf_points_to_xy
#' @export
#' @importFrom sf st_geometry
sf_points_to_xy <- function(sf.points){

  #process sf.points to get xy
  if(!is.null(sf.points) &
     inherits(sf.points, c("sf", "data.frame")) &
     inherits(sf::st_geometry(sf.points),"sfc_POINT")){

    #get xy columns
      xy <- as.data.frame(
        do.call(
          "rbind",
          sf::st_geometry(sf.points)
        )
      )
      colnames(xy) <- c("x", "y")
  } else {
    stop("Argument 'sf.points' is not an sf object with pairs of coordinates.")
  }

  xy

}
