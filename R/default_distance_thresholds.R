#' @title Default distance thresholds to generate spatial predictors
#' @description Generates four distance thresholds, from 0 to max(distance.matrix)/2.
#' @param distance.matrix Distance matrix. Default: `NULL`.
#' @return A numeric vector with distance thresholds.
#' @examples
#' if(interactive()){
#'
#'  #loading example distance matrix
#'  data(distance_matrix)
#'
#'  #computing set of default distance thresholds
#'  default_distance_thresholds(distance_matrix)
#'
#'  }
#' @rdname default_distance_thresholds
#' @export
default_distance_thresholds <- function(
  distance.matrix = NULL
){

  #stopping if no distance matrix
  if(is.null(distance.matrix)){
    stop("The argument 'distance.matrix' is missing.")
  }

  distance.thresholds <- floor(
    seq(
      0,
      max(distance.matrix, na.rm = TRUE)/2,
      length.out = 4
    )
  )

  distance.thresholds

}
