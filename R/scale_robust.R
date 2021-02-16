#' @title Robust scaling of training data frames
#' @description Scales a matrix or a data frame by the median instead of the mean.
#' @param x Matrix or data.frame
#' @param center Logical. If `TRUE`, substracts the median to center the data. Default: `TRUE`
#' @param scale Logical. If `TRUE`, scales the data by the median absolute deviation. Default: `TRUE`
#' @return a scaled matrix or dataframe
#' @examples
#' \donttest{
#' if(interactive()){
#'
#' data(plant_richness_df)
#' plant_richness_df.scaled <- scale_robust(x = plant_richness_df[, 5:10])
#'
#' }
#' }
#' @rdname scale_robust
#' @importFrom stats mad median
#' @export
scale_robust <- function(
  x,
  center = TRUE,
  scale = TRUE
  ){

  if(center == TRUE){

    medians <- apply(
      x,
      2,
      median,
      na.rm = TRUE)

    x = sweep(
      x,
      2,
      medians,
      "-"
      )

  }


  if(scale == TRUE){

    mads <- apply(
      x,
      2,
      mad,
      na.rm =TRUE
      )

    x = sweep(
      x,
      2,
      mads,
      "/"
      )

  }

  x

}
