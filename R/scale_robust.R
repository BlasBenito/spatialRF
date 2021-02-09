#' @title scale_robust
#' @description scales a matrix or a data frame by the median instead of the mean.
#' @param x matrix or data.frame
#' @param center substract median (default:TRUE)
#' @param scale scale by the median absolute deviation (mad())  (default:FALSE)
#' @return a scaled matrix or dataframe
#' @details Adapted from \link[quantable]{robustscale}.
#' @examples
#' data(plant_richness_df)
#' plant_richness_df.scaled <- scale_robust(x = plant_richness_df[, 5:10])
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
