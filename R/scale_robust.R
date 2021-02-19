#' @title Robust scaling of training data frames
#' @description Scales a matrix or a data frame by the median instead of the mean. Silently removes rows with NA, non-numeric columns, and columns with variance or mode equal to 0 before scaling.
#' @usage
#' scale_robust(
#'   x,
#'   center = TRUE,
#'   scale = TRUE
#' )
#' @param x Matrix or data.frame
#' @param center Logical. If `TRUE`, subtracts the median to center the data. Default: `TRUE`
#' @param scale Logical. If `TRUE`, scales the data by the median absolute deviation. Default: `TRUE`
#' @return A scaled matrix or dataframe
#' @details Adapted from the `robustscale()` function of the [quantable](https://cran.r-project.org/package=quantable) package.
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

  #removing non-numeric and zero variance columns
  x <- na.omit(x)
  x <- x[sapply(x, is.numeric)]
  x <- x[ , which(round(apply(x, 2, var), 4) != 0)]

  #removing columns with mode 0
  x <- x[ , which(round(as.numeric(apply(x, 2, statistical_mode)), 4) != 0)]

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
