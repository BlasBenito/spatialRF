#' @title RMSE and normalized RMSE
#' @description Computes the rmse or normalized rmse (nrmse) between two numeric vectors of the same length representing observations and model predictions.
#' @param o Numeric vector with observations, must have the same length as `p`.
#' @param p Numeric vector with predictions, must have the same length as `o`.
#' @param normalization character, normalization method, Default: "rmse" (see Details).
#' @return Named numeric vector with either one or 5 values, as selected by the user.
#' @details The normalization methods go as follows:
#' \itemize{
#'   \item `"rmse"`: RMSE with no normalization.
#'   \item `"mean"`: RMSE dividied by the mean of the observations (rmse/mean(o)).
#'   \item `"sd"`: RMSE dividied by the standard deviation of the observations (rmse/sd(o)).
#'   \item `"maxmin"`: RMSE divided by the range of the observations (rmse/(max(o) - min(o))).
#'   \item "`iq"`: RMSE divided by the interquartile range of the observations (rmse/(quantile(o, 0.75) - quantile(o, 0.25)))
#' }
#' @examples
#' if(interactive()){
#'
#'  root_mean_squared_error(
#'    o = runif(10),
#'    p = runif(10)
#'    )
#'
#' }
#' @rdname root_mean_squared_error
#' @importFrom stats na.omit quantile
#' @export
root_mean_squared_error <- function(o, p, normalization = c("rmse", "all", "mean", "sd", "maxmin", "iq")) {

  normalization <- match.arg(normalization)

  #computes rmse
  squared_sums <- sum((o - p)^2)
  mse <- squared_sums/length(o)
  rmse <- round(sqrt(mse), 4)
  names(rmse) <- "rmse"

  #computes nrmse
  if(normalization != "rmse"){

    #computing nrmse
    nrmse.sd <- rmse/sd(o)
    nrmse.mean <- rmse/mean(o)
    nrmse.maxmin <- rmse/ (max(o) - min(o))
    nrmse.iq <- rmse/ (quantile(o, 0.75) - quantile(o, 0.25))

    #building vector with nrmse values
    rmse <- c(rmse, nrmse.iq, nrmse.maxmin, nrmse.mean, nrmse.sd)
    names(rmse) <- c("rmse", "iq", "maxmin", "mean", "sd")

    #removing infinites
    rmse[!is.finite(rmse)] <- NA
    rmse <- na.omit(rmse)

    #if the user selects one particular option
    if(normalization != "all"){
      rmse <- rmse[names(rmse) == normalization]
    }
  }

  #return NA if no number is returned
  if(length(rmse) == 0){
    rmse <- NA
  }

  rmse
}


