#' @title print_performance
#' @description Prints the performance slot of a model fitted with [rf()], [rf_repeat()], or [rf_spatial()]. For models fitted with [rf_repeat()] it shows the median and the median absolute deviation of each performance measure.
#' @param model Model fitted with [rf()], [rf_repeat()], or [rf_spatial()].
#' @return Prints model performance scores to the console.
#' @seealso [print_performance()], [get_performance()]
#' @examples
#' if(interactive()){
#'
#'  #loading example data
#' data(
#'   ecoregions_df,
#'   ecoregions_distance_matrix,
#'   ecoregions_predictor_variable_names,
#'   ecoregions_dependent_variable_name
#'   )
#'
#'  #fitting random forest model
#'  rf.model <- rf(
#'    data = ecoregions_df,
#'    dependent.variable.name = ecoregions_dependent_variable_name,
#'    predictor.variable.names = ecoregions_predictor_variable_names,
#'    distance.matrix = ecoregions_distance_matrix,
#'    distance.thresholds = 0,
#'    n.cores = 1,
#'    verbose = FALSE
#'  )
#'
#'  #printing performance scores
#'  print_performance(rf.model)
#'
#' }
#' @rdname print_performance
#' @export
print_performance <- function(model){

  x <- model$performance

  if(length(x$r.squared) == 1){
    cat("\n")
    cat("Model performance \n")
    cat("  - R squared (oob):                  ", x$r.squared.oob, "\n", sep="")
    cat("  - R squared (cor(obs, pred)^2):     ", x$r.squared, "\n", sep="")
    cat("  - RMSE (oob):                       ", x$rmse.oob, "\n", sep="")
    cat("  - RMSE:                             ", x$rmse, "\n", sep="")
    cat("  - Normalized RMSE:                  ", x$nrmse, "\n", sep="")
  } else {
    cat("\n")
    cat("Model performance (median +/- mad) \n")
    cat("  - R squared (oob):              ", round(median(x$r.squared.oob), 3), " +/- ", round(mad(x$r.squared.oob), 4), "\n", sep="")
    cat("  - R squared (cor(obs, pred)^2): ", round(median(x$r.squared), 3), " +/- ", round(mad(x$r.squared), 4), "\n", sep="")
    cat("  - RMSE (oob):                   ", round(median(x$rmse.oob), 3), " +/- ", round(mad(x$rmse.oob), 4), "\n", sep="")
    cat("  - RMSE:                         ", round(median(x$rmse), 3), " +/- ", round(mad(x$rmse), 4), "\n", sep="")
    cat("  - Normalized RMSE:              ",round(median(x$nrmse), 3), " +/- ", round(mad(x$nrmse), 4), "\n", sep="")
  }


}
