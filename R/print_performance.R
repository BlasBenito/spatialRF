#' @title print_performance
#' @description Prints the performance slot of a model fitted with [rf()], [rf_repeat()], or [rf_spatial()]. For models fitted with [rf_repeat()] it shows the mean and the standard error (computed with the function [standard_error()]) of the mean of each performance measure.
#' @param model Model fitted with [rf()], [rf_repeat()], or [rf_spatial()].
#' @seealso [print_performance()], [get_performance()]
#' @examples
#' \donttest{
#' if(interactive()){
#'
#'  data(plant_richness_df)
#'  data(distance.matrix)
#'
#'  rf.model <- rf(
#'    data = plant_richness_df,
#'    dependent.variable.name = "richness_species_vascular",
#'    predictor.variable.names = colnames(plant_richness_df)[5:21],
#'    distance.matrix = distance_matrix,
#'    distance.thresholds = c(0, 1000, 2000),
#'    verbose = FALSE
#'  )
#'
#'  print_performance(rf.model)
#'
#' }
#' }
#' @rdname print_performance
#' @export
print_performance <- function(model){

  x <- model$performance

  if(length(x$r.squared) == 1){
    cat("\n")
    cat("Model performance \n")
    cat("  - R squared (oob):                 ", x$r.squared.oob, "\n", sep="")
    cat("  - R squared (cor(obs, pred)^2):    ", x$r.squared, "\n", sep="")
    cat("  - Pseudo R squared(cor(obs, pred)):", x$pseudo.r.squared, "\n", sep="")
    cat("  - RMSE:                            ", x$rmse, "\n", sep="")
    cat("  - Normalized RMSE:                 ", x$nrmse, "\n", sep="")
  } else {
    cat("\n")
    cat("Model performance (mean +/- standard error) \n")
    cat("  - R squared (oob):                 ", round(mean(x$r.squared.oob), 3), " +/- ", standard_error(x$r.squared.oob), "\n", sep="")
    cat("  - R squared (cor(obs, pred)^2):     ", round(mean(x$r.squared), 3), " +/- ", standard_error(x$r.squared), "\n", sep="")
    cat("  - Pseudo R squared:         ", round(mean(x$pseudo.r.squared), 3), " +/- ", standard_error(x$pseudo.r.squared), "\n", sep="")
    cat("  - RMSE:                     ", round(mean(x$rmse), 3), " +/- ", standard_error(x$rmse), "\n", sep="")
    cat("  - Normalized RMSE:          ",round( mean(x$nrmse), 3), " +/- ", standard_error(x$nrmse), "\n", sep="")
  }


}
