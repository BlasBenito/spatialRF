#' @title print_performanc()
#' @description Prints the performance slot of a model fitted with [rf()], [rf_repeat()], or [rf_spatial()].
#' @param x Model fitted by [rf()], [rf_repeat()], or [rf_spatial()].
#' @examples
#' \dontrun{
#' if(interactive()){
#' \dontrun{
#' if(interactive()){
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
#'  }
#' }
#' @rdname print_performance
#' @export
print_performance <- function(x){

  x <- x$performance

  if(length(x$r.squared) == 1){
    cat("\n")
    cat("Model performance \n")
    cat("  - R squared (OOB):                 ", x$r.squared, "\n", sep="")
    cat("  - Pseudo R squared:                ", x$pseudo.r.squared, "\n", sep="")
    cat("  - RMSE:                            ", x$rmse, "\n", sep="")
    cat("  - Normalized RMSE:                 ", x$nrmse, "\n", sep="")
  } else {
    cat("\n")
    cat("Model performance (mean +/- standard error) \n")
    cat("  - R squared (OOB):          ", mean(x$r.squared), " +/- ", standard_error(x$r.squared), "\n", sep="")
    cat("  - Pseudo R squared:         ", mean(x$pseudo.r.squared), " +/- ", standard_error(x$pseudo.r.squared), "\n", sep="")
    cat("  - RMSE:                     ", mean(x$rmse), " +/- ", standard_error(x$rmse), "\n", sep="")
    cat("  - Normalized RMSE:          ", mean(x$nrmse), " +/- ", standard_error(x$nrmse), "\n", sep="")
  }


}
