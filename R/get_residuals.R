#' @title get_residuals
#' @description Returns model residuals
#' @param x A model produced by [rf()], [rf_repeat()], or [rf_spatial()]. Default: NULL
#' @return A data frame with residuals, or mean and standard deviation of the residuals if the model was fitted with [rf_repeat()].
#' @examples
#' \dontrun{
#' if(interactive()){
#'
#' data(plant_richness_df)
#'
#' rf.model <- rf(
#'   data = plant_richness_df,
#'   dependent.variable.name = "richness_species_vascular",
#'   predictor.variable.names = colnames(plant_richness_df)[5:21],
#'   verbose = FALSE
#' )
#'
#' x <- get_residuals(x = rf.model)
#'  }
#' }
#' @rdname get_residuals
#' @export
get_residuals <- function(x){

  if("rf" %in% class(x)){
    residuals <- x$residuals
    residuals <- data.frame(
      residuals = residuals
    )
  }

  if("rf_repeat" %in% class(x)){
    residuals <- x$residuals$mean
  }

  residuals

}
