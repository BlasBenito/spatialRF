#' @title Gets model predictions
#' @description Returns model predictions from a model fitted with [rf()], [rf_repeat()], or [rf_spatial()].
#' @param x A model produced by [rf()], [rf_repeat()], or [rf_spatial()].
#' @return A data frame with predictions, or mean and standard deviation of the predictions if the model was fitted with [rf_repeat()].
#' @examples
#' \donttest{
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
#' x <- get_predictions(x = rf.model)
#' x
#'
#' }
#' }
#' @rdname get_predictions
#' @export
get_predictions <- function(x){

  if("rf" %in% class(x)){
    predictions <- x$predictions
    predictions <- data.frame(
      predictions = predictions
    )
  }

  if("rf_repeat" %in% class(x)){
    predictions <- x$predictions$mean
  }

  predictions

}
