#' @title Gets model predictions
#' @description Returns model predictions from a model fitted with [rf()], [rf_repeat()], or [rf_spatial()].
#' @param model A model produced by [rf()], [rf_repeat()], or [rf_spatial()].
#' @return A vector with predictions, or median of the predictions across repetitions if the model was fitted with [rf_repeat()].
#' @examples
#' if(interactive()){
#'
#' #loading example data
#' data(plant_richness_df)
#'
#' #fitting a random forest model
#' rf.model <- rf(
#'   data = plant_richness_df,
#'   dependent.variable.name = "richness_species_vascular",
#'   predictor.variable.names = colnames(plant_richness_df)[5:21],
#'   n.cores = 1,
#'   verbose = FALSE
#' )
#'
#' #get vector of predictions
#' x <- get_predictions(rf.model)
#' x
#'
#' }
#' @rdname get_predictions
#' @export
get_predictions <- function(model){

  model$predictions$values

}
