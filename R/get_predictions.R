#' @title Gets model predictions
#' @description Returns model predictions from a model fitted with [rf()], [rf_repeat()], or [rf_spatial()].
#' @param model A model produced by [rf()], [rf_repeat()], or [rf_spatial()].
#' @return A vector with predictions, or median of the predictions across repetitions if the model was fitted with [rf_repeat()].
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
get_predictions <- function(model){

  model$predictions$values

}
