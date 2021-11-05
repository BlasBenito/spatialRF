#' @title Gets model predictions
#' @description Returns model predictions from a model fitted with [rf()], [rf_repeat()], or [rf_spatial()].
#' @param model A model produced by [rf()], [rf_repeat()], or [rf_spatial()].
#' @return A vector with predictions, or median of the predictions across repetitions if the model was fitted with [rf_repeat()].
#' @examples
#' if(interactive()){
#'
#' #loading example data
#' data(
#'   ecoregions_df,
#'   ecoregions_distance_matrix,
#'   ecoregions_predvar_names,
#'   ecoregions_depvar_name
#'   )
#'
#'  #fitting random forest model
#'  rf.model <- rf(
#'    data = ecoregions_df,
#'    dependent.variable.name = ecoregions_depvar_name,
#'    predictor.variable.names = ecoregions_predvar_names,
#'    distance.matrix = ecoregions_distance_matrix,
#'    distance.thresholds = 0,
#'    n.cores = 1,
#'    verbose = FALSE
#'  )
#'
#'  #get vector of predictions
#'  x <- get_predictions(rf.model)
#'  x
#'
#' }
#' @rdname get_predictions
#' @export
get_predictions <- function(model){

  model$predictions$values

}
