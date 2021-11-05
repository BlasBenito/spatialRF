#' @title Gets model residuals
#' @description Returns the residuals of models fitted with [rf()], [rf_repeat()], or [rf_spatial()].
#' @param model A model fitted with [rf()], [rf_repeat()], or [rf_spatial()].
#' @return A vector with model residuals, or the median of model residuals across repetitions if the model was fitted with [rf_repeat()].
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
#'  #getting vector with residuals
#'  x <- get_residuals(rf.model)
#'  x
#'
#' }
#' @rdname get_residuals
#' @export
get_residuals <- function(model){

    model$residuals$values

}
