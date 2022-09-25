#' @title Functions to retrieve data from a fitted model.
#' @description All `get_xxx()` functions retrieve useful information from a fitted model.
#' @param model A model fitted with [rf_evaluate()].
#' @return The objects returned by each function are listed below:
#' \itemize{
#'   \item `get_evaluation()`
#'   \item `get_importance()`
#'   \item `get_importance_local()`
#'   \item `get_moran()`
#'   \item `get_performance()`
#'   \item `get_predictions()`
#'   \item `get_residuals()`
#'   \item `get_response_curves()`
#'   \item `get_spatial_predictors`
#'
#' }
#' @examples
#' if(interactive()){
#'
#' #loading example data
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
#'    distance.thresholds = c(0, 100, 1000),
#'    n.cores = 1,
#'    verbose = FALSE
#'  )
#'
#' #evaluating the model with spatial cross-validation
#' rf.model <- rf_evaluate(
#'   model = rf.model,
#'   xy = ecoregions_df[, c("x", "y")],
#'   n.cores = 1,
#'   verbose = FALSE
#' )
#'
#' #getting evaluation results from the model
#' x <- get_evaluation(rf.model)
#' x
#'
#' }
#' @rdname get
#' @export
get_evaluation <- function(model){

  #stop if no evaluation slot
  if(!inherits(model, "rf_evaluate")){
    stop("Object 'x' does not have an 'evaluation' slot.")
  }

  model$evaluation$aggregated

}
