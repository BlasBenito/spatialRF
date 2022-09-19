#' @title Gets performance data frame from a cross-validated model
#' @description Returns performance metrics produced by [rf_evaluate()].
#' @param model A model fitted with [rf_evaluate()].
#' @return A data frame with evaluation scores. The following columns are shown:
#' \itemize{
#'   \item `model`: Identifies the given model. The values are "Full", (original model introduced into [rf_evaluate()]), "Training" (model trained on an independent training spatial fold), and "Testing" (predictive performance of the training model on an independent testing spatial fold). The performance values of the "Testing" model represent the model performance on unseen data, and hence its ability to generalize.
#'   \item `metric`: Evaluation metrics, "rmse", "nrmse", "r.squared", and "auc" (only when the response is binary).
#'   \item `mean`, `sd`, `min`, and `max`: Average, standard deviation, minimum, and maximum of each metric across the evaluation (cross-validation) iterations.
#'
#' }
#' @seealso [rf_evaluate()], [plot_evaluation()], [print_evaluation()]
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
#' @rdname get_evaluation
#' @export
get_evaluation <- function(model){

  #stop if no evaluation slot
  if(!inherits(model, "rf_evaluate")){
    stop("Object 'x' does not have an 'evaluation' slot.")
  }

  model$evaluation$aggregated

}
