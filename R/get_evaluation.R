#' @title Gets performance data frame from a cross-validated model
#' @description Returns performance metrics produced by [rf_evaluate()].
#' @param model A model fitted with [rf_evaluate()].
#' @return A data frame with evaluation scores. The following columns are shown:
#' \itemize{
#'   \item `model`: Identifies the given model. The values are "Full", (original model introduced into [rf_evaluate()]), "Training" (model trained on an independent training spatial fold), and "Testing" (predictive performance of the training model on an independent testing spatial fold). The performance values of the "Testing" model represent the model performance on unseen data, and hence its ability to generalize.
#'   \item `metric`: Four values representing different evaluation metrics, "rmse", "nrmse", "r.squared", and "pseudo.r.squared".
#'   \item `mean`, `sd`, `min`, and `max`: Average, standard deviation, minimum, and maximum of each metric across the evaluation (cross-validation) iterations.
#'
#' }
#' @seealso [rf_evaluate()], [plot_evaluation()], [print_evaluation()]
#' @examples
#'
#' #loading data
#' data(plants_df)
#' data(plants_distance)
#'
#' #fitting a random forest model
#' rf.model <- rf(
#'   data = plants_df,
#'   dependent.variable.name = plants_response,
#'   predictor.variable.names = plants_predictors,
#'   distance.matrix = plants_distance,
#'   distance.thresholds = 0,
#'   n.cores = 1,
#'   verbose = FALSE
#' )
#'
#' #evaluating the model with spatial cross-validation
#' rf.model <- rf_evaluate(
#'   model = rf.model,
#'   xy = plants_df[, c("x", "y")],
#'   n.cores = 1,
#'   verbose = FALSE
#' )
#'
#' #getting evaluation results from the model
#' x <- get_evaluation(rf.model)
#' x
#'
#' @rdname get_evaluation
#' @export
get_evaluation <- function(model) {
  #stop if no evaluation slot
  if (!inherits(model, "rf_evaluate")) {
    stop("Object 'x' does not have an 'evaluation' slot.")
  }

  model$evaluation$aggregated
}
