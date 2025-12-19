#' @title Extract evaluation metrics from cross-validated model
#' @description Extracts aggregated performance metrics from a model evaluated with [rf_evaluate()].
#' @param model Model object with class `rf_evaluate` from [rf_evaluate()].
#' @return Data frame with aggregated evaluation metrics containing:
#' \itemize{
#'   \item `model`: Model type - "Full" (original model), "Training" (trained on training folds), or "Testing" (performance on testing folds, representing generalization ability).
#'   \item `metric`: Metric name - "rmse", "nrmse", "r.squared", or "pseudo.r.squared".
#'   \item `mean`, `sd`, `min`, `max`: Summary statistics across cross-validation repetitions.
#' }
#' @details
#' This function returns aggregated statistics across all cross-validation repetitions. The "Testing" model metrics indicate the model's ability to generalize to unseen spatial locations.
#' @seealso [rf_evaluate()], [plot_evaluation()], [print_evaluation()]
#' @examples
#' data(plants_rf, plants_xy)
#'
#' # Evaluate model with spatial cross-validation
#' m_evaluated <- rf_evaluate(
#'   model = plants_rf,
#'   xy = plants_xy,
#'   repetitions = 5,
#'   n.cores = 1
#' )
#'
#' # Extract evaluation metrics
#' eval_metrics <- get_evaluation(m_evaluated)
#' head(eval_metrics)
#'
#' # Compare with other evaluation functions
#' plot_evaluation(m_evaluated, notch = FALSE)
#' print_evaluation(m_evaluated)
#'
#' @rdname get_evaluation
#' @family model_info
#' @export
get_evaluation <- function(model) {
  #stop if no evaluation slot
  if (!inherits(model, "rf_evaluate")) {
    stop("Object 'x' does not have an 'evaluation' slot.")
  }

  model$evaluation$aggregated
}
