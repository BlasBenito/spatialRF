#' @title Extract fitted predictions from model
#' @description Extracts fitted (in-sample) predictions from models fitted with [rf()], [rf_repeat()], or [rf_spatial()].
#' @param model Model object from [rf()], [rf_repeat()], or [rf_spatial()].
#' @return Numeric vector of fitted predictions with length equal to the number of training observations. For [rf_repeat()] models, returns the median prediction across repetitions.
#' @details
#' This function returns fitted predictions for the training data used to build the model, not predictions for new data. For out-of-sample predictions on new data use [stats::predict()].
#' @seealso [rf()], [rf_repeat()], [rf_spatial()], [get_residuals()]
#' @examples
#' data(plants_rf)
#'
#' # Extract fitted predictions
#' predictions <- get_predictions(plants_rf)
#' head(predictions)
#'
#' # Check length matches number of observations
#' length(predictions)
#'
#' # Compare with observed values to assess fit
#' # (observed values would be in original data)
#'
#' @rdname get_predictions
#' @family model_info
#' @export
get_predictions <- function(model) {
  model$predictions$values
}
