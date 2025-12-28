#' @title Extract variable importance from model
#' @description Extracts variable importance scores from models fitted with [rf()], [rf_repeat()], or [rf_spatial()].
#' @param model Model object from [rf()], [rf_repeat()], or [rf_spatial()].
#' @return Data frame with columns `variable` (character) and `importance` (numeric), sorted by decreasing importance.
#' @details
#' For spatial models ([rf_spatial()]) with many spatial predictors, this function returns aggregated importance statistics for spatial predictors to improve readability. Non-spatial models return per-variable importance scores directly.
#' @seealso [rf()], [rf_repeat()], [rf_spatial()], [plot_importance()], [print_importance()]
#' @examples
#' data(plants_rf)
#'
#' # Extract variable importance
#' importance <- get_importance(plants_rf)
#' head(importance)
#'
#' # View top 5 most important variables
#' importance[1:5, ]
#'
#' @rdname get_importance
#' @family model_info
#' @export
#' @autoglobal
get_importance <- function(model) {
  #importance from rf
  if (
    (inherits(model, "rf") && !inherits(model, "rf_spatial")) ||
      (inherits(model, "rf_repeat") && !inherits(model, "rf_spatial"))
  ) {
    x <- model$importance$per.variable
  }

  #importance from rf_repeat
  if (inherits(model, "rf_spatial")) {
    if (!is.null(model$ranger.arguments$repetitions)) {
      repetitions <- model$ranger.arguments$repetitions
    } else {
      repetitions <- 1
    }

    #count non-spatial predictors
    length.non.spatial.predictors <- sum(
      model$importance$spatial.predictors$variable != "spatial_predictors"
    ) /
      repetitions

    length.spatial.predictors <- sum(
      model$importance$spatial.predictors$variable == "spatial_predictors"
    ) /
      repetitions

    #get spatial.predictor.stats if too many spatial predictors
    if (length.spatial.predictors >= length.non.spatial.predictors) {
      x <- model$importance$spatial.predictor.stats
    } else {
      x <- model$importance$per.variable
    }
  }

  if (is.null(x)) {
    stop("This model doesn't have a 'variable.importance' slot")
  }

  #arranging
  x <- x[order(x$importance, decreasing = TRUE), ]

  #return importance
  x
}
