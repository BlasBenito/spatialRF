#' @title Gets model residuals
#' @description Returns the residuals of models fitted with [rf()], [rf_repeat()], or [rf_spatial()].
#' @param model A model fitted with [rf()], [rf_repeat()], or [rf_spatial()].
#' @return A vector with model residuals, or the median of model residuals across repetitions if the model was fitted with [rf_repeat()].
#' @examples
#'
#' #load example data
#' data(plants_df)
#'
#' #fit random forest model
#' rf.model <- rf(
#'   data = plants_df,
#'   dependent.variable.name = plants_response,
#'   predictor.variable.names = plants_predictors,
#'   n.cores = 1,
#'   verbose = FALSE
#' )
#'
#' #getting vector with residuals
#' x <- get_residuals(rf.model)
#' x
#'
#' @rdname get_residuals
#' @export
get_residuals <- function(model) {
  model$residuals$values
}
