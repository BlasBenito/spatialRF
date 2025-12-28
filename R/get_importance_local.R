#' @title Extract local variable importance from model
#' @description Extracts local (case-specific) variable importance scores from models fitted with [rf()], [rf_repeat()], or [rf_spatial()].
#' @param model Model object from [rf()], [rf_repeat()], or [rf_spatial()].
#' @return Data frame with one row per observation and one column per predictor variable. Each cell contains the local importance score for that variable at that observation.
#' @details
#' Local importance measures how much each predictor contributes to predictions for individual observations, unlike global importance which summarizes contributions across all observations. This can reveal spatial or contextual patterns in variable influence.
#' @seealso [rf()], [rf_repeat()], [rf_spatial()], [get_importance()], [plot_importance()], [print_importance()]
#' @examples
#' data(plants_rf)
#'
#' # Extract local importance scores
#' local_imp <- get_importance_local(plants_rf)
#'
#' # View structure: rows = observations, columns = variables
#' dim(local_imp)
#' head(local_imp)
#'
#' # Find which variable is most important for first observation
#' colnames(local_imp)[which.max(local_imp[1, ])]
#'
#' @rdname get_importance_local
#' @family model_info
#' @export
#' @autoglobal
get_importance_local <- function(model) {
  model$importance$local
}
