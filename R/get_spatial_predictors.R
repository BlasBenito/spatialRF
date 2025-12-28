#' @title Extract spatial predictors from spatial model
#' @description Extracts the spatial predictors (Moran's Eigenvector Maps) used in a model fitted with [rf_spatial()].
#' @param model Model object from [rf_spatial()] (must have class `rf_spatial`).
#' @return Data frame containing the spatial predictor values for each observation, with predictors ordered by decreasing importance.
#' @details
#' Spatial predictors are Moran's Eigenvector Maps (MEMs) automatically generated and selected by [rf_spatial()] to capture spatial autocorrelation patterns in the data. This function extracts these predictors, which can be useful for understanding spatial structure or for making predictions on new spatial locations.
#' @seealso [rf_spatial()], [mem()], [mem_multithreshold()], [get_importance()]
#' @examples
#' data(plants_rf_spatial)
#'
#' # Extract spatial predictors
#' spatial_preds <- get_spatial_predictors(plants_rf_spatial)
#' head(spatial_preds)
#'
#' # Check dimensions
#' dim(spatial_preds)
#'
#' # View predictor names (ordered by importance)
#' colnames(spatial_preds)
#'
#' @rdname get_spatial_predictors
#' @family model_info
#' @export
#' @autoglobal
get_spatial_predictors <- function(model) {
  if (!inherits(model, "rf_spatial")) {
    stop("This function only works on models fitted with 'rf_spatial'")
  }

  model$spatial$spatial.predictors
}
