#' @title Gets the spatial predictors of a spatial model
#' @description Returns spatial predictors from a model fitted with [rf_spatial()] in order of importance.
#' @param model A model fitted with [rf_spatial()].
#' @return A data frame with the spatial predictors included in the model.
#' @examples
#'
#'  #loading example data
#'  data(plants_distance)
#'  data(plants_df)
#'
#'  #fittind spatial model
#'  model <- rf_spatial(
#'    data = plants_df,
#'    dependent.variable.name = plants_response,
#'    predictor.variable.names = plants_predictors,
#'    distance.matrix = plants_distance,
#'    distance.thresholds = c(0, 1000),
#'    n.cores = 1,
#'    method = "mem.moran.sequential"
#'  )
#'
#' #getting data frame with the selected spatial predictors
#' spatial.predictors <- get_spatial_predictors(model)
#' head(spatial.predictors)
#'
#' @rdname get_spatial_predictors
#' @export
get_spatial_predictors <- function(model) {
  if (!inherits(model, "rf_spatial")) {
    stop("This function only works on models fitted with 'rf_spatial'")
  }

  model$spatial$spatial.predictors
}
