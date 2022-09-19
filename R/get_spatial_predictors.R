#' @title Gets the spatial predictors of a spatial model
#' @description Returns spatial predictors from a model fitted with [rf_spatial()] in order of importance.
#' @param model A model fitted with [rf_spatial()].
#' @return A data frame with the spatial predictors included in the model.
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
#'  #fitting spatial model
#'  rf.model <- rf_spatial(
#'    data = ecoregions_df,
#'    dependent.variable.name = ecoregions_dependent_variable_name,
#'    predictor.variable.names = ecoregions_predictor_variable_names,
#'    distance.matrix = ecoregions_distance_matrix,
#'    distance.thresholds = c(0, 1000),
#'    n.cores = 1,
#'    method = "mem.moran.sequential"
#'  )
#'
#' #getting data frame with the selected spatial predictors
#' spatial.predictors <- get_spatial_predictors(rf.model)
#' head(spatial.predictors)
#'
#' }
#' @rdname get_spatial_predictors
#' @export
get_spatial_predictors <- function(model){

  if(!inherits(model, "rf_spatial")){
    stop("This function only works on models fitted with 'rf_spatial'")
  }

  model$spatial$spatial.predictors

}
