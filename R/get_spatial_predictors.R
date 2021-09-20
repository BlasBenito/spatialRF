#' @title Gets the spatial predictors of a spatial model
#' @description Returns spatial predictors from a model fitted with [rf_spatial()] in order of importance.
#' @param model A model fitted with [rf_spatial()].
#' @return A data frame with the spatial predictors included in the model.
#' @examples
#' \donttest{
#' if(interactive()){
#'
#'  #loading example data
#'  data(distance_matrix)
#'  data(plant_richness_df)
#'
#'  #fittind spatial model
#'  model <- rf_spatial(
#'    data = plant_richness_df,
#'    dependent.variable.name = "richness_species_vascular",
#'    predictor.variable.names = colnames(plant_richness_df)[5:21],
#'    distance.matrix = distance_matrix,
#'    distance.thresholds = c(0, 500, 1000),
#'    method = "mem.moran.sequential"
#'  )
#'
#' #getting data frame with the selected spatial predictors
#' spatial.predictors <- get_spatial_predictors(model)
#' head(spatial.predictors)
#'
#' }
#' }
#' @rdname get_spatial_predictors
#' @export
get_spatial_predictors <- function(model){

  if(!inherits(model, "rf_spatial")){
    stop("This function only works on models fitted with 'rf_spatial'")
  }

  model$spatial$spatial.predictors

}
